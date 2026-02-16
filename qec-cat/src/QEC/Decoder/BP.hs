-- | Belief propagation decoder over GF(2).
--
-- Implements min-sum and product-sum variants of the BP algorithm
-- on the Tanner graph defined by a parity check matrix H.
module QEC.Decoder.BP
  ( BPConfig(..)
  , BPMethod(..)
  , BPResult(..)
  , defaultBPConfig
  , bpDecode
  ) where

import Control.DeepSeq (NFData(..))
import Data.Bits
import Data.Word (Word64)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Control.Monad (forM_, when)
import Control.Monad.ST (runST)

import QEC.GF2
import QEC.GF2.Matrix

------------------------------------------------------------------------
-- Configuration
------------------------------------------------------------------------

-- | BP decoding method.
data BPMethod = MinSum | ProductSum
  deriving stock (Show, Eq)

-- | BP decoder configuration.
data BPConfig = BPConfig
  { bpMaxIter   :: {-# UNPACK #-} !Int     -- ^ Maximum iterations
  , bpMethod    :: !BPMethod               -- ^ Min-sum or product-sum
  , bpMsAlpha   :: {-# UNPACK #-} !Double  -- ^ Min-sum scaling factor
  } deriving stock (Show, Eq)

-- | Default BP configuration: min-sum with alpha=0.625, 100 iterations.
defaultBPConfig :: BPConfig
defaultBPConfig = BPConfig
  { bpMaxIter = 100
  , bpMethod  = MinSum
  , bpMsAlpha = 0.625
  }

-- | Result of BP decoding.
data BPResult = BPResult
  { bpCorrection :: !BinVec     -- ^ Estimated error pattern
  , bpConverged  :: !Bool       -- ^ Did BP converge?
  , bpIterations :: {-# UNPACK #-} !Int  -- ^ Iterations used
  , bpSoftOutput :: !(VU.Vector Double)  -- ^ Final LLR values
  } deriving stock (Show)

instance NFData BPResult where
  rnf (BPResult c cv it so) = rnf c `seq` cv `seq` it `seq` rnf so

------------------------------------------------------------------------
-- Tanner graph
------------------------------------------------------------------------

-- | Adjacency: for each check, list of variable indices;
-- for each variable, list of check indices.
data TannerGraph = TannerGraph
  { tgCheckToVar :: !(VU.Vector Int)  -- flat adjacency, check -> vars
  , tgCheckStart :: !(VU.Vector Int)  -- start index per check
  , tgVarToCheck :: !(VU.Vector Int)  -- flat adjacency, var -> checks
  , tgVarStart   :: !(VU.Vector Int)  -- start index per variable
  , tgNumChecks  :: {-# UNPACK #-} !Int
  , tgNumVars    :: {-# UNPACK #-} !Int
  }

buildTannerGraph :: BinMatrix -> TannerGraph
buildTannerGraph h = TannerGraph
    { tgCheckToVar = VU.fromList c2vFlat
    , tgCheckStart = VU.fromList c2vStarts
    , tgVarToCheck = VU.fromList v2cFlat
    , tgVarStart   = VU.fromList v2cStarts
    , tgNumChecks  = m
    , tgNumVars    = n
    }
  where
    m = bmNumRows h
    n = bmNumCols h
    wpr = bmWordsPerRow h
    dat = bmData h

    -- Check-to-variable adjacency
    c2v = [ [ j | j <- [0..n-1], testBit (dat VU.! (i * wpr + j `div` 64)) (j `mod` 64) ]
          | i <- [0..m-1] ]
    c2vFlat = concat c2v
    c2vStarts = scanl (+) 0 (map length c2v)

    -- Variable-to-check adjacency
    v2c = [ [ i | i <- [0..m-1], testBit (dat VU.! (i * wpr + j `div` 64)) (j `mod` 64) ]
          | j <- [0..n-1] ]
    v2cFlat = concat v2c
    v2cStarts = scanl (+) 0 (map length v2c)

------------------------------------------------------------------------
-- BP decode
------------------------------------------------------------------------

-- | Decode a syndrome using belief propagation.
--
-- @bpDecode config h syndrome channelLLR@ runs BP on the Tanner graph
-- of @h@ with the given syndrome and channel log-likelihood ratios.
--
-- Channel LLR: @llr_j = log(p(no error on j) / p(error on j))@.
-- Positive LLR means "probably no error".
bpDecode :: BPConfig -> BinMatrix -> BinVec -> VU.Vector Double -> BPResult
bpDecode config h syndrome channelLLR = runST $ do
    let tg = buildTannerGraph h
        m  = tgNumChecks tg
        n  = tgNumVars tg

    -- Total number of edges in Tanner graph
    let numEdges = VU.length (tgCheckToVar tg)

    -- Messages: check-to-variable (c2v) and variable-to-check (v2c)
    -- Indexed by edge index in c2v ordering
    mc2v <- VUM.replicate numEdges (0.0 :: Double)
    mv2c <- VUM.replicate numEdges (0.0 :: Double)

    -- Initialize v2c messages to channel LLR
    forM_ [0 .. m - 1] $ \i -> do
      let start = tgCheckStart tg VU.! i
          end   = tgCheckStart tg VU.! (i + 1)
      forM_ [start .. end - 1] $ \e -> do
        let j = tgCheckToVar tg VU.! e
        VUM.write mv2c e (channelLLR VU.! j)

    -- Build edge index lookup: for variable j and check i,
    -- find the edge index in v2c ordering
    -- v2cEdgeIdx[flat index in v2c] = corresponding edge in c2v
    -- We need a mapping from (check i, var j) -> edge in c2v
    -- This is complex; instead we'll work with the c2v ordering throughout

    let synBits = [ unGF2 (bvGetBit syndrome i) | i <- [0..m-1] ]

    -- Main iteration loop
    let iterate_ !iter = do
          if iter >= bpMaxIter config
            then return (False, iter)
            else do
              -- Check-to-variable update
              forM_ [0 .. m - 1] $ \i -> do
                let start = tgCheckStart tg VU.! i
                    end   = tgCheckStart tg VU.! (i + 1)
                    si    = synBits !! i

                case bpMethod config of
                  MinSum -> do
                    -- Read all incoming v2c messages for this check
                    let numNeighbors = end - start
                    -- Compute sign and min magnitude
                    forM_ [start .. end - 1] $ \e -> do
                      -- Product of signs of all OTHER incoming messages
                      -- times syndrome sign; min of all OTHER magnitudes
                      let computeMsg = do
                            go (1.0 :: Double) (1e30 :: Double) start
                              where
                                go !sgn !mab !idx
                                  | idx >= end = do
                                      let synSign = if si then -1.0 else 1.0
                                          result = synSign * sgn * bpMsAlpha config * mab
                                      VUM.write mc2v e (clamp result)
                                  | idx == e   = go sgn mab (idx + 1)
                                  | otherwise  = do
                                      val <- VUM.read mv2c idx
                                      let s = if val < 0 then -1.0 else 1.0
                                          a = abs val
                                          mab' = if a < mab then a else mab
                                      go (sgn * s) mab' (idx + 1)
                      computeMsg

                  ProductSum -> do
                    forM_ [start .. end - 1] $ \e -> do
                      -- Product of tanh(v2c/2) for all other edges
                      let go !prod !idx
                            | idx >= end = do
                                let synSign = if si then -1.0 else 1.0
                                    result = synSign * 2.0 * atanh (clampTanh prod)
                                VUM.write mc2v e (clamp result)
                            | idx == e   = go prod (idx + 1)
                            | otherwise  = do
                                val <- VUM.read mv2c idx
                                let t = tanh (val / 2.0)
                                go (prod * t) (idx + 1)
                      go 1.0 start

              -- Variable-to-check update + hard decision
              -- First compute total LLR per variable
              totalLLR <- VUM.replicate n (0.0 :: Double)
              forM_ [0 .. n - 1] $ \j ->
                VUM.write totalLLR j (channelLLR VU.! j)

              -- Add all incoming c2v messages
              forM_ [0 .. n - 1] $ \j -> do
                let vstart = tgVarStart tg VU.! j
                    vend   = tgVarStart tg VU.! (j + 1)
                forM_ [vstart .. vend - 1] $ \ve -> do
                  let ci = tgVarToCheck tg VU.! ve
                  -- Find the c2v edge for (ci, j)
                  let cstart = tgCheckStart tg VU.! ci
                      cend   = tgCheckStart tg VU.! (ci + 1)
                      findEdge idx
                        | idx >= cend = idx  -- shouldn't happen
                        | tgCheckToVar tg VU.! idx == j = idx
                        | otherwise = findEdge (idx + 1)
                      cedge = findEdge cstart
                  val <- VUM.read mc2v cedge
                  old <- VUM.read totalLLR j
                  VUM.write totalLLR j (old + val)

              -- Hard decision
              hardBits <- VUM.replicate (wordsNeeded n) (0 :: Word64)
              forM_ [0 .. n - 1] $ \j -> do
                llr <- VUM.read totalLLR j
                when (llr < 0) $ do
                  let (wi, bi) = j `quotRem` 64
                  old <- VUM.read hardBits wi
                  VUM.write hardBits wi (setBit old bi)

              -- Check syndrome
              hardVec <- BinVec n <$> VU.freeze hardBits
              let computedSyn = bmMulVec h hardVec
                  converged = computedSyn == syndrome

              if converged
                then do
                  tllr <- VU.freeze totalLLR
                  return (True, iter + 1)
                else do
                  -- Update v2c messages: total LLR minus incoming c2v
                  forM_ [0 .. m - 1] $ \i -> do
                    let cstart = tgCheckStart tg VU.! i
                        cend   = tgCheckStart tg VU.! (i + 1)
                    forM_ [cstart .. cend - 1] $ \e -> do
                      let j = tgCheckToVar tg VU.! e
                      tl <- VUM.read totalLLR j
                      c2vVal <- VUM.read mc2v e
                      VUM.write mv2c e (clamp (tl - c2vVal))

                  iterate_ (iter + 1)

    (conv, iters) <- iterate_ 0

    -- Final hard decision
    finalLLR <- VUM.replicate n (0.0 :: Double)
    forM_ [0 .. n - 1] $ \j ->
      VUM.write finalLLR j (channelLLR VU.! j)
    forM_ [0 .. n - 1] $ \j -> do
      let vstart = tgVarStart tg VU.! j
          vend   = tgVarStart tg VU.! (j + 1)
      forM_ [vstart .. vend - 1] $ \ve -> do
        let ci = tgVarToCheck tg VU.! ve
            cstart = tgCheckStart tg VU.! ci
            cend   = tgCheckStart tg VU.! (ci + 1)
            findEdge idx
              | idx >= cend = idx
              | tgCheckToVar tg VU.! idx == j = idx
              | otherwise = findEdge (idx + 1)
            cedge = findEdge cstart
        val <- VUM.read mc2v cedge
        old <- VUM.read finalLLR j
        VUM.write finalLLR j (old + val)

    hardBitsFinal <- VUM.replicate (wordsNeeded n) (0 :: Word64)
    forM_ [0 .. n - 1] $ \j -> do
      llr <- VUM.read finalLLR j
      when (llr < 0) $ do
        let (wi, bi) = j `quotRem` 64
        old <- VUM.read hardBitsFinal wi
        VUM.write hardBitsFinal wi (setBit old bi)

    correction <- BinVec n <$> VU.freeze hardBitsFinal
    softOut <- VU.freeze finalLLR

    return $ BPResult correction conv iters softOut

-- | Clamp a value to avoid infinities in LLR.
clamp :: Double -> Double
clamp x
  | x > 50    = 50
  | x < -50   = -50
  | otherwise  = x

-- | Clamp tanh product to avoid atanh(1) = infinity.
clampTanh :: Double -> Double
clampTanh x
  | x > 0.9999999 = 0.9999999
  | x < -0.9999999 = -0.9999999
  | otherwise = x
