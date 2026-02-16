-- | Monte Carlo simulation for code-capacity noise.
--
-- Samples random errors, computes syndromes, decodes with BP+OSD,
-- and checks for logical errors.
module QEC.Simulation
  ( SimConfig(..)
  , SimResult(..)
  , defaultSimConfig
  , runSimulation
  , logicalErrorRate
  ) where

import Control.DeepSeq (NFData(..), force)
import Data.Word (Word64)
import Control.Parallel.Strategies (parMap, rdeepseq)
import qualified Data.Vector.Unboxed as VU
import System.Random.SplitMix (SMGen, mkSMGen, nextWord64, splitSMGen)

import QEC.GF2
import QEC.GF2.Matrix
import QEC.GF2.Gauss (rref)
import QEC.Code.CSS
import QEC.Decoder.BP
import QEC.Decoder.OSD

------------------------------------------------------------------------
-- Configuration
------------------------------------------------------------------------

data SimConfig = SimConfig
  { simNumTrials  :: {-# UNPACK #-} !Int     -- ^ Number of Monte Carlo trials
  , simBPConfig   :: !BPConfig                -- ^ BP decoder configuration
  , simNumChunks  :: {-# UNPACK #-} !Int      -- ^ Number of parallel chunks
  } deriving stock (Show)

instance NFData SimConfig where
  rnf (SimConfig n bp c) = n `seq` bp `seq` c `seq` ()

defaultSimConfig :: SimConfig
defaultSimConfig = SimConfig
  { simNumTrials = 10000
  , simBPConfig  = defaultBPConfig
  , simNumChunks = 4
  }

------------------------------------------------------------------------
-- Result
------------------------------------------------------------------------

data SimResult = SimResult
  { simTotalTrials   :: {-# UNPACK #-} !Int
  , simLogicalErrors :: {-# UNPACK #-} !Int
  } deriving stock (Show)

instance NFData SimResult where
  rnf (SimResult t e) = t `seq` e `seq` ()

-- | Logical error rate = logical errors / total trials.
logicalErrorRate :: SimResult -> Double
logicalErrorRate r = fromIntegral (simLogicalErrors r) / fromIntegral (simTotalTrials r)

------------------------------------------------------------------------
-- Rowspace membership via RREF
------------------------------------------------------------------------

-- | Precomputed RREF data for fast rowspace membership testing.
data RowspaceChecker = RowspaceChecker
  { rcRrefMatrix :: !BinMatrix   -- ^ RREF of the matrix
  , rcPivots     :: ![Int]       -- ^ Pivot column indices
  }

-- | Build a rowspace checker from a matrix.
mkRowspaceChecker :: BinMatrix -> RowspaceChecker
mkRowspaceChecker m =
  let (rrefM, pivots) = rref m
  in RowspaceChecker rrefM pivots

-- | Check if a vector is in the rowspace of the matrix.
-- Reduces v by the RREF pivot rows; if result is zero, v is in rowspace.
inRowspace :: RowspaceChecker -> BinVec -> Bool
inRowspace rc v = bvWeight reduced == 0
  where
    reduced = foldl reduceByPivot v (zip (rcPivots rc) [0..])
    reduceByPivot acc (col, row) =
      if bvGetBit acc col == GF2 True
      then bvXor acc (bmGetRow (rcRrefMatrix rc) row)
      else acc

------------------------------------------------------------------------
-- Simulation
------------------------------------------------------------------------

-- | Run a Monte Carlo simulation for Z-type errors on a CSS code.
-- Uses code-capacity noise model: each qubit independently flips with
-- probability pZ. Syndrome is computed from H_Z, decoded with BP+OSD.
-- Logical error: residual = error XOR correction is in ker(H_Z) but
-- not in rowspace(H_Z), meaning it's a non-trivial logical operator.
runSimulation :: SimConfig -> CSSCode -> Double -> Word64 -> SimResult
runSimulation config code pZ seed =
  let chunks = splitWork (simNumTrials config) (simNumChunks config)
      seeds  = genSeeds seed (simNumChunks config)
      hz = cssHZ code
      n = cssNumQubits code
      -- Precompute rowspace checker for H_Z
      checker = mkRowspaceChecker hz
      channelLLR = VU.replicate n (log ((1.0 - pZ) / pZ))
      results = parMap rdeepseq
        (\(nTrials, s) -> force $ runChunk config hz checker channelLLR pZ nTrials s)
        (zip chunks seeds)
      totalErrors = sum (map simLogicalErrors results)
  in SimResult (simNumTrials config) totalErrors

-- | Run a chunk of trials.
runChunk :: SimConfig -> BinMatrix -> RowspaceChecker -> VU.Vector Double
         -> Double -> Int -> SMGen -> SimResult
runChunk config hz checker channelLLR pZ nTrials gen0 =
  go 0 0 gen0
  where
    n = bmNumCols hz
    go !errors !done !gen
      | done >= nTrials = SimResult nTrials errors
      | otherwise =
          let (err, gen1) = sampleError n pZ gen
              syn = bmMulVec hz err
              isZeroSyn = bvWeight syn == 0
              correction = if isZeroSyn
                then bvZero n
                else bpCorrection (bpOsdDecode (simBPConfig config) hz syn channelLLR)
              residual = bvXor err correction
              -- Logical error: residual is non-zero and NOT in rowspace(H_Z)
              isLogicalError = bvWeight residual > 0
                            && not (inRowspace checker residual)
          in go (if isLogicalError then errors + 1 else errors) (done + 1) gen1

-- | Sample a random error: each bit flips with probability p.
sampleError :: Int -> Double -> SMGen -> (BinVec, SMGen)
sampleError n p gen0 =
  let threshold = round (p * fromIntegral (maxBound :: Word64)) :: Word64
      go !i !gen !acc
        | i >= n = (bvFromList (reverse acc), gen)
        | otherwise =
            let (w, gen') = nextWord64 gen
                bit = GF2 (w < threshold)
            in go (i + 1) gen' (bit : acc)
  in go 0 gen0 []

-- | Split work into chunks.
splitWork :: Int -> Int -> [Int]
splitWork total nChunks =
  let base = total `div` nChunks
      extra = total `mod` nChunks
  in replicate extra (base + 1) ++ replicate (nChunks - extra) base

-- | Generate independent seeds by splitting.
genSeeds :: Word64 -> Int -> [SMGen]
genSeeds seed nChunks = go (mkSMGen seed) nChunks []
  where
    go _ 0 acc = reverse acc
    go g k acc = let (g1, g2) = splitSMGen g
                 in go g2 (k - 1) (g1 : acc)
