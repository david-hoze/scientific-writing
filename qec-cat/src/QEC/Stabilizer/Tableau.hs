-- | Aaronson-Gottesman CHP stabilizer tableau simulator for Clifford circuits.
--
-- A 'Tableau' of n qubits stores 2n generators (n destabilizers + n stabilizers)
-- in the binary symplectic representation with sign bits.
module QEC.Stabilizer.Tableau
  ( Tableau(..)
  , newTableau
  , tableauNumQubits
  , applyH, applyS, applyCNOT
  , measure
  ) where

import Control.DeepSeq (NFData(..))
import Data.Bits
import Data.Word (Word64)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Control.Monad.ST (runST)
import System.Random (RandomGen, uniformR)

import QEC.GF2

------------------------------------------------------------------------
-- Type
------------------------------------------------------------------------

-- | Stabilizer tableau for n qubits.
-- Stores 2n generators (rows 0..n-1 are destabilizers, n..2n-1 are stabilizers).
-- Each generator has x-bits, z-bits (as packed Word64), and a sign bit.
data Tableau = Tableau
  { tabN      :: {-# UNPACK #-} !Int          -- ^ Number of qubits
  , tabXBits  :: !(VU.Vector Word64)           -- ^ x-bits, flat 2n * wpr
  , tabZBits  :: !(VU.Vector Word64)           -- ^ z-bits, flat 2n * wpr
  , tabSigns  :: !(VU.Vector Bool)             -- ^ sign bits, length 2n
  , tabWpr    :: {-# UNPACK #-} !Int           -- ^ Words per row
  } deriving stock (Show)

instance NFData Tableau where
  rnf (Tableau n x z s w) = n `seq` rnf x `seq` rnf z `seq` rnf s `seq` w `seq` ()

-- | Number of qubits in the tableau.
tableauNumQubits :: Tableau -> Int
tableauNumQubits = tabN

-- | Create a new tableau initialized to |0...0>.
-- Destabilizers: X_i, Stabilizers: Z_i.
newTableau :: Int -> Tableau
newTableau n = Tableau n xbits zbits signs wpr
  where
    wpr = wordsNeeded n
    rows = 2 * n
    -- Destabilizers (rows 0..n-1): X_i has x-bit i set
    -- Stabilizers (rows n..2n-1): Z_i has z-bit i set
    xbits = VU.create $ do
      v <- VUM.replicate (rows * wpr) 0
      -- Set x-bits for destabilizers
      let setDestab i
            | i >= n    = return ()
            | otherwise = do
                let (wi, bi) = i `quotRem` 64
                VUM.modify v (`setBit` bi) (i * wpr + wi)
                setDestab (i + 1)
      setDestab 0
      return v
    zbits = VU.create $ do
      v <- VUM.replicate (rows * wpr) 0
      -- Set z-bits for stabilizers
      let setStab i
            | i >= n    = return ()
            | otherwise = do
                let (wi, bi) = i `quotRem` 64
                VUM.modify v (`setBit` bi) ((n + i) * wpr + wi)
                setStab (i + 1)
      setStab 0
      return v
    signs = VU.replicate rows False

------------------------------------------------------------------------
-- Helper: get/set bits
------------------------------------------------------------------------

_getXBit :: Tableau -> Int -> Int -> Bool
_getXBit tab row col =
  let (wi, bi) = col `quotRem` 64
  in testBit (tabXBits tab VU.! (row * tabWpr tab + wi)) bi

_getZBit :: Tableau -> Int -> Int -> Bool
_getZBit tab row col =
  let (wi, bi) = col `quotRem` 64
  in testBit (tabZBits tab VU.! (row * tabWpr tab + wi)) bi

------------------------------------------------------------------------
-- Gate operations
------------------------------------------------------------------------

-- | Apply Hadamard gate on qubit @a@.
-- Rules: swap x and z bits for column a, update sign.
-- sign ^= x_a AND z_a
applyH :: Int -> Tableau -> Tableau
applyH a tab = runST $ do
    let n = tabN tab
        wpr = tabWpr tab
        rows = 2 * n
    xm <- VU.thaw (tabXBits tab)
    zm <- VU.thaw (tabZBits tab)
    sm <- VU.thaw (tabSigns tab)
    let (wi, bi) = a `quotRem` 64
    let go r
          | r >= rows = return ()
          | otherwise = do
              let idx = r * wpr + wi
              xw <- VUM.read xm idx
              zw <- VUM.read zm idx
              let xa = testBit xw bi
                  za = testBit zw bi
              -- Update sign: s ^= x_a AND z_a
              when (xa && za) $ VUM.modify sm not r
              -- Swap x_a and z_a
              let xw' = if za then setBit xw bi else clearBit xw bi
                  zw' = if xa then setBit zw bi else clearBit zw bi
              VUM.write xm idx xw'
              VUM.write zm idx zw'
              go (r + 1)
    go 0
    xf <- VU.freeze xm
    zf <- VU.freeze zm
    sf <- VU.freeze sm
    return tab { tabXBits = xf, tabZBits = zf, tabSigns = sf }
  where
    when True act = act
    when False _ = return ()

-- | Apply S (phase) gate on qubit @a@.
-- Rules: sign ^= x_a AND z_a; z_a ^= x_a
applyS :: Int -> Tableau -> Tableau
applyS a tab = runST $ do
    let n = tabN tab
        wpr = tabWpr tab
        rows = 2 * n
    xm <- VU.thaw (tabXBits tab)
    zm <- VU.thaw (tabZBits tab)
    sm <- VU.thaw (tabSigns tab)
    let (wi, bi) = a `quotRem` 64
    let go r
          | r >= rows = return ()
          | otherwise = do
              let idx = r * wpr + wi
              xw <- VUM.read xm idx
              zw <- VUM.read zm idx
              let xa = testBit xw bi
                  za = testBit zw bi
              when (xa && za) $ VUM.modify sm not r
              -- z_a ^= x_a
              let zw' = if xa then complementBit zw bi else zw
              VUM.write zm idx zw'
              go (r + 1)
    go 0
    zf <- VU.freeze zm
    sf <- VU.freeze sm
    return tab { tabZBits = zf, tabSigns = sf }
  where
    when True act = act
    when False _ = return ()

-- | Apply CNOT gate with control @a@, target @b@.
-- Rules: sign ^= x_a AND z_b AND (x_b XOR z_a XOR 1);
--        x_b ^= x_a; z_a ^= z_b
applyCNOT :: Int -> Int -> Tableau -> Tableau
applyCNOT a b tab = runST $ do
    let n = tabN tab
        wpr = tabWpr tab
        rows = 2 * n
    xm <- VU.thaw (tabXBits tab)
    zm <- VU.thaw (tabZBits tab)
    sm <- VU.thaw (tabSigns tab)
    let (wia, bia) = a `quotRem` 64
        (wib, bib) = b `quotRem` 64
    let go r
          | r >= rows = return ()
          | otherwise = do
              xwa <- VUM.read xm (r * wpr + wia)
              zwa <- VUM.read zm (r * wpr + wia)
              xwb <- VUM.read xm (r * wpr + wib)
              zwb <- VUM.read zm (r * wpr + wib)
              let xa = testBit xwa bia
                  za = testBit zwa bia
                  xb = testBit xwb bib
                  zb = testBit zwb bib
              -- Sign update
              let signFlip = xa && zb && not (xb /= za)
              when signFlip $ VUM.modify sm not r
              -- x_b ^= x_a
              let xwb' = if xa then complementBit xwb bib else xwb
              VUM.write xm (r * wpr + wib) xwb'
              -- z_a ^= z_b
              let zwa' = if zb then complementBit zwa bia else zwa
              VUM.write zm (r * wpr + wia) zwa'
              go (r + 1)
    go 0
    xf <- VU.freeze xm
    zf <- VU.freeze zm
    sf <- VU.freeze sm
    return tab { tabXBits = xf, tabZBits = zf, tabSigns = sf }
  where
    when True act = act
    when False _ = return ()

------------------------------------------------------------------------
-- Measurement
------------------------------------------------------------------------

-- | Measure qubit @a@ in the computational basis.
-- Returns (result, updated_tableau, updated_rng).
-- Result is True for |1>, False for |0>.
measure :: RandomGen g => Int -> Tableau -> g -> (Bool, Tableau, g)
measure a tab gen =
  case findAnticommuting of
    Just p  -> randomOutcome p
    Nothing -> deterministicOutcome
  where
    n = tabN tab
    wpr = tabWpr tab
    (wi, bi) = a `quotRem` 64

    -- Find a stabilizer (row n..2n-1) that anticommutes with Z_a
    -- (i.e., has x_a = 1)
    findAnticommuting = go n
      where
        go r
          | r >= 2 * n = Nothing
          | testBit (tabXBits tab VU.! (r * wpr + wi)) bi = Just r
          | otherwise = go (r + 1)

    -- Random outcome: stabilizer p anticommutes with Z_a
    randomOutcome p = (outcome, tab'', gen')
      where
        (outcome, gen') = uniformR (False, True) gen
        -- Row-reduce: for all i /= p with x_a = 1, rowmul i p
        tab' = rowReduceExcept p tab
        -- Set row p-n (destabilizer) = old row p (stabilizer)
        -- Set row p = Z_a with sign = outcome
        tab'' = setMeasuredRow p outcome tab'

    -- Deterministic outcome: no stabilizer anticommutes with Z_a,
    -- so Z_a is in the stabilizer group. We express it as a product of
    -- generators: for each destabilizer i with x_{i,a}=1, include
    -- stabilizer (n+i). Multiply them with proper phase tracking.
    deterministicOutcome = (outcome, tab, gen)
      where
        outcome = runST $ do
          -- Scratch row accumulator (starts as identity = all zeros, sign False)
          sxm <- VUM.replicate wpr (0 :: Word64)
          szm <- VUM.replicate wpr (0 :: Word64)
          signRef <- VUM.replicate 1 False
          let gphs stabRow q !acc
                | q >= n = return acc
                | otherwise = do
                    let (wq, bq) = q `quotRem` 64
                    sx <- VUM.read sxm wq
                    sz <- VUM.read szm wq
                    let rx = tabXBits tab VU.! (stabRow * wpr + wq)
                        rz = tabZBits tab VU.! (stabRow * wpr + wq)
                        x1 = testBit sx bq
                        z1 = testBit sz bq
                        x2 = testBit rx bq
                        z2 = testBit rz bq
                        g = gFunc x1 z1 x2 z2
                    gphs stabRow (q + 1) (acc + g)
          let go i
                | i >= n = VUM.read signRef 0
                | not (testBit (tabXBits tab VU.! (i * wpr + wi)) bi) = go (i + 1)
                | otherwise = do
                    let stabRow = n + i
                    phase <- gphs stabRow 0 (0 :: Int)
                    curSign <- VUM.read signRef 0
                    let stabSign = tabSigns tab VU.! stabRow
                    VUM.write signRef 0 ((curSign /= stabSign) /= odd phase)
                    -- XOR stabilizer row into scratch
                    let xorW w
                          | w >= wpr = return ()
                          | otherwise = do
                              sx <- VUM.read sxm w
                              sz <- VUM.read szm w
                              let rx = tabXBits tab VU.! (stabRow * wpr + w)
                                  rz = tabZBits tab VU.! (stabRow * wpr + w)
                              VUM.write sxm w (xor sx rx)
                              VUM.write szm w (xor sz rz)
                              xorW (w + 1)
                    xorW 0
                    go (i + 1)
          go 0

    -- Row-reduce: XOR row p into all other rows that have x_a = 1
    rowReduceExcept p tab0 = runST $ do
      xm <- VU.thaw (tabXBits tab0)
      zm <- VU.thaw (tabZBits tab0)
      sm <- VU.thaw (tabSigns tab0)
      let go r
            | r >= 2 * n = return ()
            | r == p     = go (r + 1)
            | otherwise  = do
                xw <- VUM.read xm (r * wpr + wi)
                if testBit xw bi
                  then do
                    -- Rowmul r p: XOR row p into row r with phase tracking
                    rowMul xm zm sm r p
                    go (r + 1)
                  else go (r + 1)
      go 0
      xf <- VU.freeze xm
      zf <- VU.freeze zm
      sf <- VU.freeze sm
      return tab0 { tabXBits = xf, tabZBits = zf, tabSigns = sf }

    -- XOR row src into row dst, updating phase
    rowMul xm zm sm dst src = do
      -- Phase update: simplified
      -- Full phase tracking requires computing g(x1,z1,x2,z2)
      -- For now, track sign via the XOR of individual qubit phases
      dstSign <- VUM.read sm dst
      srcSign <- VUM.read sm src
      -- Compute phase from Pauli multiplication
      phase <- computePhase xm zm dst src
      let newSign = (dstSign /= srcSign) /= phase
      VUM.write sm dst newSign
      -- XOR the x and z bits
      let xorRow vec d s = do
            let goW w
                  | w >= wpr = return ()
                  | otherwise = do
                      dv <- VUM.read vec (d * wpr + w)
                      sv <- VUM.read vec (s * wpr + w)
                      VUM.write vec (d * wpr + w) (xor dv sv)
                      goW (w + 1)
            goW 0
      xorRow xm dst src
      xorRow zm dst src

    -- Compute extra phase from multiplying Pauli strings
    computePhase xm zm dst src = do
      let goQ q !acc
            | q >= n = return (odd acc)
            | otherwise = do
                let (wq, bq) = q `quotRem` 64
                xd <- VUM.read xm (dst * wpr + wq)
                zd <- VUM.read zm (dst * wpr + wq)
                xs <- VUM.read xm (src * wpr + wq)
                zs <- VUM.read zm (src * wpr + wq)
                let x1 = testBit xd bq
                    z1 = testBit zd bq
                    x2 = testBit xs bq
                    z2 = testBit zs bq
                    -- Phase from single-qubit Pauli multiplication
                    -- Using the Aaronson-Gottesman g function
                    g = gFunc x1 z1 x2 z2
                goQ (q + 1) (acc + g)
      goQ 0 (0 :: Int)

    gFunc x1 z1 x2 z2
      | not x1 && not z1 = 0  -- I * anything
      | x1 && z1         = z2i - x2i  -- Y * ...
      | x1               = z2i * (2 * x2i - 1)  -- X * ...
      | otherwise         = x2i * (1 - 2 * z2i)  -- Z * ...
      where
        x2i = if x2 then 1 else 0 :: Int
        z2i = if z2 then 1 else 0 :: Int

    -- Set the measured row
    setMeasuredRow p outcome tab0 = runST $ do
      xm <- VU.thaw (tabXBits tab0)
      zm <- VU.thaw (tabZBits tab0)
      sm <- VU.thaw (tabSigns tab0)
      -- Copy row p to row p-n (destabilizer gets old stabilizer)
      let destRow = p - n
      let copyRow d s vec = do
            let goW w
                  | w >= wpr = return ()
                  | otherwise = do
                      v <- VUM.read vec (s * wpr + w)
                      VUM.write vec (d * wpr + w) v
                      goW (w + 1)
            goW 0
      copyRow destRow p xm
      copyRow destRow p zm
      ds <- VUM.read sm p
      VUM.write sm destRow ds
      -- Clear row p
      let clearRow r vec = do
            let goW w
                  | w >= wpr = return ()
                  | otherwise = do
                      VUM.write vec (r * wpr + w) 0
                      goW (w + 1)
            goW 0
      clearRow p xm
      clearRow p zm
      -- Set row p = Z_a with appropriate sign
      VUM.write zm (p * wpr + wi) (setBit 0 bi)
      VUM.write sm p outcome
      xf <- VU.freeze xm
      zf <- VU.freeze zm
      sf <- VU.freeze sm
      return tab0 { tabXBits = xf, tabZBits = zf, tabSigns = sf }
