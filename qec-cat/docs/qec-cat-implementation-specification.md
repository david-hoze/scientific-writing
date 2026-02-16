# `qec-cat` Implementation Specification

**Purpose**: This document is the complete specification for implementing a Haskell library for quantum error correction resource estimation targeting biased-noise cat qubit architectures. An AI coding agent should be able to implement the library from this document alone.

**What this library does**: Takes cat qubit physical parameters + a quantum error correcting code family + a target algorithm → outputs total physical qubit count and computation time. It does this via Monte Carlo simulation of stabilizer circuits with biased noise, decoding via belief propagation + ordered statistics decoding, and a resource estimation pipeline.

**Language**: Haskell (GHC ≥ 9.8). No external C dependencies in the initial version. All pure Haskell.

---

## 1. Project Structure

```
qec-cat/
├── qec-cat.cabal
├── flake.nix                          -- optional, for Nix users
├── src/
│   └── QEC/
│       ├── GF2.hs                     -- PHASE 1: GF(2) element, bit-packed vectors
│       ├── GF2/
│       │   ├── Matrix.hs              -- PHASE 1: Dense & sparse binary matrices
│       │   └── Gauss.hs              -- PHASE 1: Gaussian elimination, rank, kernel
│       ├── Symplectic.hs              -- PHASE 1: Symplectic form, commutativity
│       ├── Code.hs                    -- PHASE 2: QuantumCode typeclass hierarchy
│       ├── Code/
│       │   ├── CSS.hs                 -- PHASE 2: CSS code with orthogonality check
│       │   ├── Repetition.hs          -- PHASE 2: Repetition code
│       │   ├── Surface.hs             -- PHASE 2: Rotated surface code
│       │   ├── XZZX.hs               -- PHASE 3: XZZX surface code
│       │   ├── BivariateBicycle.hs    -- PHASE 3: BB codes (Bravyi et al.)
│       │   ├── LDPCCat.hs            -- PHASE 3: LDPC-cat codes (Ruiz et al.)
│       │   └── LiftedProduct.hs       -- PHASE 4: Bias-tailored lifted product
│       ├── Noise.hs                   -- PHASE 2: Noise model typeclass
│       ├── Noise/
│       │   ├── Depolarizing.hs        -- PHASE 2: Standard depolarizing noise
│       │   ├── Biased.hs             -- PHASE 2: Asymmetric Pauli channel
│       │   └── CatQubit.hs           -- PHASE 3: Full cat qubit physics model
│       ├── Stabilizer/
│       │   ├── Tableau.hs             -- PHASE 2: Aaronson-Gottesman CHP
│       │   └── PauliFrame.hs         -- PHASE 3: Pauli frame simulation
│       ├── Decoder.hs                 -- PHASE 2: Decoder typeclass
│       ├── Decoder/
│       │   ├── BP.hs                  -- PHASE 2: Belief propagation
│       │   ├── OSD.hs                -- PHASE 2: Ordered statistics decoding
│       │   └── MWPM.hs              -- PHASE 4: Min-weight perfect matching
│       ├── Simulation.hs              -- PHASE 3: Monte Carlo sampling engine
│       ├── Resource.hs                -- PHASE 4: Resource estimation pipeline
│       ├── Resource/
│       │   ├── Algorithm.hs           -- PHASE 4: Algorithm specifications
│       │   ├── MagicState.hs         -- PHASE 4: Distillation factory models
│       │   └── Layout.hs            -- PHASE 4: Routing overhead models
│       └── Export.hs                  -- PHASE 4: JSON/CSV output
├── test/
│   ├── Main.hs                        -- tasty test runner
│   ├── QEC/GF2/Test.hs
│   ├── QEC/Code/Test.hs
│   ├── QEC/Decoder/Test.hs
│   └── QEC/Stabilizer/Test.hs
├── bench/
│   └── Main.hs                        -- tasty-bench benchmarks
└── examples/
    ├── ThresholdPlot.hs               -- Estimate threshold for a code family
    └── ResourceEstimate.hs            -- Full RSA-2048 / ECDLP-256 estimate
```

---

## 2. Cabal File

```cabal
cabal-version: 3.0
name:          qec-cat
version:       0.1.0.0
synopsis:      Quantum error correction resource estimation for biased-noise cat qubit architectures
license:       BSD-3-Clause
build-type:    Simple

library
  exposed-modules:
    QEC.GF2
    QEC.GF2.Matrix
    QEC.GF2.Gauss
    QEC.Symplectic
    QEC.Code
    QEC.Code.CSS
    QEC.Code.Repetition
    QEC.Code.Surface
    QEC.Code.XZZX
    QEC.Code.BivariateBicycle
    QEC.Code.LDPCCat
    QEC.Code.LiftedProduct
    QEC.Noise
    QEC.Noise.Depolarizing
    QEC.Noise.Biased
    QEC.Noise.CatQubit
    QEC.Stabilizer.Tableau
    QEC.Stabilizer.PauliFrame
    QEC.Decoder
    QEC.Decoder.BP
    QEC.Decoder.OSD
    QEC.Simulation
    QEC.Resource
    QEC.Resource.Algorithm
    QEC.Resource.MagicState
    QEC.Resource.Layout
    QEC.Export
  build-depends:
    base          >= 4.18 && < 5,
    vector        >= 0.13,
    primitive     >= 0.8,
    deepseq       >= 1.4,
    containers    >= 0.6,
    mtl           >= 2.3,
    random        >= 1.2,
    splitmix      >= 0.1,
    parallel      >= 3.2,
    aeson         >= 2.1,
    bytestring    >= 0.11,
    cassava       >= 0.5,
    text          >= 2.0
  default-language: GHC2021
  default-extensions:
    DataKinds
    TypeFamilies
    GADTs
    StrictData
    DerivingStrategies
    GeneralizedNewtypeDeriving
    ScopedTypeVariables
    TypeApplications
    BangPatterns
    OverloadedStrings
    MultiParamTypeClasses
    FlexibleContexts
    FlexibleInstances
    KindSignatures
    RankNTypes
  ghc-options: -O2 -Wall -Wno-orphans

test-suite tests
  type: exitcode-stdio-1.0
  main-is: Main.hs
  hs-source-dirs: test
  build-depends:
    base,
    qec-cat,
    tasty          >= 1.4,
    tasty-quickcheck >= 0.10,
    tasty-hunit    >= 0.10,
    QuickCheck     >= 2.14,
    vector
  default-language: GHC2021

benchmark bench
  type: exitcode-stdio-1.0
  main-is: Main.hs
  hs-source-dirs: bench
  build-depends:
    base,
    qec-cat,
    tasty-bench    >= 0.3,
    vector,
    deepseq
  default-language: GHC2021
```

---

## 3. Build Order (4 Phases)

**CRITICAL**: Implement in this order. Each phase produces testable output. Do not skip ahead.

### PHASE 1: GF(2) Linear Algebra (Weeks 1–2)

Modules: `QEC.GF2`, `QEC.GF2.Matrix`, `QEC.GF2.Gauss`, `QEC.Symplectic` Test: All algebraic properties. No quantum code needed yet.

### PHASE 2: Codes + Decoders + Tableau (Weeks 3–6)

Modules: `QEC.Code`, `QEC.Code.CSS`, `QEC.Code.Repetition`, `QEC.Code.Surface`, `QEC.Noise`, `QEC.Noise.Depolarizing`, `QEC.Noise.Biased`, `QEC.Stabilizer.Tableau`, `QEC.Decoder`, `QEC.Decoder.BP`, `QEC.Decoder.OSD` Test: Reproduce surface code threshold (~10.3% code capacity). Decode repetition code syndromes.

### PHASE 3: Cat Qubit Physics + Monte Carlo + Advanced Codes (Weeks 7–10)

Modules: `QEC.Noise.CatQubit`, `QEC.Stabilizer.PauliFrame`, `QEC.Code.XZZX`, `QEC.Code.BivariateBicycle`, `QEC.Code.LDPCCat`, `QEC.Simulation` Test: Reproduce Ruiz et al. LDPC-cat logical error rates. Validate cat qubit noise model against published parameters.

### PHASE 4: Resource Estimation + Output (Weeks 11–14)

Modules: `QEC.Resource`, `QEC.Resource.Algorithm`, `QEC.Resource.MagicState`, `QEC.Resource.Layout`, `QEC.Code.LiftedProduct`, `QEC.Export` Test: Reproduce Gouzien et al. 126,133 cat qubits for ECDLP-256.

---

## 4. Module Specifications

### 4.1 `QEC.GF2` — Bit-packed GF(2) primitives

This is the foundation. Everything else depends on it.

```haskell
module QEC.GF2
  ( GF2(..)
  , BinVec(..)
  , bvZero, bvFromList, bvToList
  , bvXor, bvAnd, bvWeight, bvInnerGF2
  , bvGetBit, bvSetBit, bvClearBit, bvFlipBit
  , bvLength, wordsNeeded
  ) where

import Data.Bits
import Data.Word
import qualified Data.Vector.Unboxed as VU

-- | GF(2) element. Thin wrapper for clarity at API boundary.
newtype GF2 = GF2 { unGF2 :: Bool }
  deriving newtype (Eq, Ord, Show, NFData)

instance Num GF2 where
  GF2 a + GF2 b = GF2 (a /= b)   -- XOR
  GF2 a * GF2 b = GF2 (a && b)   -- AND
  abs = id
  signum = id
  fromInteger n = GF2 (odd n)
  negate = id                      -- In GF(2), -x = x

-- | Bit-packed binary vector. Length n stored at runtime.
-- Bits packed into Word64s, LSB-first within each word.
-- Unused high bits in the last word MUST be zero (invariant).
data BinVec = BinVec
  { bvLen   :: {-# UNPACK #-} !Int
  , bvWords :: !(VU.Vector Word64)
  } deriving (Show)

-- IMPORTANT: Eq must ignore padding bits
instance Eq BinVec where
  a == b = bvLen a == bvLen b && bvWords a == bvWords b

-- | Number of Word64s needed to store n bits
wordsNeeded :: Int -> Int
wordsNeeded n = (n + 63) `unsafeShiftR` 6  -- i.e., (n+63) `div` 64

-- | Zero vector of length n
bvZero :: Int -> BinVec
bvZero n = BinVec n (VU.replicate (wordsNeeded n) 0)

-- | XOR of two vectors (pointwise addition in GF(2))
bvXor :: BinVec -> BinVec -> BinVec
bvXor (BinVec n1 w1) (BinVec n2 w2)
  | n1 /= n2 = error "bvXor: length mismatch"
  | otherwise = BinVec n1 (VU.zipWith xor w1 w2)

-- | AND of two vectors (pointwise multiplication in GF(2))
bvAnd :: BinVec -> BinVec -> BinVec
bvAnd (BinVec n1 w1) (BinVec n2 w2)
  | n1 /= n2 = error "bvAnd: length mismatch"
  | otherwise = BinVec n1 (VU.zipWith (.&.) w1 w2)

-- | Hamming weight (number of 1 bits)
bvWeight :: BinVec -> Int
bvWeight (BinVec _ ws) = VU.foldl' (\acc w -> acc + popCount w) 0 ws

-- | Inner product over GF(2): dot(a,b) = popCount(a AND b) mod 2
bvInnerGF2 :: BinVec -> BinVec -> GF2
bvInnerGF2 a b = GF2 (odd (bvWeight (bvAnd a b)))

-- | Get bit at position i (0-indexed)
bvGetBit :: BinVec -> Int -> GF2
bvGetBit (BinVec _ ws) i =
  let (wordIdx, bitIdx) = i `quotRem` 64
  in GF2 (testBit (ws VU.! wordIdx) bitIdx)

-- | Set bit at position i to 1
bvSetBit :: Int -> BinVec -> BinVec
bvSetBit i (BinVec n ws) =
  let (wordIdx, bitIdx) = i `quotRem` 64
  in BinVec n (ws VU.// [(wordIdx, setBit (ws VU.! wordIdx) bitIdx)])

-- | Flip bit at position i
bvFlipBit :: Int -> BinVec -> BinVec
bvFlipBit i (BinVec n ws) =
  let (wordIdx, bitIdx) = i `quotRem` 64
  in BinVec n (ws VU.// [(wordIdx, complementBit (ws VU.! wordIdx) bitIdx)])

-- | Create from list of GF2 values
bvFromList :: [GF2] -> BinVec
-- Pack bits into Word64s. Implementation left to agent.

-- | Convert to list
bvToList :: BinVec -> [GF2]
-- Unpack. Implementation left to agent.

-- | Mask off unused high bits in the last word (call after any operation
-- that might set padding bits). MUST maintain invariant.
bvCleanup :: BinVec -> BinVec
bvCleanup (BinVec n ws)
  | r == 0    = BinVec n ws
  | otherwise = BinVec n (ws VU.// [(lastIdx, ws VU.! lastIdx .&. mask)])
  where
    r = n `rem` 64
    mask = (1 `unsafeShiftL` r) - 1
    lastIdx = VU.length ws - 1
```

**Tests for QEC.GF2**:

- `bvXor a a == bvZero n` for all a
- `bvXor a (bvZero n) == a`
- `bvXor` is commutative and associative
- `bvWeight (bvZero n) == 0`
- `bvInnerGF2 a a == GF2 (odd (bvWeight a))`
- Roundtrip: `bvToList (bvFromList xs) == xs`

---

### 4.2 `QEC.GF2.Matrix` — Dense binary matrix

```haskell
module QEC.GF2.Matrix
  ( BinMatrix(..)
  , bmZero, bmIdentity, bmFromRows, bmFromList
  , bmGetRow, bmSetRow, bmGetEntry
  , bmTranspose, bmMul, bmMulVec
  , bmAddRows     -- row_i XOR= row_j
  , bmSwapRows
  , bmNumRows, bmNumCols
  ) where

-- | Dense binary matrix: m rows × n columns.
-- Stored as a flat Vector of Word64, row-major.
-- Each row is `wordsPerRow` Word64 values.
data BinMatrix = BinMatrix
  { bmRows       :: {-# UNPACK #-} !Int
  , bmCols       :: {-# UNPACK #-} !Int
  , bmWordsPerRow :: {-# UNPACK #-} !Int
  , bmData       :: !(VU.Vector Word64)
  } deriving (Show)

-- Key operations:

-- | Get row i as a BinVec
bmGetRow :: BinMatrix -> Int -> BinVec
bmGetRow (BinMatrix _ c wpr dat) i =
  BinVec c (VU.slice (i * wpr) wpr dat)

-- | XOR row j into row i: row_i ^= row_j
-- This is THE hot inner loop for Gaussian elimination.
bmAddRows :: Int -> Int -> BinMatrix -> BinMatrix
-- Implementation: XOR the Word64 chunks of row j into row i.
-- Use VU.unsafeIndex for speed in inner loop.

-- | Matrix multiplication over GF(2): A (m×k) * B (k×n) → C (m×n)
bmMul :: BinMatrix -> BinMatrix -> BinMatrix
-- For each (i,j) in result: C[i][j] = innerGF2(row_i(A), col_j(B))
-- Transpose B first for cache-friendly column access.
-- Complexity: O(m * n * k/64) with bit-packing.

-- | Matrix-vector multiply: A (m×n) * v (n) → w (m)
bmMulVec :: BinMatrix -> BinVec -> BinVec
-- For each row i: result[i] = innerGF2(row_i, v)

-- | Transpose
bmTranspose :: BinMatrix -> BinMatrix
-- Bit-level transpose. Non-trivial for packed representation.
-- Use 8×8 block transpose with byte-level operations for efficiency.
-- A simple but slower approach: iterate over all (i,j) pairs.
-- Start with the simple approach; optimize later if profiling demands.

-- | m×n zero matrix
bmZero :: Int -> Int -> BinMatrix
-- | n×n identity matrix
bmIdentity :: Int -> BinMatrix
-- | Build from list of BinVec rows
bmFromRows :: [BinVec] -> BinMatrix
```

**Tests**:

- `bmMul A (bmIdentity n) == A`
- `bmMul (bmIdentity m) A == A`
- `bmTranspose (bmTranspose A) == A`
- `bmMul A (bmTranspose B) == bmTranspose (bmMul B (bmTranspose A))` -- NO, wrong identity. Use: `(AB)^T = B^T A^T`
- `bmMulVec (bmIdentity n) v == v`
- For random A (m×k), B (k×n): `bmMul A B` has correct dimensions m×n

---

### 4.3 `QEC.GF2.Gauss` — Gaussian elimination, rank, kernel

```haskell
module QEC.GF2.Gauss
  ( rref          -- Reduced row echelon form
  , rank          -- Rank of a binary matrix
  , kernel        -- Null space (kernel) basis vectors
  , solveLinear   -- Solve Ax = b over GF(2)
  , pivotColumns  -- Return indices of pivot columns after rref
  ) where

-- | Reduced row echelon form over GF(2).
-- Returns (rref_matrix, pivot_columns, row_permutation).
-- Algorithm:
--   For each column c from left to right:
--     Find a row r (at or below current) with a 1 in column c.
--     If none found, skip column (it's a free variable).
--     Swap row r to current position.
--     XOR current row into ALL other rows that have a 1 in column c.
--     Advance current row.
--
-- Complexity: O(m * n * n/64) with bit-packing.
rref :: BinMatrix -> (BinMatrix, [Int])

-- | Rank = number of pivot columns after rref
rank :: BinMatrix -> Int

-- | Kernel (null space) of A: all x such that Ax = 0.
-- Algorithm: compute rref of A. Free variables correspond to
-- non-pivot columns. For each free variable, construct a basis
-- vector by back-substitution.
-- Returns list of basis vectors. Length = n - rank(A).
kernel :: BinMatrix -> [BinVec]

-- | Solve Ax = b. Returns Nothing if inconsistent,
-- Just x for a particular solution (not unique in general).
solveLinear :: BinMatrix -> BinVec -> Maybe BinVec
```

**Tests (CRITICAL — all downstream correctness depends on these)**:

- Rank-nullity: `rank A + length (kernel A) == bmCols A`
- Kernel correctness: for all v in `kernel A`, `bmMulVec A v == bvZero m`
- RREF idempotence: `rref (fst (rref A)) == rref A` (modulo pivot tracking)
- `rank (bmIdentity n) == n`
- `rank (bmZero m n) == 0`
- `length (kernel (bmIdentity n)) == 0`

---

### 4.4 `QEC.Symplectic` — Symplectic inner product

```haskell
module QEC.Symplectic
  ( PauliOp(..)       -- I, X, Y, Z
  , PauliString(..)   -- n-qubit Pauli with phase
  , symplecticInner   -- Symplectic inner product over GF(2)
  , pauliCommutes     -- Do two Paulis commute?
  , pauliFromXZ       -- Construct from X-bits and Z-bits
  ) where

-- | Single-qubit Pauli operator
data PauliOp = I | X | Y | Z deriving (Eq, Ord, Show, Enum)

-- | n-qubit Pauli string: product of Pauli operators with ±1 or ±i phase.
-- Represented as (x-bits, z-bits, phase) where:
--   x=0,z=0 → I; x=1,z=0 → X; x=1,z=1 → Y; x=0,z=1 → Z
data PauliString = PauliString
  { psXBits :: !BinVec    -- x₁,...,xₙ
  , psZBits :: !BinVec    -- z₁,...,zₙ
  , psPhase :: !Int        -- phase = 0,1,2,3 representing i^phase
  } deriving (Show, Eq)

-- | Symplectic inner product: ⟨(x₁,z₁), (x₂,z₂)⟩_Ω = Σᵢ (x₁ᵢ·z₂ᵢ + z₁ᵢ·x₂ᵢ) mod 2
-- Two Paulis commute iff this is 0.
symplecticInner :: PauliString -> PauliString -> GF2
symplecticInner p1 p2 =
  bvInnerGF2 (psXBits p1) (psZBits p2)
  + bvInnerGF2 (psZBits p1) (psXBits p2)
  -- note: + is XOR for GF2

-- | Two Paulis commute iff symplectic inner product = 0
pauliCommutes :: PauliString -> PauliString -> Bool
pauliCommutes p1 p2 = symplecticInner p1 p2 == GF2 False
```

---

### 4.5 `QEC.Code.CSS` — CSS codes with enforced orthogonality

```haskell
module QEC.Code.CSS
  ( CSSCode(..)
  , mkCSSCode        -- Smart constructor: checks H_X · H_Z^T = 0
  , CSSCodeError(..)
  , cssNumQubits, cssNumLogical, cssDistance
  ) where

-- | A CSS code defined by parity check matrices H_X and H_Z.
-- INVARIANT: H_X · H_Z^T = 0 (mod 2). Enforced by smart constructor.
data CSSCode = CSSCode
  { cssHX :: !BinMatrix   -- r_X × n matrix (X stabilizer generators)
  , cssHZ :: !BinMatrix   -- r_Z × n matrix (Z stabilizer generators)
  } deriving (Show)

data CSSCodeError
  = DimensionMismatch      -- H_X and H_Z have different column counts
  | OrthogonalityViolation -- H_X · H_Z^T ≠ 0
  deriving (Show)

-- | Smart constructor. The ONLY way to build a CSSCode.
mkCSSCode :: BinMatrix -> BinMatrix -> Either CSSCodeError CSSCode
mkCSSCode hx hz
  | bmCols hx /= bmCols hz = Left DimensionMismatch
  | bmMul hx (bmTranspose hz) /= bmZero (bmRows hx) (bmRows hz) = Left OrthogonalityViolation
  | otherwise = Right (CSSCode hx hz)

-- | Code parameters
cssNumQubits :: CSSCode -> Int
cssNumQubits = bmCols . cssHX

-- | k = n - rank(H_X) - rank(H_Z)
cssNumLogical :: CSSCode -> Int
cssNumLogical c = cssNumQubits c - rank (cssHX c) - rank (cssHZ c)

-- | Code distance (expensive: minimum weight of a non-trivial logical operator).
-- For small codes, compute via exhaustive search over coset representatives.
-- For large codes, use random sampling lower bound.
-- Returns a lower bound.
cssDistance :: CSSCode -> Int
-- This is NP-hard in general. Implement a simple version:
-- d_X = min weight of v in ker(H_Z) \ rowspace(H_X)
-- d_Z = min weight of v in ker(H_X) \ rowspace(H_Z)
-- d = min(d_X, d_Z)
-- For small codes (n < 50), enumerate kernel vectors.
-- For larger codes, sample random kernel vectors and return min weight found.
```

---

### 4.6 `QEC.Code.Repetition` — Simplest code for validation

```haskell
module QEC.Code.Repetition (repetitionCode) where

-- | Repetition code of distance d: [[d, 1, d]] (CSS form).
-- H_X = empty (0 × d matrix, no X checks for phase-flip-only code)
-- H_Z = (d-1) × d matrix with 1s on diagonal and superdiagonal:
--   1 1 0 0 0
--   0 1 1 0 0
--   0 0 1 1 0
--   0 0 0 1 1
-- This checks Z_i Z_{i+1} stabilizers.
repetitionCode :: Int -> CSSCode
repetitionCode d =
  let n = d
      -- H_Z: (d-1) × d matrix
      hz = bmFromRows [ ... ] -- row i has 1s at columns i and i+1
      -- H_X: 0 × d matrix (no X stabilizers for pure phase-flip code)
      hx = bmZero 0 d
  in case mkCSSCode hx hz of
       Right c -> c
       Left e  -> error $ "repetitionCode: impossible: " ++ show e
```

For a standard repetition code that corrects BOTH X and Z:

```haskell
-- Full repetition code correcting all errors: [[d, 1, d]]
-- H_X is same structure as H_Z
repetitionCodeFull :: Int -> CSSCode
repetitionCodeFull d =
  let hz = adjacencyMatrix d  -- (d-1)×d
      hx = adjacencyMatrix d  -- same structure
  in case mkCSSCode hx hz of ...
```

**IMPORTANT**: For cat qubits, we primarily use the phase-flip-only version (empty H_X) because bit-flips are exponentially suppressed.

---

### 4.7 `QEC.Code.Surface` — Rotated surface code

```haskell
module QEC.Code.Surface (surfaceCode) where

-- | Rotated surface code of distance d: [[(d²+1)/2, 1, d]] for odd d.
-- Physical qubits live on the faces and edges of a d×d grid.
-- n = d² data qubits. (d²-1)/2 X checks, (d²-1)/2 Z checks.
--
-- Layout for d=3 (9 data qubits, 4 X checks, 4 Z checks):
--
--   X checks are on white faces, Z checks on dark faces
--   of a checkerboard pattern on the d×d grid.
--
-- H_X: each row has 1s for the data qubits adjacent to an X check
-- H_Z: each row has 1s for the data qubits adjacent to a Z check
--
-- For the rotated code, checks have weight 4 (interior) or weight 2 (boundary).
surfaceCode :: Int -> CSSCode
-- Implementation: number qubits on a d×d grid.
-- For each interior face, create a weight-4 check.
-- For boundary faces, create weight-2 checks.
-- Assign X vs Z based on checkerboard coloring.
```

**Test**: `cssNumLogical (surfaceCode d) == 1` for all d. **Test**: `cssNumQubits (surfaceCode d) == d*d` for odd d (for the standard formulation; the rotated code has (d²+1)/2 but this depends on convention — use d² initially for simplicity).

---

### 4.8 `QEC.Code.LDPCCat` — The Ruiz et al. LDPC-cat family

This is a key novel code family for the paper.

```haskell
module QEC.Code.LDPCCat (ldpcCatCode) where

-- | LDPC-cat code from Ruiz et al. (Nature Comms 2025).
-- Parameters: [165 + 8*ell, 34 + 2*ell, 22]
-- where ell >= 0 determines the code size.
--
-- Since cat qubits suppress bit-flips, this is essentially a
-- CLASSICAL LDPC code used for phase-flip correction only.
-- The quantum code has:
--   H_X = empty (no X checks needed)
--   H_Z = classical LDPC parity check matrix
--
-- The parity check matrix has cellular automaton structure:
-- each row is a shifted version of a base row pattern.
-- The base pattern and shift structure come from Ruiz et al.
-- Table I and Supplementary Information.
--
-- For the specific codes in the paper:
--   ell=0:  [165, 34, 22]  (smallest)
--   ell=33: [429, 100, 22] (the one achieving 758 total qubits for 100 logical)
--
-- CONSTRUCTION: The parity check matrix H has (n-k) rows and n columns.
-- The structure is based on circulant blocks.
-- SEE: Ruiz et al. arXiv:2401.09541, Section "Code construction"
-- and the Python code at github.com/DiegoRuiz-Git/LDPCat
ldpcCatCode :: Int -> CSSCode
ldpcCatCode ell =
  let n = 165 + 8 * ell
      k = 34 + 2 * ell
      numChecks = n - k  -- = 131 + 6*ell
      hz = constructLDPCCatParityCheck ell
      hx = bmZero 0 n
  in case mkCSSCode hx hz of ...

-- AGENT NOTE: You will need to reverse-engineer or reproduce the
-- parity check matrix construction from:
--   1. The paper: arXiv:2401.09541 (Section III and Appendix)
--   2. The Python code: github.com/DiegoRuiz-Git/LDPCat
-- The construction uses a seed row and cyclic shifts.
-- Start with the smallest code (ell=0, n=165) and validate parameters.
```

---

### 4.9 `QEC.Noise.CatQubit` — Physics-based noise model

```haskell
module QEC.Noise.CatQubit
  ( CatQubitParams(..)
  , catBitFlipRate, catPhaseFlipRate, catBias
  , catPauliChannel
  , PauliChannel(..)
  ) where

-- | Physical parameters of a cat qubit.
data CatQubitParams = CatQubitParams
  { catAlphaSq     :: !Double   -- |α|², mean photon number (2-20)
  , catKappa1      :: !Double   -- κ₁, single-photon loss rate (Hz)
  , catKappa2      :: !Double   -- κ₂, two-photon dissipation rate (Hz)
  , catCycleTimeNs :: !Double   -- Syndrome extraction cycle time (ns)
  , catSqueezing   :: !Double   -- Squeezing parameter γ (2.0 standard, 4.3 squeezed)
  } deriving (Show, Eq)

-- | Commonly used parameter sets from the literature
defaultCatParams :: CatQubitParams
defaultCatParams = CatQubitParams
  { catAlphaSq     = 19        -- Gouzien et al. 2023
  , catKappa1      = 1e3       -- ~kHz range (1/T₁)
  , catKappa2      = 1e8       -- ~100 MHz (κ₂/2π ≈ 2 MHz × 2π, times enhancement)
  , catCycleTimeNs = 500       -- 500 ns target from Gouzien et al.
  , catSqueezing   = 2.0       -- standard (non-squeezed)
  }

-- | A Pauli error channel: independent X, Y, Z errors.
data PauliChannel = PauliChannel
  { pcPx :: !Double   -- Prob of X error
  , pcPy :: !Double   -- Prob of Y error
  , pcPz :: !Double   -- Prob of Z error
  } deriving (Show, Eq)

-- | Bit-flip rate: exponentially suppressed.
-- Γ_X ∝ exp(-γ * |α|²) where γ=2 (standard) or γ=4.3 (squeezed)
-- More precisely: p_X ≈ (κ₁/κ₂) * |α|² * exp(-2γ|α|²) * T_cycle
-- Reference: Guillaud & Mirrahimi, PRX 2019; Rousseau et al. 2025
catBitFlipRate :: CatQubitParams -> Double
catBitFlipRate p =
  let alpha2 = catAlphaSq p
      gamma  = catSqueezing p
      ratio  = catKappa1 p / catKappa2 p
      tCycle = catCycleTimeNs p * 1e-9  -- convert to seconds
      kappa2 = catKappa2 p
  in ratio * alpha2 * exp (-gamma * alpha2) * kappa2 * tCycle
  -- NOTE: This formula is approximate. The exact prefactor depends on
  -- the specific gate being performed. For idle qubits, this is the
  -- leading-order expression. See Guillaud & Mirrahimi Eq. (8-11).

-- | Phase-flip rate: grows linearly with |α|².
-- p_Z ≈ κ₁ * |α|² * T_cycle
-- This is the DOMINANT error channel.
catPhaseFlipRate :: CatQubitParams -> Double
catPhaseFlipRate p =
  let alpha2 = catAlphaSq p
      tCycle = catCycleTimeNs p * 1e-9
  in catKappa1 p * alpha2 * tCycle

-- | Noise bias η = p_Z / p_X
catBias :: CatQubitParams -> Double
catBias p = catPhaseFlipRate p / max 1e-30 (catBitFlipRate p)

-- | Convert to Pauli channel for use in simulation.
-- p_Y ≈ p_X (both exponentially suppressed)
catPauliChannel :: CatQubitParams -> PauliChannel
catPauliChannel p = PauliChannel
  { pcPx = catBitFlipRate p
  , pcPy = catBitFlipRate p   -- Y = iXZ, same order as X
  , pcPz = catPhaseFlipRate p
  }
```

**Validation data points** (use these to check the model):

- At |α|²=4, γ=2: bit-flip time ~10s (Réglade 2024) → p_X ≈ T_cycle/T_flip
- At |α|²=4.1, γ=4.3: bit-flip time ~22s (Rousseau 2025)
- At |α|²=11, γ≈2: bit-flip time >1 hour (Alice & Bob 2025)
- At |α|²=19, κ₁/κ₂=10⁻⁵: bias η ≈ 10⁶+ (Gouzien 2023 assumption)

---

### 4.10 `QEC.Decoder.BP` — Belief Propagation

```haskell
module QEC.Decoder.BP
  ( BPConfig(..)
  , bpDecode
  , BPResult(..)
  ) where

data BPConfig = BPConfig
  { bpMaxIter    :: !Int     -- Maximum iterations (default: 100)
  , bpMethod     :: !BPMethod -- MinSum or ProductSum
  , bpMinSumAlpha :: !Double  -- Scaling factor for MinSum (default: 0.625)
  } deriving (Show)

data BPMethod = MinSum | ProductSum deriving (Show, Eq)

data BPResult = BPResult
  { bpConverged  :: !Bool
  , bpCorrection :: !BinVec    -- Estimated error vector
  , bpSoftOutput :: !(VU.Vector Double)  -- Log-likelihood ratios (for OSD)
  , bpIterations :: !Int
  } deriving (Show)

-- | Run belief propagation on the Tanner graph of H.
--
-- INPUTS:
--   H:         (m × n) parity check matrix
--   syndrome:  m-bit syndrome vector (= H · error mod 2)
--   channel:   per-bit prior error probabilities (length n)
--
-- ALGORITHM (min-sum variant):
--   1. Initialize variable-to-check messages:
--      For each variable node j, for each check i connected to j:
--        q_{j→i} = channel_LLR_j = log((1-p_j)/p_j)
--
--   2. Check-to-variable update:
--      For each check i, for each variable j connected to i:
--        r_{i→j} = (-1)^{s_i} × sign_product × min_magnitude
--      where:
--        sign_product = product of sign(q_{j'→i}) for all j' ≠ j connected to i
--        min_magnitude = min of |q_{j'→i}| for all j' ≠ j connected to i
--        s_i = syndrome bit i
--      Scale by alpha (0.625 for normalized min-sum).
--
--   3. Variable-to-check update:
--      For each variable j, for each check i connected to j:
--        q_{j→i} = channel_LLR_j + sum of r_{i'→j} for all i' ≠ i connected to j
--
--   4. Tentative decision:
--      For each variable j:
--        total_LLR_j = channel_LLR_j + sum of all r_{i→j}
--        ê_j = 1 if total_LLR_j < 0, else 0
--
--   5. Check convergence: if H · ê == syndrome, return success.
--      Otherwise, go to step 2.
--
-- DATA STRUCTURES:
--   Use adjacency lists for the Tanner graph (sparse H).
--   Messages stored in a flat Double vector indexed by edge.
--
-- For BIASED NOISE: channel_LLR_j = log((1-p_z_j)/p_z_j) for Z errors.
-- Under high bias (η >> 1), p_z >> p_x, so LLRs are small (close to 0)
-- for Z errors, driving the decoder to correct them aggressively.
bpDecode :: BinMatrix -> BinVec -> VU.Vector Double -> BPConfig -> BPResult
```

---

### 4.11 `QEC.Decoder.OSD` — Ordered Statistics Decoding

```haskell
module QEC.Decoder.OSD
  ( OSDConfig(..)
  , osdDecode
  , bpOsdDecode  -- Combined BP+OSD
  ) where

data OSDConfig = OSDConfig
  { osdOrder :: !Int   -- OSD order w (0 = OSD-0, i.e. just Gaussian elimination)
  } deriving (Show)

-- | OSD post-processing after BP.
--
-- ALGORITHM (OSD-0):
--   1. Sort columns of H by BP soft output (most reliable first).
--   2. Apply the same permutation to the syndrome.
--   3. Gaussian elimination on permuted H to find pivot columns
--      (the "information set" I of k=rank(H) most reliable bits).
--   4. Solve: ê_I = H_I^{-1} · syndrome (restricted to pivot rows/cols).
--   5. Set non-pivot (redundant) positions to 0.
--   6. Undo the permutation.
--
-- ALGORITHM (OSD-w, order w > 0):
--   Same as OSD-0, but additionally:
--   For each test error pattern (TEP) of weight ≤ w on the REDUNDANT positions:
--     Compute correction = OSD-0 correction ⊕ TEP contribution.
--     If this correction has lower weight, keep it.
--   Number of TEPs = sum_{i=0}^{w} C(n-k, i).
--   OSD-10 on a [429,100] code: C(329, ≤10) is enormous.
--   Practical limit: w ≤ 5-7 for codes with n < 500.
--
-- NOTE: OSD-0 alone is often sufficient for biased noise because
-- BP provides good soft information when the noise is highly asymmetric.
osdDecode :: BinMatrix -> BinVec -> VU.Vector Double -> OSDConfig -> BinVec

-- | Combined BP+OSD: run BP first, then OSD if BP didn't converge.
bpOsdDecode :: BinMatrix -> BinVec -> VU.Vector Double -> BPConfig -> OSDConfig -> BinVec
bpOsdDecode h syndrome channel bpConf osdConf =
  let bpResult = bpDecode h syndrome channel bpConf
  in if bpConverged bpResult
     then bpCorrection bpResult
     else osdDecode h syndrome (bpSoftOutput bpResult) osdConf
```

---

### 4.12 `QEC.Stabilizer.Tableau` — Aaronson-Gottesman CHP algorithm

```haskell
module QEC.Stabilizer.Tableau
  ( Tableau(..)
  , newTableau          -- |0...0⟩ initial state
  , applyH, applyS, applyCNOT, applyCZ
  , measure             -- Measure qubit, returns (bit, updated_tableau)
  , getStabilizers      -- Extract stabilizer generators as PauliStrings
  ) where

-- | Stabilizer tableau for n qubits.
-- 2n generators: rows 0..n-1 are destabilizers, rows n..2n-1 are stabilizers.
-- Each generator is a Pauli string on n qubits: (x-bits, z-bits, phase).
-- Stored as 3 arrays: xBits, zBits (each 2n rows × ceil(n/64) words), signs (2n bytes).
data Tableau = Tableau
  { tabN     :: {-# UNPACK #-} !Int
  , tabXBits :: !(VU.Vector Word64)   -- 2n rows × wpr words, row-major
  , tabZBits :: !(VU.Vector Word64)   -- same shape
  , tabSigns :: !(VU.Vector Word8)    -- 2n elements, 0 or 1
  , tabWpr   :: {-# UNPACK #-} !Int   -- words per row = ceil(n/64)
  } deriving (Show)

-- | Initialize to |0⟩^⊗n.
-- Destabilizer i = X_i  (row i:     x-bit i = 1, all z-bits = 0)
-- Stabilizer i  = Z_i  (row n+i:   all x-bits = 0, z-bit i = 1)
-- All signs = 0 (+1 phase).
newTableau :: Int -> Tableau

-- | Hadamard on qubit a.
-- For each row i: swap x_{i,a} and z_{i,a}, then sign_i ^= x_{i,a} * z_{i,a}
applyH :: Int -> Tableau -> Tableau

-- | Phase gate S on qubit a.
-- For each row i: sign_i ^= x_{i,a} * z_{i,a}, then z_{i,a} ^= x_{i,a}
applyS :: Int -> Tableau -> Tableau

-- | CNOT from control a to target b.
-- For each row i:
--   sign_i ^= x_{i,a} * z_{i,b} * (x_{i,b} XOR z_{i,a} XOR 1)
--   x_{i,b} ^= x_{i,a}
--   z_{i,a} ^= z_{i,b}
applyCNOT :: Int -> Int -> Tableau -> Tableau

-- | Measure qubit a in Z basis.
-- Returns (measurement_result :: Bool, updated_tableau).
-- Algorithm (Aaronson & Gottesman 2004, Section III):
--   1. Check if any stabilizer generator (rows n..2n-1) has x_{i,a} = 1.
--   2. If YES (random outcome):
--      Let p be such a stabilizer. Set all other stabilizers/destabilizers
--      that anticommute with measurement to their product with p.
--      Replace destabilizer p-n with old stabilizer p.
--      Set stabilizer p to ±Z_a.
--      Return random bit.
--   3. If NO (deterministic outcome):
--      Outcome determined by destabilizers. Compute by rowsum.
--      Return determined bit without modifying tableau.
measure :: Int -> Tableau -> IO (Bool, Tableau)
-- Uses IO for random bit in the random-outcome case.
-- Alternative: pass in a PRNG explicitly.
```

**Tests**:

- Measure |0⟩ → always 0
- Measure H|0⟩ = |+⟩ → random 50/50
- Create Bell state (CNOT · (H⊗I) · |00⟩), measure both → always agree
- Apply X then measure → always 1
- Applying H twice returns to original state (up to global phase)

---

### 4.13 `QEC.Simulation` — Monte Carlo sampling engine

```haskell
module QEC.Simulation
  ( SimConfig(..)
  , SimResult(..)
  , runSimulation
  , logicalErrorRate
  ) where

import System.Random (StdGen, split, randomR)
import Control.Parallel.Strategies (parMap, rdeepseq)

data SimConfig = SimConfig
  { simShots        :: !Int           -- Number of Monte Carlo trials
  , simPhysicalPz   :: !Double        -- Physical Z error probability
  , simPhysicalPx   :: !Double        -- Physical X error probability
  , simRounds       :: !Int           -- Number of syndrome rounds
  , simBPConfig     :: !BPConfig
  , simOSDConfig    :: !OSDConfig
  , simNumCores     :: !Int           -- Parallelism
  } deriving (Show)

data SimResult = SimResult
  { simTotalTrials   :: !Int
  , simLogicalErrors :: !Int
  , simLogicalErrorRate :: !Double
  , simStdError      :: !Double       -- Statistical uncertainty
  } deriving (Show)

-- | Run Monte Carlo simulation for a CSS code.
--
-- For each trial:
--   1. Sample a random error vector e of length n,
--      where each bit is 1 with probability p_z (for Z errors).
--   2. Compute syndrome s = H_Z · e (mod 2).
--   3. Decode: ê = bpOsdDecode H_Z s priors
--   4. Check logical error: compute residual r = e ⊕ ê.
--      A logical error occurred if r is NOT in the row space of H_Z
--      (i.e., r is a non-trivial logical operator).
--      Equivalently: r is in ker(H_X) \ rowspace(H_Z).
--      Quick check: if H_X · r ≠ 0, it's not even a valid codeword diff → error.
--                   if r = 0 or r ∈ rowspace(H_Z), no logical error.
--                   else logical error.
--
-- Parallelize by splitting the PRNG and distributing shots across cores.
runSimulation :: CSSCode -> SimConfig -> IO SimResult
runSimulation code conf = do
  gen <- newStdGen
  let gens = take (simNumCores conf) (iterate (snd . split) gen)
      shotsPerCore = simShots conf `div` simNumCores conf
      results = parMap rdeepseq (\g -> runBatch code conf shotsPerCore g) gens
      totalErrors = sum (map fst results)
      totalTrials = sum (map snd results)
      rate = fromIntegral totalErrors / fromIntegral totalTrials
      stdErr = sqrt (rate * (1 - rate) / fromIntegral totalTrials)
  return (SimResult totalTrials totalErrors rate stdErr)

-- | Check if residual error is a logical error.
-- residual = error XOR correction
-- Logical error if residual ∈ ker(H_Z) but residual ∉ rowspace(H_X^T)?
-- More precisely for CSS codes:
-- Z logical error: residual_Z ∈ ker(H_X) \ rowspace(H_Z^T)
-- For phase-flip-only codes (empty H_X): any nonzero residual
-- in ker(trivial) is a logical error if it has nontrivial overlap
-- with logical Z operators.
--
-- SIMPLEST CHECK: precompute logical operator representatives L_i.
-- Logical error occurred if any L_i has odd overlap with residual.
isLogicalError :: CSSCode -> [BinVec] -> BinVec -> Bool
isLogicalError _code logicalOps residual =
  any (\l -> bvInnerGF2 l residual /= GF2 False) logicalOps
```

---

### 4.14 `QEC.Resource` — Resource estimation pipeline

```haskell
module QEC.Resource
  ( Algorithm(..)
  , ResourceEstimate(..)
  , estimateResources
  ) where

data Algorithm = Algorithm
  { algoName        :: !String
  , algoLogicalQubits :: !Int       -- Number of logical qubits needed
  , algoToffoliCount  :: !Double    -- Total Toffoli gates (can be > 10^9)
  , algoTDepth        :: !Double    -- T/Toffoli depth (sequential gates)
  , algoErrorBudget   :: !Double    -- Target total failure probability (e.g. 1/3)
  } deriving (Show)

-- | Pre-defined algorithm specifications
shor_rsa2048 :: Algorithm
shor_rsa2048 = Algorithm
  { algoName = "Shor RSA-2048"
  , algoLogicalQubits = 1400    -- Gidney 2025
  , algoToffoliCount  = 6.5e9   -- Gidney 2025
  , algoTDepth        = 1.0e9   -- approximate
  , algoErrorBudget   = 1.0/3.0
  }

ecdlp_256 :: Algorithm
ecdlp_256 = Algorithm
  { algoName = "ECDLP 256-bit"
  , algoLogicalQubits = 2 * 256 + 256  -- ~768 logical qubits (Gouzien et al.)
  , algoToffoliCount  = 1.28e11        -- 448*256³*log₂(256) + 4090*256³
  , algoTDepth        = 1.0e9
  , algoErrorBudget   = 1.0/3.0
  }

data ResourceEstimate = ResourceEstimate
  { reDataQubits      :: !Int
  , reSyndromeQubits  :: !Int
  , reRoutingQubits   :: !Int
  , reFactoryQubits   :: !Int
  , reTotalQubits     :: !Int
  , reRuntimeSeconds  :: !Double
  , reCodeDistance     :: !Int
  , reLogicalErrorPerCycle :: !Double
  , reCodeFamily      :: !String
  } deriving (Show)

-- | Main resource estimation function.
--
-- PIPELINE:
-- 1. Determine per-logical-qubit error budget:
--    p_logical = algoErrorBudget / (algoLogicalQubits * num_cycles)
--
-- 2. Find minimum code distance d such that:
--    logicalErrorRate(code, d, physical_noise) ≤ p_logical
--    This uses the simulation results or analytical formula.
--    Analytical formula (approximation):
--      p_L ≈ A × (p_phys / p_threshold)^((d+1)/2)
--    where A and p_threshold are code-family-specific.
--
-- 3. Compute qubit counts:
--    data_qubits = algoLogicalQubits × (qubits_per_logical for this code at distance d)
--    syndrome_qubits = number of ancilla qubits for syndrome extraction
--    routing_qubits = overhead for lattice surgery / routing (code-dependent)
--    factory_qubits = magic state factory footprint × number of factories
--
-- 4. Compute number of factories:
--    factory_cycle_time = d × physical_cycle_time × rounds_per_distillation
--    factories_needed = ceil(toffoli_count / (runtime / factory_cycle_time))
--    This is a fixed-point equation: runtime depends on code distance,
--    which depends on error budget, which depends on runtime.
--    Solve iteratively.
--
-- 5. Runtime:
--    runtime = algo_T_depth × logical_cycle_time
--    logical_cycle_time = d × physical_cycle_time
estimateResources :: Algorithm -> CatQubitParams -> CSSCode -> ResourceEstimate
```

---

## 5. Key Algorithms — Pseudocode

### 5.1 Gaussian Elimination over GF(2) (CRITICAL PATH)

```
FUNCTION rref(A: m×n binary matrix) → (R, pivots):
  R ← copy of A
  pivots ← []
  current_row ← 0

  FOR col = 0 TO n-1:
    # Find pivot in this column at or below current_row
    pivot_row ← -1
    FOR row = current_row TO m-1:
      IF R[row][col] == 1:
        pivot_row ← row
        BREAK

    IF pivot_row == -1:
      CONTINUE  # Free variable, skip column

    # Swap pivot row to current position
    SWAP R[current_row] and R[pivot_row]  # Word64-level swap

    # Eliminate all other rows with a 1 in this column
    FOR row = 0 TO m-1:
      IF row ≠ current_row AND R[row][col] == 1:
        R[row] ← R[row] XOR R[current_row]  # Word64-level XOR

    APPEND col TO pivots
    current_row ← current_row + 1

  RETURN (R, pivots)
```

### 5.2 Belief Propagation (Min-Sum)

```
FUNCTION bp_decode(H: m×n, syndrome: m-bit, priors: n doubles, config) → result:
  # Build adjacency structure
  FOR each nonzero entry H[i][j]:
    checks_of_var[j] ← append i
    vars_of_check[i] ← append j

  # Initialize messages
  q[j→i] ← log((1-priors[j]) / priors[j])  for each edge (j,i)

  FOR iter = 1 TO config.maxIter:
    # Check-to-variable update
    FOR each check i:
      FOR each variable j in vars_of_check[i]:
        sign ← (-1)^syndrome[i]
        min_abs ← +∞
        FOR each j' in vars_of_check[i], j' ≠ j:
          sign ← sign × sgn(q[j'→i])
          min_abs ← min(min_abs, |q[j'→i]|)
        r[i→j] ← sign × min_abs × config.alpha

    # Variable-to-check update
    FOR each variable j:
      FOR each check i in checks_of_var[j]:
        q[j→i] ← channel_llr[j] + sum(r[i'→j] for i' in checks_of_var[j], i' ≠ i)

    # Tentative hard decision
    FOR each variable j:
      total_llr[j] ← channel_llr[j] + sum(r[i→j] for i in checks_of_var[j])
      ê[j] ← 1 if total_llr[j] < 0 else 0

    # Check convergence
    IF H · ê == syndrome:
      RETURN BPResult(converged=True, correction=ê, softOutput=total_llr, iter)

  RETURN BPResult(converged=False, correction=ê, softOutput=total_llr, maxIter)
```

### 5.3 OSD-0

```
FUNCTION osd0(H: m×n, syndrome: m-bit, soft_output: n doubles) → correction:
  # Sort by reliability (most reliable = highest |LLR|, first)
  perm ← argsort(|soft_output|, descending)
  H' ← permute_columns(H, perm)
  s' ← syndrome  # syndrome doesn't change with column permutation

  # Gaussian elimination to find information set
  (R, pivots) ← rref(H')
  k ← length(pivots)  # = rank(H)

  # Solve for pivot positions
  ê ← zero vector of length n
  FOR i = k-1 DOWNTO 0:
    ê[pivots[i]] ← s'[i]  # back-substitution from RREF
    FOR j = i+1 TO k-1:
      ê[pivots[i]] ← ê[pivots[i]] XOR (R[i][pivots[j]] AND ê[pivots[j]])

  # Undo permutation
  correction ← inverse_permute(ê, perm)
  RETURN correction
```

---

## 6. Validation Milestones

### Milestone 1 (end of Phase 1): GF(2) algebra works

- [ ] All QuickCheck properties pass (rank-nullity, kernel correctness, rref idempotence)
- [ ] Can construct and verify CSS orthogonality for hand-built small codes
- [ ] Benchmark: 1000×1000 GF(2) Gaussian elimination < 100ms

### Milestone 2 (end of Phase 2): Decode surface codes

- [ ] Surface code construction: `cssNumLogical (surfaceCode d) == 1` for d = 3,5,7,9
- [ ] BP+OSD decodes repetition code syndromes correctly
- [ ] Code-capacity threshold estimate for d=3,5,7 surface code is ~10% ± 1% (Code capacity = errors only on data qubits, perfect syndrome measurements)

### Milestone 3 (end of Phase 3): Reproduce LDPC-cat results

- [ ] LDPC-cat code [165,34,22] (ℓ=0) constructed, CSS orthogonality verified
- [ ] Cat qubit noise model produces bias η ≈ 10⁶ at |α|²=19
- [ ] Monte Carlo: logical error rate for LDPC-cat code matches Ruiz et al. Fig 3 within statistical uncertainty (order of magnitude agreement is sufficient initially)

### Milestone 4 (end of Phase 4): Resource estimates

- [ ] Reproduce Gouzien et al.: ~126,000 cat qubits for ECDLP-256 (within 10%)
- [ ] Produce novel LDPC-cat resource estimates for ECDLP-256 and RSA-2048
- [ ] JSON export of comparison table across architectures

---

## 7. Critical Implementation Notes

### Performance priorities

1. **Bit-packing is non-negotiable.** The entire library's performance depends on GF(2) operations using Word64. Do not use `Bool` vectors or `[GF2]` lists in any inner loop.
    
2. **Strict fields everywhere.** Use `StrictData` extension (already in default-extensions). Use `{-# UNPACK #-}` on Int and Double fields in hot data structures.
    
3. **Avoid lazy evaluation in simulation.** Use `seq`, `deepseq`, `BangPatterns` in the Monte Carlo loop. A single space leak will destroy performance.
    
4. **Profiling from day 1.** Build with `cabal bench` using `tasty-bench` from Phase 1 onward. Track GF(2) matrix operation throughput.
    

### Numerical precision

- All LLR computations in BP should use `Double`. Avoid `log(0)` by clamping probabilities to `[1e-15, 1-1e-15]`.
- The cat qubit bit-flip rate at high |α|² can underflow `Double` (exp(-38) ≈ 3×10⁻¹⁷ is near the limit). Use `log` domain for extreme parameter regimes.

### Random number generation

- Use `System.Random.SplitMix` (imported via `splitmix` package) for the Monte Carlo PRNG. It's the fastest pure Haskell generator with good statistical properties.
- Split the generator for parallelism: `splitSMGen` produces two independent streams.
- For sampling Bernoulli(p): generate `Double` in [0,1), compare to p. Do NOT use `randomR (0,1)` from `System.Random` (it's slow).

### What NOT to implement initially

- **Elevator codes and Romanesco codes**: These are stretch goals. Implement only if Phase 4 finishes early.
- **Circuit-level noise simulation**: Start with code-capacity noise (errors only on data qubits, noiseless syndrome extraction). Circuit-level noise (noisy CNOTs in syndrome extraction, measurement errors) is a Phase 3+ optimization.
- **MWPM decoder**: BP+OSD handles all code families. MWPM is only needed for comparison with surface code literature.
- **Pauli frame simulation**: The full Stim-style frame simulator is a Phase 3 optimization. The basic Monte Carlo (sample error → compute syndrome → decode → check) works without it.

---

## 8. Key References for the Agent

If you need to look up implementation details:

1. **Aaronson & Gottesman (2004)** — Tableau algorithm. arXiv:quant-ph/0406196. Section III has all gate update rules.
2. **Gidney, Stim (2021)** — Pauli frame trick. arXiv:2103.02202. Section IV.
3. **Roffe et al. (2020)** — BP+OSD algorithm. arXiv:2005.07016. Section III-IV.
4. **Gouzien et al. (2023)** — Repetition-cat resource estimation. arXiv:2302.06639. Python code at github.com/ElieGouzien/elliptic_log_cat.
5. **Ruiz et al. (2025)** — LDPC-cat codes. arXiv:2401.09541. Python/C++ code at github.com/DiegoRuiz-Git/LDPCat.
6. **Puri et al. (2020)** — Bias-preserving gates. arXiv:1905.00450. Noise model equations.
7. **Bravyi et al. (2024)** — Bivariate bicycle codes. arXiv:2308.07915. Code at github.com/sbravyi/BivariateBicycleCodes.

---

## 9. Output Format

The library's final output is a `ResourceEstimate` record (see §4.14) that can be serialized to JSON via `aeson`. The `QEC.Export` module should provide:

```haskell
module QEC.Export (exportJSON, exportCSV) where

-- | Export a list of resource estimates as a JSON array
exportJSON :: [ResourceEstimate] -> ByteString

-- | Export as CSV with headers
exportCSV :: [ResourceEstimate] -> ByteString
```

The paper's comparison table is generated by running `estimateResources` with different code families and parameters, then exporting the results.

---

## 10. Summary of Dependencies Between Modules

```
GF2 ← GF2.Matrix ← GF2.Gauss ← Symplectic
                  ↑                    ↑
                  Code ← Code.CSS ← Code.Repetition
                  ↑        ↑         Code.Surface
                  ↑        ↑         Code.LDPCCat
                  ↑        ↑         Code.BivariateBicycle
                  ↑        ↑
                  Noise ← Noise.CatQubit
                  ↑
                  Decoder ← Decoder.BP ← Decoder.OSD
                  ↑
                  Stabilizer.Tableau
                  ↑
                  Simulation
                  ↑
                  Resource ← Resource.Algorithm
                           ← Resource.MagicState
                           ← Resource.Layout
                  ↑
                  Export
```

Start at the top (GF2), work down. Never skip a level.