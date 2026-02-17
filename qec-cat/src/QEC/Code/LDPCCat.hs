-- | LDPC-cat codes from Ruiz et al. (Nature Communications 2025, arXiv:2401.09541).
--
-- This is a family of classical LDPC codes used for phase-flip correction
-- in cat qubit architectures. The base code is @[136, 34, 22]@ constructed
-- via a cellular automaton on an H×L grid with periodic boundary conditions.
--
-- Since cat qubits exponentially suppress bit-flips, only Z (phase-flip)
-- errors need correction, so:
--
--   * H_X = empty (no X stabilizers needed)
--   * H_Z = classical LDPC parity check matrix (weight-4 checks)
--
-- The parity check matrix is constructed by the cellular automaton method
-- from @FractalCode.py@ in the paper's repository. Each stabilizer level
-- has a 2×3 binary pattern determining which qubits participate.
module QEC.Code.LDPCCat
  ( ldpcCatCode
  , fractalCode
  , torusCode
  , Stabilizer3x3
  ) where

import QEC.GF2
import QEC.GF2.Matrix
import QEC.Code.CSS

-- | A 3x3 binary stabilizer pattern, stored as 9 bits row-major.
-- Bit (i,j) with 0 <= i,j < 3 is at index 3*i + j.
type Stabilizer3x3 = [Bool]

------------------------------------------------------------------------
-- Cellular automaton (fractal) construction
------------------------------------------------------------------------

-- | Stabilizer patterns for each level, indexed by H value.
-- Each pattern is a 2×3 matrix stored row-major (6 entries).
-- From FractalCode.py CreateCheckMatrix for H=4..8.
type StabPattern = [[Int]]  -- outer: 2 rows, inner: 3 cols

-- | Get the stabilizer patterns for a given H.
-- These are the exact patterns from FractalCode.py.
stabPatterns :: Int -> [StabPattern]
stabPatterns 4 =
  [ [[1,1,1],[0,0,0]]
  , [[1,0,1],[1,0,0]]
  ]
stabPatterns 5 =
  [ [[1,1,1],[0,0,0]]
  , [[1,1,1],[0,0,0]]
  , [[1,0,1],[1,0,0]]
  ]
stabPatterns 6 =
  [ [[1,1,1],[0,0,0]]
  , [[1,1,1],[0,0,0]]
  , [[1,0,1],[1,0,0]]
  , [[1,0,1],[1,0,0]]
  ]
stabPatterns 7 =
  [ [[1,1,1],[0,0,0]]
  , [[1,1,1],[0,0,0]]
  , [[1,0,1],[1,0,0]]
  , [[1,0,1],[1,0,0]]
  , [[0,0,1],[1,0,1]]
  ]
stabPatterns 8 =
  [ [[1,1,1],[0,0,0]]
  , [[1,1,1],[0,0,0]]
  , [[1,0,1],[1,0,0]]
  , [[1,0,1],[1,0,0]]
  , [[0,0,1],[1,0,1]]
  , [[1,0,0],[1,0,1]]
  ]
stabPatterns h = error $ "stabPatterns: unsupported H=" ++ show h ++ " (use 4..8)"

-- | Build the parity check matrix for the fractal LDPC code.
--
-- Parameters: H = number of qubit rows (4..8), L = number of columns.
-- The code has n = H*L physical qubits, (H-2)*L checks.
-- Each check has weight exactly 4: one pointed qubit plus three support qubits.
--
-- Construction from FractalCode.py CreateCheckMatrix:
-- For stabilizer level i (0..H-3), position j (0..L-1):
--   - Pointed qubit: (i+2)*L + j
--   - Support qubits: from nonzero entries of stabPatterns[i]
--     For each nonzero at (row_k, col_k): qubit = (i + row_k)*L + ((j - 1 + col_k) mod L)
fractalCode :: Int -> Int -> BinMatrix
fractalCode h l
  | h < 4 || h > 8 = error $ "fractalCode: H must be 4..8, got " ++ show h
  | l < 3          = error $ "fractalCode: L must be >= 3, got " ++ show l
  | otherwise = bmFromRows checkRows
  where
    n = h * l
    pats = stabPatterns h
    numLevels = h - 2  -- number of stabilizer levels

    checkRows =
      [ makeCheckRow i j
      | i <- [0 .. numLevels - 1]
      , j <- [0 .. l - 1]
      ]

    makeCheckRow i j = bvFromList
      [ if col `elem` supportCols then GF2 True else GF2 False
      | col <- [0 .. n - 1]
      ]
      where
        pat = pats !! i
        -- Pointed qubit
        pointed = (i + 2) * l + j
        -- Support qubits from nonzero entries of the 2x3 pattern
        supports =
          [ (i + rowK) * l + ((j - 1 + colK) `mod` l)
          | (rowK, row) <- zip [0..] pat
          , (colK, val) <- zip [0..] row
          , val == 1
          ]
        supportCols = pointed : supports

-- | Construct an LDPC-cat code from Ruiz et al.
-- Parameter @ell@ >= 0 determines the code size.
-- Base code (ell=0): @[136, 34, 22]@ with H=8, L=17.
-- Extension: @[136+8*ell, 34+2*ell, 22]@ with H=8, L=17+ell.
ldpcCatCode :: Int -> CSSCode
ldpcCatCode ell
  | ell < 0   = error "ldpcCatCode: ell must be >= 0"
  | otherwise = case mkCSSCode hx hz of
      Left err   -> error $ "ldpcCatCode: unexpected error: " ++ show err
      Right code -> code
  where
    h = 8
    l = 17 + ell
    n = h * l
    hz = fractalCode h l
    hx = bmZero 0 n

------------------------------------------------------------------------
-- Legacy torus code (kept as utility)
------------------------------------------------------------------------

-- | Construct a classical code on an l x m torus from a 3x3 stabilizer.
-- Each position (i,j) on the torus generates a check by cyclically
-- shifting the 3x3 pattern to position (i,j).
-- Returns the parity check matrix H.
torusCode :: Int -> Int -> Stabilizer3x3 -> BinMatrix
torusCode l m stab = bmFromRows checkRows
  where
    checkRows =
      [ bvFromList [ if getStabBit ((r - i + l) `mod` l) ((c - j + m) `mod` m)
                     then GF2 True else GF2 False
                   | r <- [0 .. l - 1], c <- [0 .. m - 1] ]
      | i <- [0 .. l - 1], j <- [0 .. m - 1]
      ]

    getStabBit r c
      | r < 3 && c < 3 = stab !! (3 * r + c)
      | otherwise       = False
