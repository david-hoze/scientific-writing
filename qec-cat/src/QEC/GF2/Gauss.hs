-- | Gaussian elimination over GF(2).
--
-- Provides reduced row echelon form ('rref'), 'rank', 'kernel' (null space),
-- and linear system solving ('solveLinear'). The RREF computation uses
-- in-place mutable Word64 XOR inside 'ST' for performance.
module QEC.GF2.Gauss
  ( rref
  , rank
  , kernel
  , solveLinear
  , pivotColumns
  ) where

import Control.Monad (when, forM_)
import Control.Monad.ST (runST)
import Data.Bits
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import QEC.GF2
import QEC.GF2.Matrix

------------------------------------------------------------------------
-- RREF
------------------------------------------------------------------------

-- | Reduced row echelon form over GF(2).
-- Returns @(rref_matrix, pivot_columns)@.
--
-- For each column left-to-right: find a pivot row at or below current,
-- swap to current position, XOR into all other rows with a 1 in that
-- column. Complexity: O(m * n * ceil(n\/64)).
rref :: BinMatrix -> (BinMatrix, [Int])
rref mat = runST $ do
    mv <- VU.thaw (bmData mat)
    let
      getBit i j = do
        let (wi, bi) = j `quotRem` 64
        w <- VUM.read mv (i * wpr + wi)
        return (testBit w bi)

      addRow dst src =
        forM_ [0 .. wpr - 1] $ \k -> do
          dv <- VUM.read mv (dst * wpr + k)
          sv <- VUM.read mv (src * wpr + k)
          VUM.write mv (dst * wpr + k) (xor dv sv)

      swapRow i j = when (i /= j) $
        forM_ [0 .. wpr - 1] $ \k -> do
          let iIdx = i * wpr + k
              jIdx = j * wpr + k
          iv <- VUM.read mv iIdx
          jv <- VUM.read mv jIdx
          VUM.write mv iIdx jv
          VUM.write mv jIdx iv

      findPivot curRow col = go curRow
        where
          go r
            | r >= m    = return Nothing
            | otherwise = do
                b <- getBit r col
                if b then return (Just r) else go (r + 1)

      loop !curRow !col !pivAcc
        | col >= n || curRow >= m = return (reverse pivAcc)
        | otherwise = do
            maybePivot <- findPivot curRow col
            case maybePivot of
              Nothing -> loop curRow (col + 1) pivAcc
              Just pr -> do
                swapRow curRow pr
                forM_ [0 .. m - 1] $ \row ->
                  when (row /= curRow) $ do
                    b <- getBit row col
                    when b $ addRow row curRow
                loop (curRow + 1) (col + 1) (col : pivAcc)

    pivs <- loop 0 0 []
    dat' <- VU.freeze mv
    return (BinMatrix m n wpr dat', pivs)
  where
    m   = bmNumRows mat
    n   = bmNumCols mat
    wpr = bmWordsPerRow mat

------------------------------------------------------------------------
-- Rank
------------------------------------------------------------------------

-- | Rank of a binary matrix = number of pivot columns in RREF.
rank :: BinMatrix -> Int
rank = length . snd . rref

------------------------------------------------------------------------
-- Pivot columns
------------------------------------------------------------------------

-- | Pivot column indices from RREF.
pivotColumns :: BinMatrix -> [Int]
pivotColumns = snd . rref

------------------------------------------------------------------------
-- Kernel (null space)
------------------------------------------------------------------------

-- | Kernel (null space) of A: all x such that Ax = 0.
--
-- From RREF, free variables correspond to non-pivot columns.
-- For each free variable, construct a basis vector by reading
-- off the RREF entries.
-- Returns a list of basis vectors. Length = n - rank(A).
kernel :: BinMatrix -> [BinVec]
kernel mat = map buildKernelVec freeVars
  where
    (rrefMat, pivots) = rref mat
    n = bmNumCols mat
    pivotSet = VU.replicate n False VU.// [(p, True) | p <- pivots]
    freeVars = [ j | j <- [0 .. n - 1], not (pivotSet VU.! j) ]

    pivotToRow :: VU.Vector Int
    pivotToRow = VU.replicate n (-1) VU.// zip pivots [0 :: Int ..]

    buildKernelVec freeCol = bvFromList
      [ if j == freeCol
        then GF2 True
        else if pivotSet VU.! j
          then bmGetEntry rrefMat (pivotToRow VU.! j) freeCol
          else GF2 False
      | j <- [0 .. n - 1]
      ]

------------------------------------------------------------------------
-- Solve linear system
------------------------------------------------------------------------

-- | Solve Ax = b over GF(2). Returns 'Nothing' if inconsistent,
-- @Just x@ for a particular solution otherwise.
solveLinear :: BinMatrix -> BinVec -> Maybe BinVec
solveLinear mat b
  | bvLength b /= m = error "solveLinear: dimension mismatch"
  | otherwise =
    let augmented = bmFromRows
          [ bvFromList (bvToList (bmGetRow mat i) ++ [bvGetBit b i])
          | i <- [0 .. m - 1]
          ]
        (rrefAug, pivots) = rref augmented

        inconsistent = n `elem` pivots

        pivotSet = VU.replicate (n + 1) False VU.// [(p, True) | p <- pivots]
        pivotToRow = VU.replicate (n + 1) (-1) VU.// zip pivots [0 :: Int ..]

        solution = bvFromList
          [ if pivotSet VU.! j
            then bmGetEntry rrefAug (pivotToRow VU.! j) n
            else GF2 False
          | j <- [0 .. n - 1]
          ]
    in if inconsistent then Nothing else Just solution
  where
    m = bmNumRows mat
    n = bmNumCols mat
