-- | Ordered Statistics Decoding (OSD) post-processor.
--
-- OSD-0 sorts columns by soft-output reliability, performs Gaussian
-- elimination on the most reliable columns, and back-substitutes.
-- Combined with BP as BP+OSD: run BP first; if it fails, pass
-- soft output to OSD.
module QEC.Decoder.OSD
  ( osdDecode
  , bpOsdDecode
  ) where

import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import qualified Data.Vector.Unboxed as VU

import QEC.GF2
import QEC.GF2.Matrix
import QEC.GF2.Gauss (rref)
import QEC.Decoder.BP

------------------------------------------------------------------------
-- OSD-0
------------------------------------------------------------------------

-- | OSD-0 decoder. Takes a parity check matrix, syndrome, and
-- soft reliability values (|LLR| per variable). Returns a correction.
osdDecode :: BinMatrix -> BinVec -> VU.Vector Double -> BinVec
osdDecode h syndrome softOutput = correction
  where
    m = bmNumRows h
    n = bmNumCols h

    -- Sort columns by decreasing reliability (|LLR|)
    colOrder = map fst $ sortBy (comparing (Down . snd))
                 [ (j, abs (softOutput VU.! j)) | j <- [0 .. n - 1] ]

    -- Inverse permutation: invPerm[original_col] = position in sorted order
    invPerm = map snd $ sortBy (comparing fst) $ zip colOrder [0 :: Int ..]

    -- Permute columns of H according to colOrder
    -- Column j of hPerm = column colOrder[j] of H
    hPerm = bmFromRows
      [ bvFromList [ bmGetEntry h i (colOrder !! j) | j <- [0 .. n - 1] ]
      | i <- [0 .. m - 1]
      ]

    -- Augment [H_perm | syndrome] and RREF
    augmented = bmFromRows
      [ bvFromList ([ bmGetEntry hPerm i j | j <- [0 .. n - 1] ]
                    ++ [bvGetBit syndrome i])
      | i <- [0 .. m - 1]
      ]

    (rrefAug, pivots) = rref augmented

    -- Build solution in permuted column order:
    -- Pivot variables: determined by the augmented RREF
    -- Free variables: set to hard decision from soft output (most reliable = 0)
    pivotSet = [ j `elem` pivots | j <- [0 .. n - 1] ]

    -- Map: pivot column -> its row in RREF
    pivotRowMap = zip pivots [0 :: Int ..]
    lookupPivotRow col = case lookup col pivotRowMap of
      Just r  -> r
      Nothing -> error "lookupPivotRow: not a pivot"

    -- For each column in permuted order, compute the value
    permSolution = bvFromList
      [ if pivotSet !! j
        then -- Pivot variable: from RREF, x_j = rref[row, n] XOR sum of rref[row,k]*x_k for free k
             let row = lookupPivotRow j
                 rhs = bmGetEntry rrefAug row n  -- augmented column
                 -- XOR in contributions from free variables
                 freeContrib = foldl (+) (GF2 False)
                   [ bmGetEntry rrefAug row k * freeVal k
                   | k <- [0 .. n - 1]
                   , not (pivotSet !! k)
                   ]
             in rhs + freeContrib
        else freeVal j
      | j <- [0 .. n - 1]
      ]

    -- Free variable value: hard decision from soft output
    freeVal j = GF2 (softOutput VU.! (colOrder !! j) < 0)

    -- Undo column permutation: correction[colOrder[j]] = permSolution[j]
    correction = bvFromList
      [ bvGetBit permSolution (invPerm !! origCol)
      | origCol <- [0 .. n - 1]
      ]

------------------------------------------------------------------------
-- BP + OSD
------------------------------------------------------------------------

-- | Combined BP+OSD decoder.
-- Runs BP first. If BP converges, returns its result.
-- Otherwise, passes BP's soft output to OSD.
bpOsdDecode :: BPConfig -> BinMatrix -> BinVec -> VU.Vector Double -> BPResult
bpOsdDecode config h syndrome channelLLR =
  let bpResult = bpDecode config h syndrome channelLLR
  in if bpConverged bpResult
     then bpResult
     else let softOut = bpSoftOutput bpResult
              corr = osdDecode h syndrome softOut
          in BPResult
               { bpCorrection = corr
               , bpConverged  = True
               , bpIterations = bpIterations bpResult
               , bpSoftOutput = softOut
               }
