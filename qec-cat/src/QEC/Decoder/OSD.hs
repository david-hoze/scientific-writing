-- | Ordered Statistics Decoding (OSD) post-processor.
--
-- OSD-0 sorts columns by soft-output reliability, performs Gaussian
-- elimination on the most reliable columns, and back-substitutes.
-- OSD-w additionally tries flipping up to w free variables and picks
-- the minimum weight correction.
-- Combined with BP as BP+OSD: run BP first; if it fails, pass
-- soft output to OSD.
module QEC.Decoder.OSD
  ( osdDecode
  , bpOsdDecode
  ) where

import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V

import QEC.GF2
import QEC.GF2.Matrix
import QEC.GF2.Gauss (rref)
import QEC.Decoder.BP

------------------------------------------------------------------------
-- OSD-w
------------------------------------------------------------------------

-- | OSD decoder with configurable order.
-- Order 0: classic OSD-0 (hard decision on free variables).
-- Order w>0: additionally tries flipping up to w free variables
-- and returns the minimum weight correction.
osdDecode :: Int -> BinMatrix -> BinVec -> VU.Vector Double -> BinVec
osdDecode order h syndrome softOutput = bestCorrection
  where
    m = bmNumRows h
    n = bmNumCols h

    -- Sort columns by decreasing reliability (|LLR|)
    colOrder = map fst $ sortBy (comparing (Down . snd))
                 [ (j, abs (softOutput VU.! j)) | j <- [0 .. n - 1] ]

    -- Inverse permutation as vector for O(1) lookup
    invPermV = V.replicate n 0 V.// zip colOrder [0 :: Int ..]

    -- Permute columns of H according to colOrder
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

    -- Identify pivot and free columns
    pivotSet = VU.generate n (\j -> j `elem` pivots)
    freeIndices = [ j | j <- [0 .. n - 1], not (pivotSet VU.! j) ]
    numFree = length freeIndices

    -- Map: pivot column -> its row in RREF (as vector for O(1) lookup)
    pivotRowV = V.replicate n (-1) V.// zip pivots [0 :: Int ..]

    -- Free variable base values: hard decision from soft output
    baseFreeVal j = softOutput VU.! (colOrder !! j) < 0

    -- Build the base correction (OSD-0) in permuted column space
    basePermSolution = bvFromList
      [ if pivotSet VU.! j
        then let row = pivotRowV V.! j
                 rhs = bmGetEntry rrefAug row n
                 freeContrib = foldl (+) (GF2 False)
                   [ bmGetEntry rrefAug row k * GF2 (baseFreeVal k)
                   | k <- freeIndices
                   ]
             in rhs + freeContrib
        else GF2 (baseFreeVal j)
      | j <- [0 .. n - 1]
      ]

    -- Undo column permutation
    unpermute permSol = bvFromList
      [ bvGetBit permSol (invPermV V.! origCol)
      | origCol <- [0 .. n - 1]
      ]

    baseCorr = unpermute basePermSolution

    -- For order > 0: precompute delta vectors.
    -- Flipping free variable f changes the permuted solution at:
    --   - position f (the free variable itself)
    --   - each pivot j where rrefAug[row_j][f] = GF2 True
    deltas = V.fromList
      [ unpermute $ bvFromList
          [ if j == f
            then GF2 True
            else if pivotSet VU.! j
                 then bmGetEntry rrefAug (pivotRowV V.! j) f
                 else GF2 False
          | j <- [0 .. n - 1]
          ]
      | f <- freeIndices
      ]

    bestCorrection
      | order <= 0 || numFree == 0 = baseCorr
      | otherwise = bestOfAll

    -- Try all subsets of size 1..min(order, numFree) and pick minimum weight
    bestOfAll = foldl pickLighter baseCorr candidates
      where
        candidates = concatMap subsetCorrections [1 .. min order numFree]

    subsetCorrections 1 =
      [ bvXor baseCorr (deltas V.! i)
      | i <- [0 .. numFree - 1]
      ]
    subsetCorrections 2 =
      [ bvXor (bvXor baseCorr (deltas V.! i)) (deltas V.! j)
      | i <- [0 .. numFree - 2]
      , j <- [i + 1 .. numFree - 1]
      ]
    subsetCorrections 3 =
      [ bvXor (bvXor (bvXor baseCorr (deltas V.! i)) (deltas V.! j)) (deltas V.! k)
      | i <- [0 .. numFree - 3]
      , j <- [i + 1 .. numFree - 2]
      , k <- [j + 1 .. numFree - 1]
      ]
    subsetCorrections w = map applyFlips (choose w [0 .. numFree - 1])
      where
        applyFlips idxs = foldl (\acc i -> bvXor acc (deltas V.! i)) baseCorr idxs
        choose 0 _      = [[]]
        choose _ []     = []
        choose k' (x:xs) = map (x:) (choose (k'-1) xs) ++ choose k' xs

    pickLighter best candidate
      | bvWeight candidate < bvWeight best = candidate
      | otherwise = best

------------------------------------------------------------------------
-- BP + OSD
------------------------------------------------------------------------

-- | Combined BP+OSD decoder.
-- Runs BP first. If BP converges, returns its result.
-- Otherwise, passes BP's soft output to OSD with the configured order.
bpOsdDecode :: BPConfig -> BinMatrix -> BinVec -> VU.Vector Double -> BPResult
bpOsdDecode config h syndrome channelLLR =
  let bpResult = bpDecode config h syndrome channelLLR
  in if bpConverged bpResult
     then bpResult
     else let softOut = bpSoftOutput bpResult
              corr = osdDecode (bpOsdOrder config) h syndrome softOut
          in BPResult
               { bpCorrection = corr
               , bpConverged  = True
               , bpIterations = bpIterations bpResult
               , bpSoftOutput = softOut
               }
