module Analysis.RestrictionImage

import Circuit.Formula
import Circuit.Canonical
import Circuit.Restriction
import Circuit.Enumerate
import Data.SortedMap
import Data.SortedSet
import Data.List

%default covering

||| Restriction image: for a set of formulas and direction (i,v),
||| the set of canonical strings of restrict(F, i, v).
restrictionImage : (i : Nat) -> (v : Bool) -> List Formula -> SortedSet String
restrictionImage i v formulas =
  foldl (\acc, f => insert (canonical (restrict f i v)) acc) empty formulas

||| All directions for dimension d: (i, v) for i in 0..d-1, v in {False, True}.
directions : (d : Nat) -> List (Nat, Bool)
directions d = concatMap (\i => [(i, False), (i, True)]) [0 .. minus d 1]

||| Result of restriction image analysis.
public export
record RIResult where
  constructor MkRIResult
  ||| Size of the restriction universe |U(s,d)|
  universeSize : Nat
  ||| Max restriction image size M(s,d)
  maxImage : Nat
  ||| sigma(s,d) = |U| / M
  sigma : Double
  ||| Top share alpha = M / |U|
  topShare : Double

||| Compute restriction image analysis for a given enumeration result.
||| Iterates over all (function, direction) pairs, computing restriction images.
export
analyzeRestrictions : (d : Nat) -> EnumResult -> RIResult
analyzeRestrictions d res =
  let funcs = SortedMap.toList (byTruthTable res)
      dirs = directions d
      (universe, maxImg) = foldl (processFunc dirs) (empty, 0) funcs
      uSize = length (SortedSet.toList universe)
      sigma = if maxImg == 0 then 0.0
              else cast uSize / cast maxImg
      alpha = if uSize == 0 then 0.0
              else cast maxImg / cast uSize
  in MkRIResult uSize maxImg sigma alpha
  where
    processDir : List Formula -> (SortedSet String, Nat) -> (Nat, Bool) -> (SortedSet String, Nat)
    processDir formulas (univ, maxI) (i, v) =
      let img = restrictionImage i v formulas
          imgList = SortedSet.toList img
          imgSize = length imgList
          univ' = foldl (\u, s => insert s u) univ imgList
      in (univ', max maxI imgSize)

    processFunc : List (Nat, Bool) -> (SortedSet String, Nat) -> (Bits32, List Formula) -> (SortedSet String, Nat)
    processFunc dirs acc (_, formulas) = foldl (processDir formulas) acc dirs

||| Compute restriction analysis at each size level from 0 to maxSize.
||| Returns a list of (size, RIResult) pairs for convergence analysis.
export
analyzeBySize : (d : Nat) -> (maxSize : Nat) -> List (Nat, RIResult)
analyzeBySize d maxSize =
  let res = enumerate d maxSize
      sizes = [0 .. maxSize]
  in map (\s => (s, analyzeAtSize d s res)) sizes
  where
    ||| Filter formulas to those with size <= s.
    filterBySize : Nat -> List Formula -> List Formula
    filterBySize s = filter (\f => size f <= s)

    ||| Build a restricted EnumResult containing only formulas up to size s.
    restrictToSize : Nat -> EnumResult -> EnumResult
    restrictToSize s res =
      let funcs = SortedMap.toList (byTruthTable res)
          filtered = map (\(tt, fs) => (tt, filterBySize s fs)) funcs
          nonEmpty = filter (\(_, fs) => not (isNil fs)) filtered
          byTT' = foldl (\m, (tt, fs) => insert tt fs m) (the (SortedMap Bits32 (List Formula)) empty) nonEmpty
      in { byTruthTable := byTT' } res

    analyzeAtSize : (d : Nat) -> Nat -> EnumResult -> RIResult
    analyzeAtSize d s res = analyzeRestrictions d (restrictToSize s res)
