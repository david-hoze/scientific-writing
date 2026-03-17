module Analysis.AffineRestriction

import Circuit.Formula
import Circuit.Canonical
import Circuit.Restriction
import Circuit.Enumerate
import Data.SortedMap
import Data.SortedSet
import Data.List

%default covering

--- Restriction modes and directions ---

||| Restriction mode: value-only or value + affine projections.
public export
data RestrictionMode = ValueMode | AffineMode

||| A restriction direction.
||| ValDir i v: fix variable i to value v.
||| ProjDir src dst neg: substitute x_src with x_dst (or NOT x_dst if neg).
public export
data Direction = ValDir Nat Bool | ProjDir Nat Nat Bool

valDirsFor : Nat -> List Direction
valDirsFor i = [ValDir i False, ValDir i True]

projDirsForPair : Nat -> Nat -> List Direction
projDirsForPair src dst =
  if src == dst then []
  else [ProjDir src dst False, ProjDir src dst True]

projDirsForSrc : Nat -> Nat -> List Direction
projDirsForSrc d src = concatMap (projDirsForPair src) [0 .. minus d 1]

||| All directions for a given dimension and mode.
export
allDirections : Nat -> RestrictionMode -> List Direction
allDirections d ValueMode =
  concatMap valDirsFor [0 .. minus d 1]
allDirections d AffineMode =
  let valDirs = concatMap valDirsFor [0 .. minus d 1]
      projDirs = concatMap (projDirsForSrc d) [0 .. minus d 1]
  in valDirs ++ projDirs

||| Apply a direction to a formula, producing a (d-1)-var formula.
export
applyDirection : Formula -> Direction -> Formula
applyDirection f (ValDir i v) = restrict f i v
applyDirection f (ProjDir src dst neg) =
  let dstAdj = if dst < src then dst else minus dst 1
      replacement = if neg then Not (Input dstAdj) else Input dstAdj
  in propagate (substituteFormula f src replacement)

--- Sigma computation ---

||| Result of sigma computation.
public export
record SigmaResult where
  constructor MkSigmaResult
  srFormulas : Nat
  srFunctions : Nat
  srUniverseSize : Nat
  srMaxImage : Nat
  srSigma : Double

||| Compute sigma for a given dimension, max formula size, and restriction mode.
export
computeSigma : Nat -> Nat -> RestrictionMode -> SigmaResult
computeSigma d maxSize mode =
  let res = enumerate d maxSize
      dirs = allDirections d mode
      funcs = SortedMap.toList (byTruthTable res)
      (universe, maxImg) = foldl (processFunc dirs) (SortedSet.empty, the Nat 0) funcs
      uSize = length (SortedSet.toList universe)
      sig = if maxImg == 0 then 0.0 else cast uSize / cast maxImg
  in MkSigmaResult (totalCount res) (functionsCovered res) uSize maxImg sig
  where
    processDirForFunc : List Direction -> List Formula -> (SortedSet String, Nat) -> Direction -> (SortedSet String, Nat)
    processDirForFunc dirs formulas (univ, maxI) dir =
      let img = foldl (\s, f => SortedSet.insert (canonical (applyDirection f dir)) s)
                  SortedSet.empty formulas
          imgList = SortedSet.toList img
          imgSize = length imgList
          univ' = foldl (\u, cs => SortedSet.insert cs u) univ imgList
      in (univ', max maxI imgSize)

    processFunc : List Direction -> (SortedSet String, Nat) -> (Bits32, List Formula) -> (SortedSet String, Nat)
    processFunc dirs acc (_, formulas) = foldl (processDirForFunc dirs formulas) acc dirs

||| Compute sigma at each cumulative size level.
export
computeSigmaBySize : Nat -> Nat -> RestrictionMode -> List (Nat, SigmaResult)
computeSigmaBySize d maxSize mode =
  map (\s => (s, computeSigma d s mode)) [0 .. maxSize]

||| Compute both value and affine sigma for comparison.
export
computeSigmaBoth : Nat -> Nat -> (SigmaResult, SigmaResult)
computeSigmaBoth d maxSize = (computeSigma d maxSize ValueMode, computeSigma d maxSize AffineMode)

--- Per-function analysis ---

||| Per-function restriction statistics.
public export
record FuncRestrictionInfo where
  constructor MkFuncRestrictionInfo
  friTT : Bits32
  friTg : Nat
  friMaxDirImage : Nat
  friUnionImage : Nat
  friShare : Double

||| Compute per-function restriction image statistics.
export
perFunctionStats : Nat -> Nat -> RestrictionMode -> List FuncRestrictionInfo
perFunctionStats d maxSize mode =
  let res = enumerate d maxSize
      dirs = allDirections d mode
      funcs = SortedMap.toList (byTruthTable res)
      universe = computeUniverse dirs funcs
      uSize = length (SortedSet.toList universe)
  in map (analyzeFn dirs uSize) funcs
  where
    addFormulasForDir : List Formula -> Direction -> SortedSet String -> SortedSet String
    addFormulasForDir formulas dir u =
      foldl (\u2, f => SortedSet.insert (canonical (applyDirection f dir)) u2) u formulas

    addFuncToUniverse : List Direction -> SortedSet String -> (Bits32, List Formula) -> SortedSet String
    addFuncToUniverse dirs univ (_, formulas) =
      foldl (\u, dir => addFormulasForDir formulas dir u) univ dirs

    computeUniverse : List Direction -> List (Bits32, List Formula) -> SortedSet String
    computeUniverse dirs funcs = foldl (addFuncToUniverse dirs) SortedSet.empty funcs

    dirImage : List Formula -> Direction -> SortedSet String
    dirImage formulas dir =
      foldl (\s, f => SortedSet.insert (canonical (applyDirection f dir)) s)
        SortedSet.empty formulas

    accumDir : List Formula -> (Nat, SortedSet String) -> Direction -> (Nat, SortedSet String)
    accumDir formulas (mx, uni) dir =
      let img = dirImage formulas dir
          sz = length (SortedSet.toList img)
      in (max mx sz, SortedSet.union uni img)

    analyzeFn : List Direction -> Nat -> (Bits32, List Formula) -> FuncRestrictionInfo
    analyzeFn dirs uSize (tt, formulas) =
      let (maxDir, unionImg) = foldl (accumDir formulas) (the Nat 0, SortedSet.empty) dirs
          tg = length formulas
          share = if uSize == 0 then 0.0 else cast maxDir / cast uSize
      in MkFuncRestrictionInfo tt tg maxDir (length (SortedSet.toList unionImg)) share
