module Circuit.Enumerate

import Circuit.Formula
import Circuit.Canonical
import Data.SortedMap
import Data.SortedSet
import Data.IORef
import Data.List

%default covering

||| Result of enumeration: formulas grouped by size,
||| and a map from truth table (Bits32) to list of formulas.
public export
record EnumResult where
  constructor MkEnumResult
  ||| Formulas grouped by exact size
  bySize : SortedMap Nat (List Formula)
  ||| All canonical strings seen (for dedup)
  seen : SortedSet String
  ||| Map from truth table to formulas that compute it
  byTruthTable : SortedMap Bits32 (List Formula)
  ||| Total formula count
  totalCount : Nat

||| Empty enumeration result.
emptyResult : EnumResult
emptyResult = MkEnumResult empty empty empty 0

||| Add a formula to the result if its canonical string is new.
||| Returns (updated result, True if formula was new).
addFormula : (d : Nat) -> Formula -> EnumResult -> (EnumResult, Bool)
addFormula d f res =
  let cs = canonical f
  in if contains cs (seen res)
     then (res, False)
     else let s = size f
              tt = truthTable d f
              bySize' = case lookup s (bySize res) of
                          Nothing => insert s [f] (bySize res)
                          Just fs => insert s (f :: fs) (bySize res)
              byTT' = case lookup tt (byTruthTable res) of
                        Nothing => insert tt [f] (byTruthTable res)
                        Just fs => insert tt (f :: fs) (byTruthTable res)
          in (MkEnumResult bySize' (insert cs (seen res)) byTT' (S (totalCount res)), True)

||| Generate all size-0 formulas (leaves): inputs x0..x_{d-1} and constants 0, 1.
leaves : (d : Nat) -> List Formula
leaves d = [Const False, Const True] ++ map Input [0 .. (minus d 1)]

||| Generate size-s formulas by combining smaller formulas.
||| NOT of size-(s-1), AND/OR of sizes s1 + s2 = s-1.
generateSize : (d : Nat) -> (s : Nat) -> EnumResult -> EnumResult
generateSize d s res =
  let -- NOT of size-(s-1) formulas
      notCandidates = case lookup (minus s 1) (bySize res) of
                        Nothing => []
                        Just fs => map Not fs
      -- AND and OR of sizes s1 + s2 = s - 1
      binCandidates = concatMap (binPairs (bySize res)) (splitPairs (minus s 1))
      allCandidates = notCandidates ++ binCandidates
  in foldl (\r, f => fst (addFormula d f r)) res allCandidates
  where
    ||| Generate (s1, s2) pairs where s1 + s2 = n and s1 <= s2.
    half : Nat -> Nat
    half Z = Z
    half (S Z) = Z
    half (S (S k)) = S (half k)

    splitPairs : Nat -> List (Nat, Nat)
    splitPairs n = map (\s1 => (s1, minus n s1)) [0 .. half n]

    ||| For same-size pairing, generate all ordered pairs including self.
    selfPairs : List Formula -> List Formula
    selfPairs [] = []
    selfPairs (f :: fs) =
      concatMap (\g => [And f g, Or f g]) (f :: fs) ++ selfPairs fs

    ||| Generate AND and OR combinations for a given (s1, s2) pair.
    binPairs : SortedMap Nat (List Formula) -> (Nat, Nat) -> List Formula
    binPairs byS (s1, s2) =
      let fs1 = case lookup s1 byS of Nothing => []; Just fs => fs
          fs2 = case lookup s2 byS of Nothing => []; Just fs => fs
      in if s1 == s2
         then selfPairs fs1
         else concatMap (\f1 => concatMap (\f2 => [And f1 f2, Or f1 f2]) fs2) fs1

||| Enumerate all formulas up to a given max size for dimension d.
||| Returns the enumeration result.
export
enumerate : (d : Nat) -> (maxSize : Nat) -> EnumResult
enumerate d maxSize =
  let -- Add leaves (size 0)
      res0 = foldl (\r, f => fst (addFormula d f r)) emptyResult (leaves d)
      -- Generate sizes 1 through maxSize
      -- Note: [1..0] for Nat counts down to [1,0], so guard explicitly
  in case maxSize of
       Z => res0
       S _ => foldl (\r, s => generateSize d s r) res0 [1 .. maxSize]

||| Count of formulas at each size level.
export
sizeCounts : EnumResult -> List (Nat, Nat)
sizeCounts res = map (\(s, fs) => (s, length fs)) (SortedMap.toList (bySize res))

||| Number of distinct functions covered.
export
functionsCovered : EnumResult -> Nat
functionsCovered res = length (SortedMap.toList (byTruthTable res))
