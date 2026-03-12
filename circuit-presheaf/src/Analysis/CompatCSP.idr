module Analysis.CompatCSP

import Circuit.Formula
import Circuit.Canonical
import Circuit.Restriction
import Circuit.Enumerate
import Analysis.SubCube
import Data.SortedMap
import Data.SortedSet
import Data.List
import Data.Bits

%default covering

||| CSP result for a given (n, d, maxSize) configuration.
public export
record CSPResult where
  constructor MkCSPResult
  nodeCount : Nat
  edgeCount : Nat
  fullyCompatible : Nat
  partiallyCompatible : Nat
  fullyIncompatible : Nat

||| Edge classification.
data EdgeClass = FullyCompat | PartialCompat | FullyIncompat

||| Get formulas from the enumeration that compute a given truth table.
formulasForTT : Bits32 -> EnumResult -> List Formula
formulasForTT tt res = case lookup tt (byTruthTable res) of
                         Nothing => []
                         Just fs => fs

||| Compute a multiset of overlap-restricted canonical forms.
||| Returns a SortedMap from canonical string to count.
overlapCanonicals : SubCube -> SubCube -> List Formula -> SortedMap String Nat
overlapCanonicals scSelf scOther formulas =
  foldl (\m, f =>
    let cs = canonical (overlapRestrict scSelf scOther f)
    in case lookup cs m of
         Nothing => insert cs 1 m
         Just n => insert cs (S n) m
  ) empty formulas

||| Count compatible pairs by matching canonical form multisets.
countCompatPairs : SortedMap String Nat -> SortedMap String Nat -> Nat
countCompatPairs canI canJ =
  foldl addMatches 0 (SortedMap.toList canI)
  where
    addMatches : Nat -> (String, Nat) -> Nat
    addMatches acc (k, ni) =
      case lookup k canJ of
        Nothing => acc
        Just nj => acc + ni * nj

||| Classify an edge using precomputed canonical form multisets.
||| O(|D_i| + |D_j|) instead of O(|D_i| * |D_j|).
classifyEdge : SubCube -> SubCube -> List Formula -> List Formula -> EdgeClass
classifyEdge sci scj domI domJ =
  let totalPairs = length domI * length domJ
  in if totalPairs == 0 then FullyCompat
     else let canI = overlapCanonicals sci scj domI
              canJ = overlapCanonicals scj sci domJ
              compatPairs = countCompatPairs canI canJ
          in if compatPairs == totalPairs then FullyCompat
             else if compatPairs == 0 then FullyIncompat
             else PartialCompat

||| Build and analyze the compatibility CSP (structural only).
export
buildCSP : (n : Nat) -> (d : Nat) -> (maxSize : Nat) -> CSPResult
buildCSP n d maxSize =
  let scs = allSubCubes n d
      edges = structuralEdges scs
  in MkCSPResult (length scs) (length edges) 0 0 0

indexList : Nat -> List a -> Maybe a
indexList _ [] = Nothing
indexList 0 (x :: _) = Just x
indexList (S k) (_ :: xs) = indexList k xs

orDefault : a -> Maybe a -> a
orDefault def Nothing = def
orDefault _ (Just x) = x

getDomain : (n : Nat) -> Bits32 -> EnumResult -> SubCube -> List Formula
getDomain n targetTT subRes sc = formulasForTT (subFunction n targetTT sc) subRes

classifyOneEdge : List SubCube -> List (List Formula) -> (Nat, Nat) -> EdgeClass
classifyOneEdge scs domains (i, j) =
  let sci = orDefault (MkSubCube [] []) (indexList i scs)
      scj = orDefault (MkSubCube [] []) (indexList j scs)
      domI = orDefault [] (indexList i domains)
      domJ = orDefault [] (indexList j domains)
  in classifyEdge sci scj domI domJ

isFullyCompat : EdgeClass -> Bool
isFullyCompat FullyCompat = True
isFullyCompat _ = False

isPartialCompat : EdgeClass -> Bool
isPartialCompat PartialCompat = True
isPartialCompat _ = False

isFullyIncompat : EdgeClass -> Bool
isFullyIncompat FullyIncompat = True
isFullyIncompat _ = False

||| Build and analyze the CSP for a specific n-variable function (given as truth table).
export
buildCSPForFunction : (n : Nat) -> (d : Nat) -> (maxSize : Nat) -> Bits32 -> CSPResult
buildCSPForFunction n d maxSize targetTT =
  let subRes = enumerate d maxSize
      scs = allSubCubes n d
      edges = structuralEdges scs
      domains = map (getDomain n targetTT subRes) scs
      classifications = map (classifyOneEdge scs domains) edges
      fc = length (filter isFullyCompat classifications)
      pc = length (filter isPartialCompat classifications)
      fi = length (filter isFullyIncompat classifications)
  in MkCSPResult (length scs) (length edges) fc pc fi
