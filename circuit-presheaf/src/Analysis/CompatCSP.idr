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

||| Get formulas from the enumeration that comxxxxxxxxxven truth table.
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

export
indexList : Nat -> List a -> Maybe a
indexList _ [] = Nothing
indexList 0 (x :: _) = Just x
indexList (S k) (_ :: xs) = indexList k xs

export
orDefault : a -> Maybe a -> a
orDefault def Nothing = def
orDefault _ (Just x) = x

export
getDomain : (n : Nat) -> Bits32 -> EnumResult -> SubCube -> List Formula
getDomain n targetTT subRes sc = formulasForTT (subFunction n targetTT sc) subRes

||| Get domain deduplicated by canonical string: one representative per canonical class.
||| This dramatically reduces domain sizes (median 2x compression per the test vectors).
getDomainDedup : (n : Nat) -> Bits32 -> EnumResult -> SubCube -> List Formula
getDomainDedup n targetTT subRes sc =
  let allFs = formulasForTT (subFunction n targetTT sc) subRes
  in snd (foldl (\(seen, acc), f =>
    let cs = canonical f
    in if contains cs seen then (seen, acc) else (SortedSet.insert cs seen, f :: acc)
  ) (SortedSet.empty, []) allFs)

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

||| Canonical group: maps overlap canonical form -> list of domain indices.
||| O(|D|) storage instead of O(|D|^2) for all pairs.
public export
CanonGroups : Type
CanonGroups = SortedMap String (List Nat)

||| CSP edge data: canonical groups for each side of an edge.
||| Incompatible pairs are derived at M2 generation time: cross-product
||| of indices from DIFFERENT canonical groups.
public export
record CSPEdgeGroups where
  constructor MkCSPEdgeGroups
  edgeI : Nat
  edgeJ : Nat
  groupsI : CanonGroups
  groupsJ : CanonGroups

||| CSP data for M2 generation: nodes with canonical domains,
||| edges with canonical group info (compact representation).
public export
record CSPData where
  constructor MkCSPData
  cspNodes : List (Nat, List String)
  cspEdgeGroups : List CSPEdgeGroups
  cspResult : CSPResult

||| Build canonical groups: map each domain element's index to its
||| overlap canonical form. O(|D|) per edge side.
export
overlapGroups : SubCube -> SubCube -> List Formula -> CanonGroups
overlapGroups scSelf scOther formulas =
  let indexed = zip [0 .. minus (length formulas) 1] formulas
  in foldl (\m, (idx, f) =>
    let cs = canonical (overlapRestrict scSelf scOther f)
    in case lookup cs m of
         Nothing => insert cs [idx] m
         Just ids => insert cs (idx :: ids) m
  ) empty indexed

||| Build a filtered EnumResult containing only formulas whose truth tables
||| are in the given set. This avoids keeping all 36K+ ASTs in memory.
filterTT : SortedSet Bits32 -> SortedMap Bits32 (List Formula) -> SortedMap Bits32 (List Formula)
filterTT needed = foldl addIfNeeded empty . SortedMap.toList
  where
    addIfNeeded : SortedMap Bits32 (List Formula) -> (Bits32, List Formula) -> SortedMap Bits32 (List Formula)
    addIfNeeded m (tt, fs) = if contains tt needed then insert tt fs m else m

filterEnum : SortedSet Bits32 -> EnumResult -> EnumResult
filterEnum needed res =
  MkEnumResult empty (seen res) (filterTT needed (byTruthTable res)) (totalCount res)

||| Build the full CSP data for M2 generation.
||| Filters enumeration results to only keep formulas for needed truth tables.
export
buildCSPData : (n : Nat) -> (d : Nat) -> (maxSize : Nat) -> Bits32 -> CSPData
buildCSPData n d maxSize targetTT =
  let scs = allSubCubes n d
      -- Compute which sub-function truth tables we actually need
      neededTTs = foldl (\s, sc => SortedSet.insert (subFunction n targetTT sc) s)
                    SortedSet.empty scs
      -- Enumerate and immediately filter to only needed truth tables
      fullRes = enumerate d maxSize
      subRes = filterEnum neededTTs fullRes
      edges = structuralEdges scs
      domains = map (getDomainDedup n targetTT subRes) scs
      -- Build nodes: (index, canonical domain strings)
      nodes = zipWith (\i, dom => (i, map canonical dom))
                [0 .. minus (length scs) 1] domains
      -- Build edge groups (O(|D|) per edge, not O(|D|^2))
      edgeGrps = map (\(i, j) =>
        let sci = orDefault (MkSubCube [] []) (indexList i scs)
            scj = orDefault (MkSubCube [] []) (indexList j scs)
            domI = orDefault [] (indexList i domains)
            domJ = orDefault [] (indexList j domains)
        in MkCSPEdgeGroups i j
             (overlapGroups sci scj domI)
             (overlapGroups scj sci domJ)) edges
      -- Comxxxxxxxassification counts
      classifications = map (classifyOneEdge scs domains) edges
      fc = length (filter isFullyCompat classifications)
      pc = length (filter isPartialCompat classifications)
      fi = length (filter isFullyIncompat classifications)
      res = MkCSPResult (length scs) (length edges) fc pc fi
  in MkCSPData nodes edgeGrps res
