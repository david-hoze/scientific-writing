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
  emptyDomainNodes : Nat

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
||| Empty domain on either side → FullyIncompat (node has no valid assignment).
classifyEdge : SubCube -> SubCube -> List Formula -> List Formula -> EdgeClass
classifyEdge sci scj domI domJ =
  if length domI == 0 || length domJ == 0 then FullyIncompat
  else let canI = overlapCanonicals sci scj domI
           canJ = overlapCanonicals scj sci domJ
           compatPairs = countCompatPairs canI canJ
           totalPairs = length domI * length domJ
       in if compatPairs == totalPairs then FullyCompat
          else if compatPairs == 0 then FullyIncompat
          else PartialCompat

||| Build and analyze the compatibility CSP (structural only).
export
buildCSP : (n : Nat) -> (d : Nat) -> (maxSize : Nat) -> CSPResult
buildCSP n d maxSize =
  let scs = allSubCubes n d
      edges = structuralEdges scs
  in MkCSPResult (length scs) (length edges) 0 0 0 0

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

||| Semantic edge classification: compatible iff sub-functions agree on overlap.
||| All formulas in a domain compute the same function, so any representative
||| suffices. This gives the clean binary split (FullyCompat / FullyIncompat).
classifyEdgeSemantic : SubCube -> SubCube -> List Formula -> List Formula -> EdgeClass
classifyEdgeSemantic sci scj domI domJ =
  if length domI == 0 || length domJ == 0 then FullyIncompat
  else let od = overlapDim sci scj
           ttI : Bits32 = case domI of
                   (f :: _) => truthTable od (overlapRestrict sci scj f)
                   [] => the Bits32 0
           ttJ : Bits32 = case domJ of
                   (g :: _) => truthTable od (overlapRestrict scj sci g)
                   [] => the Bits32 0
       in if ttI == ttJ then FullyCompat else FullyIncompat

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

classifyOneEdgeSemantic : List SubCube -> List (List Formula) -> (Nat, Nat) -> EdgeClass
classifyOneEdgeSemantic scs domains (i, j) =
  let sci = orDefault (MkSubCube [] []) (indexList i scs)
      scj = orDefault (MkSubCube [] []) (indexList j scs)
      domI = orDefault [] (indexList i domains)
      domJ = orDefault [] (indexList j domains)
  in classifyEdgeSemantic sci scj domI domJ

||| Build and analyze the CSP for a specific n-variable function (given as truth table).
||| Uses structural (DAG-isomorphism) compatibility.
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
      ed = length (filter (\dom => length dom == 0) domains)
  in MkCSPResult (length scs) (length edges) fc pc fi ed

||| Build and analyze the CSP using semantic (function-level) compatibility.
||| All formulas in a domain compute the same function, so overlap agreement
||| is binary: either the sub-functions agree or they don't.
||| Result has 0 partially compatible edges.
export
buildCSPSemantic : (n : Nat) -> (d : Nat) -> (maxSize : Nat) -> Bits32 -> CSPResult
buildCSPSemantic n d maxSize targetTT =
  let subRes = enumerate d maxSize
      scs = allSubCubes n d
      edges = structuralEdges scs
      domains = map (getDomain n targetTT subRes) scs
      classifications = map (classifyOneEdgeSemantic scs domains) edges
      fc = length (filter isFullyCompat classifications)
      pc = length (filter isPartialCompat classifications)
      fi = length (filter isFullyIncompat classifications)
      ed = length (filter (\dom => length dom == 0) domains)
  in MkCSPResult (length scs) (length edges) fc pc fi ed

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
  let indexed = zip (case length formulas of Z => []; S k => [0 .. k]) formulas
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
      idxs : List Nat = case length scs of Z => []; S k => [0 .. k]
      nodes = zipWith (\i, dom => (i, map canonical dom)) idxs domains
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
      ed = length (filter (\dom => length dom == 0) domains)
      res = MkCSPResult (length scs) (length edges) fc pc fi ed
  in MkCSPData nodes edgeGrps res

||| Extract a sub-instance: keep only nodes in the given index set,
||| and edges between them. Re-indexes domain elements within each node.
export
extractSubInstance : List Nat -> CSPData -> CSPData
extractSubInstance keep cspData =
  let keepSet = SortedSet.fromList keep
      nodes' = filter (\(i, _) => contains i keepSet) (cspNodes cspData)
      edges' = filter (\eg => contains (edgeI eg) keepSet
                           && contains (edgeJ eg) keepSet) (cspEdgeGroups cspData)
      nNodes = length nodes'
      nEdges = length edges'
  in MkCSPData nodes' edges' (MkCSPResult nNodes nEdges 0 0 0 0)

||| Find nodes with the largest domains (most interesting for NS degree).
export
topNodesByDomain : Nat -> CSPData -> List Nat
topNodesByDomain k cspData =
  let withSizes = map (\(i, dom) => (length dom, i)) (cspNodes cspData)
      sorted = sortBy (\(a, _), (b, _) => compare b a) withSizes
  in map snd (take k sorted)

||| BFS-expand from a seed node along edges, skipping empty-domain nodes.
||| Returns up to maxNodes node indices (all with non-empty domains).
export
bfsExpand : Nat -> Nat -> CSPData -> List Nat
bfsExpand seed maxNodes cspData =
  let allEdges = cspEdgeGroups cspData
      emptySet : SortedSet Nat = SortedSet.fromList (map (\(i, _) => i) (filter (\(_, dom) => length dom == 0) (cspNodes cspData)))
  in go [seed] (SortedSet.insert seed SortedSet.empty) maxNodes allEdges emptySet
  where
    neighbors : Nat -> List CSPEdgeGroups -> SortedSet Nat -> SortedSet Nat -> List Nat
    neighbors node edges visited empty =
      concatMap (\eg =>
        let other = if edgeI eg == node then edgeJ eg
                    else if edgeJ eg == node then edgeI eg
                    else node
        in if (edgeI eg == node || edgeJ eg == node)
              && not (contains other visited)
              && not (contains other empty)
           then [other] else []
      ) edges

    go : List Nat -> SortedSet Nat -> Nat -> List CSPEdgeGroups -> SortedSet Nat -> List Nat
    go [] visited _ _ _ = SortedSet.toList visited
    go _ visited 0 _ _ = SortedSet.toList visited
    go (q :: qs) visited remaining edges empty =
      let nbrs = neighbors q edges visited empty
          toAdd = take remaining nbrs
          visited' = foldl (\s, n => SortedSet.insert n s) visited toAdd
      in go (qs ++ toAdd) visited' (minus remaining (length toAdd)) edges empty
