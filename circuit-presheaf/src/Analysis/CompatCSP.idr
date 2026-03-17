module Analysis.CompatCSP

import Circuit.Formula
import Circuit.Canonical
import Circuit.Restriction
import Circuit.Enumerate
import Analysis.SubCube
import Data.SortedMap
import Data.SortedSet
import Data.List
import Data.List1
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

||| Build CSP data using a pre-computed EnumResult (avoids re-enumerating).
export
buildCSPDataWith : (n : Nat) -> (d : Nat) -> EnumResult -> Bits32 -> CSPData
buildCSPDataWith n d fullRes targetTT =
  let scs = allSubCubes n d
      neededTTs = foldl (\s, sc => SortedSet.insert (subFunction n targetTT sc) s)
                    SortedSet.empty scs
      subRes = filterEnum neededTTs fullRes
      edges = structuralEdges scs
      domains = map (getDomainDedup n targetTT subRes) scs
      idxs : List Nat = case length scs of Z => []; S k => [0 .. k]
      nodes = zipWith (\i, dom => (i, map canonical dom)) idxs domains
      edgeGrps = map (\(i, j) =>
        let sci = orDefault (MkSubCube [] []) (indexList i scs)
            scj = orDefault (MkSubCube [] []) (indexList j scs)
            domI = orDefault [] (indexList i domains)
            domJ = orDefault [] (indexList j domains)
        in MkCSPEdgeGroups i j
             (overlapGroups sci scj domI)
             (overlapGroups scj sci domJ)) edges
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

--- CSP Solver (backtracking with forward checking) ---

||| Solver result: SAT with a witness, or UNSAT.
public export
data SolveResult = SatResult (List (Nat, Nat))   -- (nodeIdx, domainIdx) pairs
                 | UnsatResult

export
Show SolveResult where
  show (SatResult assignments) = "SAT: " ++ show assignments
  show UnsatResult = "UNSAT"

export
isSat : SolveResult -> Bool
isSat (SatResult _) = True
isSat UnsatResult = False

||| For each edge and each domain element, store its overlap canonical key.
||| An EdgeKeyMap maps: domIdx -> canonical key (String).
||| Two elements are compatible on this edge iff they have the same key.
||| This is O(D) per edge to build, O(1) to check compatibility.
public export
record EdgeKeys where
  constructor MkEdgeKeys
  ekNodeI : Nat
  ekNodeJ : Nat
  keysI : SortedMap Nat String   -- domIdx at nodeI -> canonical key
  keysJ : SortedMap Nat String   -- domIdx at nodeJ -> canonical key

||| Build edge keys from canonical groups.
||| groupsI maps canonical_key -> [domIdx], so we invert to domIdx -> canonical_key.
export
invertGroups : CanonGroups -> SortedMap Nat String
invertGroups groups =
  foldl addEntries empty (SortedMap.toList groups)
  where
    addEntries : SortedMap Nat String -> (String, List Nat) -> SortedMap Nat String
    addEntries m (k, ids) = foldl (\m', idx => insert idx k m') m ids

export
mkEdgeKeys : CSPEdgeGroups -> EdgeKeys
mkEdgeKeys eg = MkEdgeKeys (edgeI eg) (edgeJ eg)
                  (invertGroups (groupsI eg)) (invertGroups (groupsJ eg))

||| Check if value valIdx at nodeIdx is compatible with the assigned value
||| at the other end of an edge. O(1) lookup per edge.
export
checkEdge : Nat -> Nat -> Nat -> Nat -> EdgeKeys -> Bool
checkEdge nodeIdx valIdx otherNode otherVal ek =
  if ekNodeI ek == nodeIdx && ekNodeJ ek == otherNode then
    case (lookup valIdx (keysI ek), lookup otherVal (keysJ ek)) of
      (Just k1, Just k2) => k1 == k2
      _ => False
  else if ekNodeJ ek == nodeIdx && ekNodeI ek == otherNode then
    case (lookup valIdx (keysJ ek), lookup otherVal (keysI ek)) of
      (Just k1, Just k2) => k1 == k2
      _ => False
  else True

||| Check consistency against all assigned neighbors.
||| Only checks edges involving this node where the other end is assigned.
export
isConsistent : Nat -> Nat -> SortedMap Nat Nat -> List EdgeKeys -> Bool
isConsistent nodeIdx valIdx assignment edges =
  all checkOneEdge edges
  where
    checkOneEdge : EdgeKeys -> Bool
    checkOneEdge ek =
      let otherNode = if ekNodeI ek == nodeIdx then ekNodeJ ek
                      else if ekNodeJ ek == nodeIdx then ekNodeI ek
                      else nodeIdx  -- not relevant
      in if otherNode == nodeIdx then True  -- edge doesn't involve this node
         else case lookup otherNode assignment of
                Nothing => True
                Just otherVal => checkEdge nodeIdx valIdx otherNode otherVal ek

||| Backtracking CSP solver with forward checking.
||| Fuel: maximum number of backtracks before giving up.
export
solveCSP : CSPData -> Nat -> SolveResult
solveCSP cspData fuel =
  let nonEmpty = filter (\(_, dom) => length dom > 0) (cspNodes cspData)
      sorted = sortBy (\(_, d1), (_, d2) => compare (length d1) (length d2)) nonEmpty
      nodeOrder = map (\(i, dom) => (i, length dom)) sorted
      edges = map mkEdgeKeys (cspEdgeGroups cspData)
  in go nodeOrder empty edges fuel
  where
    mutual
      go : List (Nat, Nat) -> SortedMap Nat Nat -> List EdgeKeys -> Nat -> SolveResult
      go [] assignment _ _ = SatResult (SortedMap.toList assignment)
      go _ _ _ 0 = UnsatResult
      go ((nodeIdx, domSize) :: rest) assignment edges remaining =
        tryValues nodeIdx 0 domSize rest assignment edges remaining

      tryValues : Nat -> Nat -> Nat -> List (Nat, Nat) -> SortedMap Nat Nat -> List EdgeKeys -> Nat -> SolveResult
      tryValues _ _ _ _ _ _ 0 = UnsatResult
      tryValues nodeIdx idx domSize rest assignment edges remaining =
        if idx >= domSize then UnsatResult
        else if isConsistent nodeIdx idx assignment edges
          then case go rest (insert nodeIdx idx assignment) edges (minus remaining 1) of
                 SatResult sol => SatResult sol
                 UnsatResult => tryValues nodeIdx (S idx) domSize rest assignment edges (minus remaining 1)
          else tryValues nodeIdx (S idx) domSize rest assignment edges remaining

--- Profile-based CSP reduction for NS degree ---

||| A profile at node i: maps edge list index → canonical key.
||| Elements with the same profile are completely interchangeable.
Profile : Type
Profile = SortedMap Nat String

profileToString : Profile -> String
profileToString p = fastConcat (intersperse "|" (map (\(k, v) => show k ++ ":" ++ v) (SortedMap.toList p)))

||| Reduced CSP node: profiles and their edge keys.
public export
record ProfileNode where
  constructor MkProfileNode
  pnNodeIdx : Nat
  pnProfiles : List (Nat, Profile)  -- (profileIdx, profile)
  pnOrigCount : Nat                  -- how many domain elements total

||| Reduced CSP: one variable per profile per node.
public export
record ReducedCSP where
  constructor MkReducedCSP
  rcNodes : List ProfileNode
  rcEdges : List (Nat, Nat, Nat)  -- (nodeI, nodeJ, edgeListIdx)
  rcTotalVars : Nat

||| Compute profiles for a single node.
||| Returns: map from profile string → (list of domain indices with that profile)
export
computeNodeProfiles : Nat -> Nat -> List CSPEdgeGroups -> SortedMap String (List Nat)
computeNodeProfiles nodeIdx domSize edgeGrps =
  let -- Build inverted maps for each edge involving this node
      edgeInverted : List (Nat, SortedMap Nat String)
      edgeInverted = buildEdgeInverted 0 edgeGrps
      -- For each domain element, compute its profile
      domIdxs = the (List Nat) (case domSize of Z => []; S k => [0 .. k])
  in foldl (\m, idx =>
    let prof = buildProfileStr idx edgeInverted
    in case lookup prof m of
         Nothing => insert prof [idx] m
         Just ids => insert prof (idx :: ids) m
  ) empty domIdxs
  where
    buildEdgeInverted : Nat -> List CSPEdgeGroups -> List (Nat, SortedMap Nat String)
    buildEdgeInverted _ [] = []
    buildEdgeInverted eidx (eg :: rest) =
      let invMap = if edgeI eg == nodeIdx then invertGroups (groupsI eg)
                   else if edgeJ eg == nodeIdx then invertGroups (groupsJ eg)
                   else empty
          tail = buildEdgeInverted (S eidx) rest
      in if length (SortedMap.toList invMap) > 0
         then (eidx, invMap) :: tail
         else tail

    buildProfileStr : Nat -> List (Nat, SortedMap Nat String) -> String
    buildProfileStr idx edges =
      let keys = map (\(eidx, m) =>
            let keyStr = case lookup idx m of
                           Just k => k
                           Nothing => "?"
            in show eidx ++ ":" ++ keyStr) edges
      in fastConcat (intersperse "|" keys)

||| Build a reduced CSP using profile-based compression.
export
reduceCSP : CSPData -> ReducedCSP
reduceCSP cspData =
  let edgeGrps = cspEdgeGroups cspData
      nodes = cspNodes cspData
      profileNodes = map (buildProfileNode edgeGrps) nodes
      edgeList = buildEdgeList 0 edgeGrps
      totalVars = foldl (\acc, pn => acc + length (pnProfiles pn)) (the Nat 0) profileNodes
  in MkReducedCSP profileNodes edgeList totalVars
  where
    buildProfileNode : List CSPEdgeGroups -> (Nat, List String) -> ProfileNode
    buildProfileNode egs (nodeIdx, dom) =
      let domSize = length dom
          profMap = computeNodeProfiles nodeIdx domSize egs
          profList = SortedMap.toList profMap
          idxs : List Nat = case length profList of Z => []; S k => [0 .. k]
          numbered = zip idxs profList
          profs = map (\(pidx, (_, _)) => (pidx, the Profile empty)) numbered
      in MkProfileNode nodeIdx profs domSize

    buildEdgeList : Nat -> List CSPEdgeGroups -> List (Nat, Nat, Nat)
    buildEdgeList _ [] = []
    buildEdgeList idx (eg :: rest) = (edgeI eg, edgeJ eg, idx) :: buildEdgeList (S idx) rest

||| Get the number of profiles per node.
export
profileCounts : ReducedCSP -> List (Nat, Nat)
profileCounts rcsp = map (\pn => (pnNodeIdx pn, length (pnProfiles pn))) (rcNodes rcsp)

||| Dump CSP data in a machine-readable text format for external processing.
||| Format:
|||   NODES <count>
|||   NODE <idx> <domSize>
|||   EDGE <i> <j>
|||   GI <canonicalKey> <idx1> <idx2> ...
|||   GJ <canonicalKey> <idx1> <idx2> ...
export
dumpCSP : CSPData -> String
dumpCSP cspData =
  let nodes = cspNodes cspData
      nodeLines = concatMap dumpNode nodes
      edgeLines = concatMap dumpEdge (cspEdgeGroups cspData)
  in "NODES " ++ show (length nodes) ++ "\n"
  ++ nodeLines ++ edgeLines
  where
    dumpNode : (Nat, List String) -> String
    dumpNode (idx, dom) = "NODE " ++ show idx ++ " " ++ show (length dom) ++ "\n"

    dumpGroups : String -> CanonGroups -> String
    dumpGroups pfx groups =
      concatMap (\(key, ids) =>
        pfx ++ " " ++ key ++ " " ++ fastConcat (intersperse " " (map show ids)) ++ "\n"
      ) (SortedMap.toList groups)

    dumpEdge : CSPEdgeGroups -> String
    dumpEdge eg =
      "EDGE " ++ show (edgeI eg) ++ " " ++ show (edgeJ eg) ++ "\n"
      ++ dumpGroups "GI" (groupsI eg)
      ++ dumpGroups "GJ" (groupsJ eg)
