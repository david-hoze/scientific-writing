module Analysis.GraphTopology

import Analysis.CompatCSP
import Analysis.SubCube
import Data.SortedMap
import Data.SortedSet
import Data.List
import Data.Bits

%default covering

--- Constraint graph ---

||| A constraint graph extracted from CSP edge groups.
public export
record ConstraintGraph where
  constructor MkConstraintGraph
  cgNodes : SortedSet Nat
  cgEdges : List (Nat, Nat)
  cgAdj : SortedMap Nat (List Nat)

||| Extract the constraint (obstruction) subgraph: edges that are PC or FI.
||| An edge is FC iff all src keys match all dst keys.
||| An edge is FI iff no keys match.
||| Otherwise it's PC.
export
extractObstructionGraph : List CSPEdgeGroups -> ConstraintGraph
extractObstructionGraph egs =
  let classified = map classifyEG egs
      obstEdges = mapMaybe (\(eg, cls) =>
        if cls == "PC" || cls == "FI"
        then Just (edgeI eg, edgeJ eg)
        else Nothing) classified
      nodes = foldl (\s, (i, j) => SortedSet.insert j (SortedSet.insert i s))
                SortedSet.empty obstEdges
      adj = foldl (\m, (i, j) =>
        let m1 = case lookup i m of
                   Nothing => insert i [j] m
                   Just ns => insert i (j :: ns) m
            m2 = case lookup j m1 of
                   Nothing => insert j [i] m1
                   Just ns => insert j (i :: ns) m1
        in m2) (the (SortedMap Nat (List Nat)) empty) obstEdges
  in MkConstraintGraph nodes obstEdges adj
  where
    classifyEG : CSPEdgeGroups -> (CSPEdgeGroups, String)
    classifyEG eg =
      let srcKeys = SortedSet.fromList (map fst (SortedMap.toList (groupsI eg)))
          dstKeys = SortedSet.fromList (map fst (SortedMap.toList (groupsJ eg)))
          nSrc = length (SortedSet.toList srcKeys)
          nDst = length (SortedSet.toList dstKeys)
          common = length (filter (\k => SortedSet.contains k dstKeys) (SortedSet.toList srcKeys))
      in if nSrc == 0 || nDst == 0 then (eg, "empty")
         else if common == 0 then (eg, "FI")
         else if common == nSrc && common == nDst then (eg, "FC")
         else (eg, "PC")

||| Count connected components via BFS.
export
connectedComponents : ConstraintGraph -> Nat
connectedComponents g =
  let allNodes = SortedSet.toList (cgNodes g)
  in fst (foldl (\(count, visited), node =>
    if SortedSet.contains node visited then (count, visited)
    else (S count, bfs [node] visited (cgAdj g))
  ) (the Nat 0, SortedSet.empty) allNodes)
  where
    bfs : List Nat -> SortedSet Nat -> SortedMap Nat (List Nat) -> SortedSet Nat
    bfs [] visited _ = visited
    bfs (q :: qs) visited adj =
      if SortedSet.contains q visited then bfs qs visited adj
      else let visited' = SortedSet.insert q visited
               nbrs = case lookup q adj of Nothing => []; Just ns => ns
               newNbrs = filter (\n => not (SortedSet.contains n visited')) nbrs
           in bfs (qs ++ newNbrs) visited' adj

||| First Betti number (cycle rank) = |E| - |V| + components.
export
betti1 : ConstraintGraph -> Nat
betti1 g =
  let nE = length (cgEdges g)
      nV = length (SortedSet.toList (cgNodes g))
      comp = connectedComponents g
      -- Betti = E - V + comp, but be careful with underflow on Nat
  in if nE + comp > nV then (nE + comp) `minus` nV else 0

||| Count triangles in the graph.
export
countTriangles : ConstraintGraph -> Nat
countTriangles g =
  let nodeList = SortedSet.toList (cgNodes g)
      adj = cgAdj g
  in foldl (\tCount, ni =>
    let nbrsI = SortedSet.fromList (case lookup ni adj of Nothing => []; Just ns => ns)
    in foldl (\t2, nj =>
      if nj <= ni then t2
      else let nbrsJ = case lookup nj adj of Nothing => []; Just ns => ns
               common = filter (\nk => nk > nj && SortedSet.contains nk nbrsI) nbrsJ
           in t2 + length common
    ) tCount (SortedSet.toList nbrsI)
  ) (the Nat 0) nodeList

--- Fundamental cycles (for Approach 1 analysis) ---

||| Build a spanning tree via BFS, returning parent map.
export
spanningTree : ConstraintGraph -> SortedMap Nat Nat
spanningTree g =
  let nodeList = SortedSet.toList (cgNodes g)
  in case nodeList of
    [] => empty
    (root :: _) => bfsTree [root] (SortedSet.singleton root) (cgAdj g) empty
  where
    bfsTree : List Nat -> SortedSet Nat -> SortedMap Nat (List Nat) ->
              SortedMap Nat Nat -> SortedMap Nat Nat
    bfsTree [] _ _ parents = parents
    bfsTree (q :: qs) visited adj parents =
      let nbrs = case lookup q adj of Nothing => []; Just ns => ns
          newNbrs = filter (\n => not (SortedSet.contains n visited)) nbrs
          visited' = foldl (\s, n => SortedSet.insert n s) visited newNbrs
          parents' = foldl (\m, n => insert n q m) parents newNbrs
      in bfsTree (qs ++ newNbrs) visited' adj parents'

||| Find path from node to root in spanning tree (returns reversed path).
pathToRoot : SortedMap Nat Nat -> Nat -> List Nat
pathToRoot parents node = go node [node] 100
  where
    go : Nat -> List Nat -> Nat -> List Nat
    go _ path 0 = path
    go n path fuel =
      case lookup n parents of
        Nothing => path
        Just p => go p (p :: path) (minus fuel 1)

stripCommonPrefix : List Nat -> List Nat -> (List Nat, List Nat)
stripCommonPrefix (x :: xs) (y :: ys) =
  if x == y then stripCommonPrefix xs ys
  else (x :: xs, y :: ys)
stripCommonPrefix xs ys = (xs, ys)

buildCycleFromPaths : SortedMap Nat Nat -> (Nat, Nat) -> List Nat
buildCycleFromPaths parents (u, v) =
  let pathU = pathToRoot parents u
      pathV = pathToRoot parents v
      (restU, restV) = stripCommonPrefix pathU pathV
  in restU ++ reverse restV

||| Find fundamental cycles: one per non-tree edge.
||| Each cycle is a list of node IDs forming a closed path.
export
fundamentalCycles : ConstraintGraph -> List (List Nat)
fundamentalCycles g =
  let parents = spanningTree g
      treeEdgeSet = SortedSet.fromList
        (map (\(c, p) => if c < p then (c, p) else (p, c))
             (SortedMap.toList parents))
      nonTreeEdges = filter (\(i, j) =>
        let e = if i < j then (i, j) else (j, i)
        in not (SortedSet.contains e treeEdgeSet)) (cgEdges g)
  in map (buildCycleFromPaths parents) nonTreeEdges

||| Compute information loss for a cycle given an edge overlap ratio map.
||| Returns -sum(log2(r_i)) for edges in the cycle.
export
cycleInfoLoss : List Nat -> SortedMap (Nat, Nat) Double -> Double
cycleInfoLoss [] _ = 0.0
cycleInfoLoss [_] _ = 0.0
cycleInfoLoss cycle ratioMap = go cycle 0.0
  where
    lookupRatio : Nat -> Nat -> Double
    lookupRatio i j =
      let fwd = lookup (i, j) ratioMap
          bwd = lookup (j, i) ratioMap
      in case fwd of
        Just r => r
        Nothing => case bwd of
          Just r => r
          Nothing => 1.0

    go : List Nat -> Double -> Double
    go [] acc = acc
    go [last] acc =
      -- Close the cycle: edge from last node back to first
      case cycle of
        (first :: _) =>
          let r = lookupRatio last first
          in if r > 0.0 then acc + negate (log r / log 2.0) else acc + 20.0
        [] => acc
    go (a :: b :: rest) acc =
      let r = lookupRatio a b
          loss = if r > 0.0 then negate (log r / log 2.0) else 20.0
      in go (b :: rest) (acc + loss)

--- Edge classification counts ---

||| Count FC, PC, FI edges from CSP edge groups.
export
classifyEdges : List CSPEdgeGroups -> (Nat, Nat, Nat)
classifyEdges egs =
  foldl (\(fc, pc, fi), eg =>
    let srcKeys = SortedSet.fromList (map fst (SortedMap.toList (groupsI eg)))
        dstKeys = SortedSet.fromList (map fst (SortedMap.toList (groupsJ eg)))
        nSrc = length (SortedSet.toList srcKeys)
        nDst = length (SortedSet.toList dstKeys)
        common = length (filter (\k => SortedSet.contains k dstKeys) (SortedSet.toList srcKeys))
    in if nSrc == 0 || nDst == 0 then (fc, pc, fi)
       else if common == 0 then (fc, pc, S fi)
       else if common == nSrc && common == nDst then (S fc, pc, fi)
       else (fc, S pc, fi)
  ) (the Nat 0, the Nat 0, the Nat 0) egs

--- Communication complexity ---

||| Communication complexity result for one edge.
public export
record CommResult where
  constructor MkCommResult
  crNGroupsI : Nat
  crNGroupsJ : Nat
  crNCommon : Nat
  crOneWayCC : Nat
  crCompatFraction : Double

||| Compute communication complexity for a CSP edge.
export
communicationComplexity : CSPEdgeGroups -> CommResult
communicationComplexity eg =
  let keysI = map fst (SortedMap.toList (groupsI eg))
      keysJ = map fst (SortedMap.toList (groupsJ eg))
      setJ = SortedSet.fromList keysJ
      nGI = length keysI
      nGJ = length keysJ
      nCommon = length (filter (\k => SortedSet.contains k setJ) keysI)
      oneWay = if nGI == 0 then 0 else ceilLog2 nGI + 1
      totalI = sumGroupSizes (SortedMap.toList (groupsI eg))
      totalJ = sumGroupSizes (SortedMap.toList (groupsJ eg))
      compatPairs = countCompatPairs (SortedMap.toList (groupsI eg)) (groupsJ eg)
      totalPairs = totalI * totalJ
      fraction = if totalPairs == 0 then 0.0 else cast compatPairs / cast totalPairs
  in MkCommResult nGI nGJ nCommon oneWay fraction
  where
    ceilLog2 : Nat -> Nat
    ceilLog2 0 = 0
    ceilLog2 1 = 0
    ceilLog2 n = S (ceilLog2 (assert_total (divNat (n + 1) 2)))

    sumGroupSizes : List (String, List Nat) -> Nat
    sumGroupSizes = foldl (\a, (_, ids) => a + length ids) 0

    countCompatPairs : List (String, List Nat) -> SortedMap String (List Nat) -> Nat
    countCompatPairs pairs jGroups =
      foldl (\a, (k, idsI) =>
        case lookup k jGroups of
          Nothing => a
          Just idsJ => a + length idsI * length idsJ
      ) 0 pairs

--- Constraint tightness metrics ---

||| Aggregate CC metrics across all edges.
public export
record CCMetrics where
  constructor MkCCMetrics
  ccTotalOneWay : Nat       -- sum of one-way CC across all edges
  ccMaxOneWay : Nat         -- max one-way CC (width lower bound)
  ccMinCompat : Double      -- min compatFraction (tightest constraint)
  ccAvgCompat : Double      -- average compatFraction
  ccTightnessSum : Double   -- sum of -log2(compatFrac) across PC+FI edges
  ccNEdges : Nat            -- number of PC+FI edges analyzed

||| Compute CC metrics across all edge groups.
export
ccMetrics : List CSPEdgeGroups -> CCMetrics
ccMetrics egs =
  let crs = map communicationComplexity egs
      nEdges = length crs
      totalOW = foldl addOneWay 0 crs
      maxOW = foldl maxOneWay 0 crs
      minCF = foldl minCompat 1.0 crs
      sumCF = foldl addCompat 0.0 crs
      avgCF = if nEdges == 0 then 0.0 else sumCF / cast nEdges
      tSum = foldl addTightness 0.0 crs
  in MkCCMetrics totalOW maxOW minCF avgCF tSum nEdges
  where
    addOneWay : Nat -> CommResult -> Nat
    addOneWay a cr = a + crOneWayCC cr

    maxOneWay : Nat -> CommResult -> Nat
    maxOneWay a cr = max a (crOneWayCC cr)

    minCompat : Double -> CommResult -> Double
    minCompat a cr = if crCompatFraction cr < a then crCompatFraction cr else a

    addCompat : Double -> CommResult -> Double
    addCompat a cr = a + crCompatFraction cr

    addTightness : Double -> CommResult -> Double
    addTightness a cr =
      let f = crCompatFraction cr
      in if f > 0.0 && f < 1.0
         then a + negate (log f / log 2.0)
         else if f <= 0.0 then a + 20.0
         else a  -- f == 1.0, fully compatible, contributes 0

||| Compute average and max compatFraction across edges.
export
avgMaxCompat : List CSPEdgeGroups -> (Double, Double)
avgMaxCompat egs =
  let crs = map communicationComplexity egs
      fracs = map crCompatFraction crs
      sumF = foldl (+) 0.0 fracs
      maxF = foldl (\a, f => if f > a then f else a) 0.0 fracs
      avgF = if length fracs == 0 then 0.0 else sumF / cast (length fracs)
  in (avgF, maxF)

--- Type diversity ---

||| Count distinct sub-function truth tables across a set of sub-cube positions.
export
typeDiversity : (n : Nat) -> Bits32 -> List SubCube -> Nat
typeDiversity n tt scs =
  let tts = map (subFunction n tt) scs
      unique = foldl (\s, t => SortedSet.insert t s) SortedSet.empty tts
  in length (SortedSet.toList unique)
