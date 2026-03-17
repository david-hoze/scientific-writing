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

--- Type diversity ---

||| Count distinct sub-function truth tables across a set of sub-cube positions.
export
typeDiversity : (n : Nat) -> Bits32 -> List SubCube -> Nat
typeDiversity n tt scs =
  let tts = map (subFunction n tt) scs
      unique = foldl (\s, t => SortedSet.insert t s) SortedSet.empty tts
  in length (SortedSet.toList unique)
