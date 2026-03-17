module Analysis.VerifiedStats

import Circuit.Formula
import Circuit.Canonical
import Circuit.Enumerate
import Analysis.SubCube
import Analysis.CompatCSP
import Analysis.GraphTopology
import Data.SortedMap
import Data.SortedSet
import Data.List
import Data.Bits
import Data.String

%default covering

--- Edge info ---

||| Information about a single CSP edge.
public export
record EdgeInfo where
  constructor MkEdgeInfo
  eiSrc : Nat
  eiDst : Nat
  eiSrcKeys : Nat
  eiDstKeys : Nat
  eiMatching : Nat
  eiClass : String
  eiRSrc : Double
  eiRDst : Double

||| Extract edge info from a CSPEdgeGroups.
export
edgeInfo : CSPEdgeGroups -> EdgeInfo
edgeInfo eg =
  let srcKeys = map fst (SortedMap.toList (groupsI eg))
      dstKeys = map fst (SortedMap.toList (groupsJ eg))
      dstSet = SortedSet.fromList dstKeys
      sk = length srcKeys
      dk = length dstKeys
      mk = length (filter (\k => SortedSet.contains k dstSet) srcKeys)
      cls = if mk == 0 then "FI"
            else if mk == sk && mk == dk then "FC"
            else "PC"
      rSrc = if sk == 0 then 0.0 else cast mk / cast sk
      rDst = if dk == 0 then 0.0 else cast mk / cast dk
  in MkEdgeInfo (edgeI eg) (edgeJ eg) sk dk mk cls rSrc rDst

--- Instance analysis ---

||| Full analysis result for one truth table.
public export
record InstanceResult where
  constructor MkInstanceResult
  irTT : Bits32
  irEdges : List EdgeInfo
  irDiversity : (Nat, Nat)
  irResult : String

||| Analyze a single function's CSP structure.
||| Returns edge info, type diversity, and SAT/UNSAT result.
export
analyzeInstance : (n : Nat) -> Bits32 -> (d : Nat) -> (maxS : Nat) -> InstanceResult
analyzeInstance n targetTT d maxS =
  let cspData = buildCSPData n d maxS targetTT
      res = cspResult cspData
      egs = cspEdgeGroups cspData
      edges = map edgeInfo egs
      -- Type diversity
      scs = allSubCubes n d
      nDistinct = typeDiversity n targetTT scs
      diversity = (nDistinct, numFunctions d)
      -- Solve
      result = if emptyDomainNodes res > 0
               then "TRIVIAL_UNSAT"
               else case solveCSP cspData 1000000 of
                      SatResult _ => "SAT"
                      UnsatResult => "UNSAT"
  in MkInstanceResult targetTT edges diversity result

--- Aggregate statistics ---

||| Edge classification distribution across multiple instances.
export
edgeClassDist : List InstanceResult -> (Nat, Nat, Nat)
edgeClassDist results =
  let allEdges = concatMap irEdges results
  in foldl (\(fc, pc, fi), e =>
    if eiClass e == "FC" then (S fc, pc, fi)
    else if eiClass e == "PC" then (fc, S pc, fi)
    else if eiClass e == "FI" then (fc, pc, S fi)
    else (fc, pc, fi)
  ) (the Nat 0, the Nat 0, the Nat 0) allEdges

||| Overlap ratio statistics for PC edges.
||| Returns (mean, min, max) of r_src on PC edges.
export
overlapStats : List InstanceResult -> (Double, Double, Double)
overlapStats results =
  let pcEdges = filter (\e => eiClass e == "PC" && eiSrcKeys e > 0)
                  (concatMap irEdges results)
      rVals = map eiRSrc pcEdges
  in case rVals of
    [] => (0.0, 0.0, 0.0)
    (r :: rs) =>
      let sumR = foldl (+) 0.0 rVals
          n : Double = cast (length rVals)
          mn = foldl (\a, v => if v < a then v else a) r rs
          mx = foldl (\a, v => if v > a then v else a) r rs
      in (sumR / n, mn, mx)

||| Cycle loss analysis: geometric mean of r_src across PC edges per instance.
||| Returns (average geometric mean, estimated B1, combined loss).
export
cycleLossAnalysis : List InstanceResult -> (Double, Double)
cycleLossAnalysis results =
  let perInstance = mapMaybe computeGeoMean results
      geoMeans = map fst perInstance
  in case geoMeans of
    [] => (1.0, 0.0)
    _ => let sumG = foldl (+) 0.0 geoMeans
             n : Double = cast (length geoMeans)
         in (sumG / n, n)
  where
    computeGeoMean : InstanceResult -> Maybe (Double, Nat)
    computeGeoMean ir =
      let pcEdges = filter (\e => eiClass e == "PC" && eiSrcKeys e > 0) (irEdges ir)
      in case pcEdges of
        [] => Nothing
        _ => let logR = foldl (\a, e => a + log (eiRSrc e)) 0.0 pcEdges
                 n : Double = cast (length pcEdges)
             in Just (exp (logR / n), length pcEdges)

--- File parsing utilities ---

||| Parse a line containing a decimal integer.
export
parseTTLine : String -> Maybe Bits32
parseTTLine s =
  let trimmed = trim s
  in if trimmed == "" then Nothing
     else case parsePositive {a=Integer} trimmed of
            Just n => Just (cast n)
            Nothing => Nothing
