module Verified.CSP

import Verified.Formula
import Verified.SubCube
import Analysis.CompatCSP
import Data.Vect
import Data.Fin
import Data.List
import Data.So
import Data.SortedMap

%default total

--- Canonical group partition verification ---

||| Check that a list of (label, indices) forms a partition of {0..domSize-1}.
||| Every element in 0..domSize-1 appears exactly once across all groups.
public export
checkPartition : (domSize : Nat) -> List (String, List (Fin domSize)) -> Bool
checkPartition domSize groups =
  let allIndices = concatMap snd groups
      -- Check: total count equals domSize
      countOk = length allIndices == domSize
      -- Check: no duplicates (all distinct)
      distinctOk = allDistinctList allIndices
  in countOk && distinctOk
  where
    allDistinctList : List (Fin k) -> Bool
    allDistinctList [] = True
    allDistinctList (x :: xs) = not (elem x xs) && allDistinctList xs

||| Verified canonical groups: proven to partition {0..domSize-1}.
public export
record VCanonGroups (domSize : Nat) where
  constructor MkVCanonGroups
  groups : List (String, List (Fin domSize))
  0 isPartition : So (checkPartition domSize groups)

||| Get the canonical key for a domain element.
public export
lookupKey : Fin domSize -> VCanonGroups domSize -> Maybe String
lookupKey fi vcg = go (groups vcg)
  where
    go : List (String, List (Fin domSize)) -> Maybe String
    go [] = Nothing
    go ((key, members) :: rest) =
      if elem fi members then Just key else go rest

--- Verified edges ---

||| An edge between two distinct nodes, both bounded by numNodes.
public export
record VEdge (numNodes : Nat) where
  constructor MkVEdge
  src : Fin numNodes
  dst : Fin numNodes
  0 neq : So (src /= dst)

||| Verified CSP edge: bounded node references + partitioned groups.
public export
record VCSPEdge (numNodes : Nat) (domSizes : Vect numNodes Nat) where
  constructor MkVCSPEdge
  edge : VEdge numNodes
  groupsSrc : VCanonGroups (index (src (edge)) domSizes)
  groupsDst : VCanonGroups (index (dst (edge)) domSizes)

--- Edge classification with evidence ---

||| Check if two key lists are identical (as sorted lists).
public export
sameKeys : List String -> List String -> Bool
sameKeys xs ys = sort xs == sort ys

||| Check if two key lists are disjoint.
public export
disjointKeys : List String -> List String -> Bool
disjointKeys xs ys = all (\k => not (elem k ys)) xs

||| Edge classification carrying evidence about compatibility.
public export
data VEdgeClass : Type where
  VFullyCompat   : (srcKeys : List String) -> (dstKeys : List String) ->
                   (0 prf : So (sameKeys srcKeys dstKeys)) -> VEdgeClass
  VFullyIncompat : (srcKeys : List String) -> (dstKeys : List String) ->
                   (0 prf : So (disjointKeys srcKeys dstKeys)) -> VEdgeClass
  VPartialCompat : VEdgeClass

||| Classify a verified edge by comparing canonical group keys.
public export
classifyVEdge : VCanonGroups m -> VCanonGroups k -> VEdgeClass
classifyVEdge vcgSrc vcgDst =
  let srcKeys = map fst (groups vcgSrc)
      dstKeys = map fst (groups vcgDst)
  in case choose (sameKeys srcKeys dstKeys) of
       Left prf => VFullyCompat srcKeys dstKeys prf
       Right _ =>
         case choose (disjointKeys srcKeys dstKeys) of
           Left prf => VFullyIncompat srcKeys dstKeys prf
           Right _  => VPartialCompat

--- Bridges from raw CSP data ---

||| Convert raw CanonGroups to verified, given a known domain size.
public export
fromRawGroups : (domSize : Nat) -> CanonGroups -> Maybe (VCanonGroups domSize)
fromRawGroups domSize rawGroups = do
  let entries = SortedMap.toList rawGroups
  converted <- traverse convertEntry entries
  case choose (checkPartition domSize converted) of
    Left prf => Just (MkVCanonGroups converted prf)
    Right _ => Nothing
  where
    convertIndices : List Nat -> Maybe (List (Fin domSize))
    convertIndices [] = Just []
    convertIndices (x :: xs) = do
      fi <- natToFin x domSize
      rest <- convertIndices xs
      Just (fi :: rest)

    convertEntry : (String, List Nat) -> Maybe (String, List (Fin domSize))
    convertEntry (key, ids) = do
      fids <- convertIndices ids
      Just (key, fids)

||| Convert raw CSPData to verified form.
||| Returns the number of nodes, domain sizes, and verified edges.
public export
fromRawCSP : CSPData ->
  Maybe (numNodes : Nat ** domSizes : Vect numNodes Nat ** List (VCSPEdge numNodes domSizes))
fromRawCSP cspData =
  let nodes = cspNodes cspData
      numNodes = length nodes
  in do
    domSizesL <- Just (map (length . snd) nodes)
    domSizesV <- listToVect numNodes domSizesL
    edges <- traverse (convertEdge numNodes domSizesV nodes) (cspEdgeGroups cspData)
    Just (numNodes ** domSizesV ** edges)
  where
    listToVect : (k : Nat) -> List a -> Maybe (Vect k a)
    listToVect 0 [] = Just []
    listToVect 0 _  = Nothing
    listToVect (S k) [] = Nothing
    listToVect (S k) (x :: xs) = map (x ::) (listToVect k xs)

    -- Find the index of a node by its node ID
    findNodeIdx : Nat -> List (Nat, List String) -> Maybe Nat
    findNodeIdx _ [] = Nothing
    findNodeIdx target ((nid, _) :: rest) =
      if nid == target then Just 0
      else map S (findNodeIdx target rest)

    convertEdge : (nn : Nat) -> (ds : Vect nn Nat) ->
                  List (Nat, List String) ->
                  CSPEdgeGroups -> Maybe (VCSPEdge nn ds)
    convertEdge nn ds nodes eg = do
      srcIdx <- findNodeIdx (edgeI eg) nodes
      dstIdx <- findNodeIdx (edgeJ eg) nodes
      srcFin <- natToFin srcIdx nn
      dstFin <- natToFin dstIdx nn
      case choose (srcFin /= dstFin) of
        Right _ => Nothing
        Left neqPrf => do
          gSrc <- fromRawGroups (index srcFin ds) (groupsI eg)
          gDst <- fromRawGroups (index dstFin ds) (groupsJ eg)
          Just (MkVCSPEdge (MkVEdge srcFin dstFin neqPrf) gSrc gDst)
