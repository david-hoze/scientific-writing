module Verified.Solver

import Verified.CSP
import Analysis.CompatCSP
import Decidable.Equality
import Data.Vect
import Data.Fin
import Data.List
import Data.So
import Data.SortedMap

%default covering

--- Total assignment ---

||| A total assignment: every node gets a domain-bounded choice.
public export
record VAssignment (numNodes : Nat) (domSizes : Vect numNodes Nat) where
  constructor MkVAssignment
  choices : (i : Fin numNodes) -> Fin (index i domSizes)

--- Edge satisfaction ---

||| Proof that an assignment satisfies one edge:
||| the canonical keys at both endpoints agree.
public export
data SatisfiesEdge : {nn : Nat} -> {ds : Vect nn Nat} ->
                     VAssignment nn ds -> VCSPEdge nn ds -> Type where
  MkSatisfiesEdge :
    {0 nn : Nat} -> {0 ds : Vect nn Nat} ->
    {a : VAssignment nn ds} -> {e : VCSPEdge nn ds} ->
    (keyEq : lookupKey (choices a (src (edge e))) (groupsSrc e)
           = lookupKey (choices a (dst (edge e))) (groupsDst e))
    -> SatisfiesEdge a e

--- All-edges satisfaction ---

||| Proof that ALL edges in a list are satisfied.
public export
data AllSatisfied : {nn : Nat} -> {ds : Vect nn Nat} ->
                    VAssignment nn ds ->
                    List (VCSPEdge nn ds) -> Type where
  SatNil  : AllSatisfied a []
  SatCons : SatisfiesEdge a e -> AllSatisfied a es -> AllSatisfied a (e :: es)

--- SAT witness ---

||| A SAT witness: an assignment together with proof that all edges are satisfied.
public export
record SATWitness (nn : Nat) (ds : Vect nn Nat)
                  (edges : List (VCSPEdge nn ds)) where
  constructor MkSATWitness
  assignment : VAssignment nn ds
  allSatisfied : AllSatisfied assignment edges

--- UNSAT certificate (only real proofs) ---

||| UNSAT certificate. Only TrivialUnsat is valid: a node with empty domain.
||| This is a genuine proof of infeasibility — if any node has zero domain
||| elements, no total assignment can exist.
public export
data UNSATCert : {nn : Nat} -> Vect nn Nat -> Type where
  TrivialUnsat : (node : Fin nn) -> (0 prf : index node ds = 0) ->
                 UNSATCert {nn} ds

--- Verified solve result ---

||| Result of a verified solve.
||| VSat: SAT with machine-checkable witness.
||| VUnsat: genuine proof of infeasibility (empty domain).
||| VInconclusive: solver exhausted fuel — no claim either way.
public export
data VSolveResult : (nn : Nat) -> (ds : Vect nn Nat) ->
                    List (VCSPEdge nn ds) -> Type where
  VSat          : SATWitness nn ds edges -> VSolveResult nn ds edges
  VUnsat        : UNSATCert {nn} ds -> VSolveResult nn ds edges
  VInconclusive : (fuel : Nat) -> VSolveResult nn ds edges

--- Validation of raw solver output ---

||| Check whether a single edge is satisfied by a choice function.
||| Uses DecEq from Decidable.Equality (no believe_me).
public export
checkOneSat : {nn : Nat} -> {ds : Vect nn Nat} ->
              (a : VAssignment nn ds) -> (e : VCSPEdge nn ds) ->
              Either String (SatisfiesEdge a e)
checkOneSat a e =
  case decEq
         (lookupKey (choices a (src (edge e))) (groupsSrc e))
         (lookupKey (choices a (dst (edge e))) (groupsDst e)) of
    Yes prf => Right (MkSatisfiesEdge prf)
    No _    => Left ("Edge " ++ show (finToNat (src (edge e)))
               ++ "->" ++ show (finToNat (dst (edge e)))
               ++ ": key mismatch")

||| Validate all edges, accumulating proofs.
public export
checkAllSat : {nn : Nat} -> {ds : Vect nn Nat} ->
              (a : VAssignment nn ds) ->
              (edges : List (VCSPEdge nn ds)) ->
              Either String (AllSatisfied a edges)
checkAllSat a [] = Right SatNil
checkAllSat a (e :: es) = do
  prf <- checkOneSat a e
  rest <- checkAllSat a es
  Right (SatCons prf rest)

||| Build a VAssignment from node list and raw (nodeId, domIdx) pairs.
buildAssignFromNodes : (nn : Nat) -> (ds : Vect nn Nat) ->
                       SortedMap Nat Nat ->
                       List (Nat, List String) ->
                       Maybe (VAssignment nn ds)
buildAssignFromNodes nn ds assignMap nodes = go nn ds nodes
  where
    go : (k : Nat) -> (sizes : Vect k Nat) ->
         List (Nat, List String) ->
         Maybe (VAssignment k sizes)
    go 0 [] _ = Just (MkVAssignment (\i => absurd i))
    go (S n) (s :: ss) [] = Nothing
    go (S n) (s :: ss) ((nid, _) :: restNodes) = do
      val <- lookup nid assignMap
      fi <- natToFin val s
      restAssign <- go n ss restNodes
      Just (MkVAssignment (\i =>
        case i of
          FZ => fi
          FS j => choices restAssign j))
    go _ _ _ = Nothing

||| Scan domain sizes for a zero entry, returning a TrivialUnsat certificate.
scanForEmpty : {nn : Nat} -> (ds : Vect nn Nat) ->
               (idx : Nat) -> List (Nat, List String) ->
               Maybe (UNSATCert {nn} ds)
scanForEmpty ds idx [] = Nothing
scanForEmpty ds idx ((_, dom) :: rest) =
  case (natToFin idx nn) of
    Nothing => scanForEmpty ds (S idx) rest
    Just fi =>
      case decEq (index fi ds) 0 of
        Yes prf => Just (TrivialUnsat fi prf)
        No _    => scanForEmpty ds (S idx) rest

||| Validate raw solver output into a SAT witness.
public export
validateAssignment : (cspData : CSPData) ->
                     (rawAssign : List (Nat, Nat)) ->
                     Either String
                       (nn : Nat ** ds : Vect nn Nat **
                        edges : List (VCSPEdge nn ds) ** SATWitness nn ds edges)
validateAssignment cspData rawAssign = do
  let Just (nn ** ds ** vedges) = fromRawCSP cspData
    | Nothing => Left "Failed to verify CSP structure"
  let Just assign = buildAssignFromNodes nn ds
                      (SortedMap.fromList rawAssign) (cspNodes cspData)
    | Nothing => Left "Failed to build verified assignment"
  case checkAllSat assign vedges of
    Right allPrf => Right (nn ** ds ** vedges ** MkSATWitness assign allPrf)
    Left err => Left err

||| Run the existing solver and validate the result.
||| SAT: returns a machine-checkable witness.
||| UNSAT with empty domain: returns a genuine proof.
||| UNSAT from fuel exhaustion: returns VInconclusive (no false claim).
export
verifiedSolve : CSPData -> Nat -> Either String
  (nn : Nat ** ds : Vect nn Nat **
   edges : List (VCSPEdge nn ds) ** VSolveResult nn ds edges)
verifiedSolve cspData fuel =
  let Just (nn ** ds ** vedges) = fromRawCSP cspData
    | Nothing => Left "Failed to verify CSP structure"
  in case solveCSP cspData fuel of
       SatResult rawAssign =>
         case validateAssignment cspData rawAssign of
           Right (nn2 ** ds2 ** edges2 ** witness) =>
             Right (nn2 ** ds2 ** edges2 ** VSat witness)
           Left err => Left ("SAT but validation failed: " ++ err)
       UnsatResult =>
         -- Check if we can produce a genuine UNSAT certificate
         case scanForEmpty ds 0 (cspNodes cspData) of
           Just cert => Right (nn ** ds ** vedges ** VUnsat cert)
           Nothing   => Right (nn ** ds ** vedges ** VInconclusive fuel)

export
Show (VSolveResult nn ds edges) where
  show (VSat _) = "VSAT (verified witness)"
  show (VUnsat (TrivialUnsat node _)) =
    "VUNSAT (empty domain at node " ++ show (finToNat node) ++ ")"
  show (VInconclusive fuel) =
    "VINCONCLUSIVE (solver returned UNSAT after " ++ show fuel ++ " fuel, no proof)"
