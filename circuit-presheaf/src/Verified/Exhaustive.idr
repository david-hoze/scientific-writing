module Verified.Exhaustive

import Analysis.CompatCSP
import Data.SortedMap
import Data.List

%default covering

--- Certificate Types ---

mutual
  ||| Reason why a specific domain value at a node was rejected.
  public export
  data RejectReason : Type where
    ||| Value incompatible with an already-assigned neighbor.
    ||| Records the index into the edge list where the conflict occurs.
    Inconsistent : (edgeIdx : Nat) -> RejectReason
    ||| Value was locally consistent, but the sub-problem is UNSAT.
    DeeperUnsat : RefutationCert -> RejectReason

  ||| Complete refutation certificate: a search tree proving no valid
  ||| assignment exists. Every branch is justified.
  public export
  data RefutationCert : Type where
    ||| Node has zero domain elements — trivially no assignment.
    CertEmpty : (nodeIdx : Nat) -> RefutationCert
    ||| Every value in [0..domSize-1] for this node was rejected.
    ||| The list has exactly domSize entries, one per value.
    CertBranch : (nodeIdx : Nat) -> (domSize : Nat) ->
                 List (Nat, RejectReason) -> RefutationCert

--- Certificate Statistics ---

||| Count total nodes in the certificate tree.
public export
certSize : RefutationCert -> Nat

||| Size of a single rejection reason.
public export
reasonSize : (Nat, RejectReason) -> Nat

certSize (CertEmpty _) = 1
certSize (CertBranch _ _ reasons) = 1 + foldl (\acc, r => acc + reasonSize r) 0 reasons

reasonSize (_, Inconsistent _) = 1
reasonSize (_, DeeperUnsat cert) = 1 + certSize cert

||| Maximum depth of the certificate tree.
public export
certDepth : RefutationCert -> Nat
certDepth (CertEmpty _) = 0
certDepth (CertBranch _ _ reasons) =
  1 + foldl (\mx, r => max mx (reasonDepth r)) 0 reasons
  where
    reasonDepth : (Nat, RejectReason) -> Nat
    reasonDepth (_, Inconsistent _) = 0
    reasonDepth (_, DeeperUnsat cert) = certDepth cert

||| Count immediate (Inconsistent) vs recursive (DeeperUnsat) rejections at root.
public export
certRootStats : RefutationCert -> (Nat, Nat)
certRootStats (CertEmpty _) = (0, 0)
certRootStats (CertBranch _ _ reasons) =
  foldl (\(imm, deep), (_, r) => case r of
    Inconsistent _ => (S imm, deep)
    DeeperUnsat _ => (imm, S deep)) (0, 0) reasons

--- Certificate-Producing Complete Solver ---

||| Find the first edge that causes incompatibility for a value.
||| Returns the edge list index, or Nothing if consistent on all edges.
findConflictEdge : Nat -> Nat -> SortedMap Nat Nat -> List EdgeKeys -> Nat -> Maybe Nat
findConflictEdge nodeIdx valIdx assignment [] _ = Nothing
findConflictEdge nodeIdx valIdx assignment (ek :: rest) eidx =
  let otherNode = if ekNodeI ek == nodeIdx then ekNodeJ ek
                  else if ekNodeJ ek == nodeIdx then ekNodeI ek
                  else nodeIdx
  in if otherNode == nodeIdx
     then findConflictEdge nodeIdx valIdx assignment rest (S eidx)
     else case lookup otherNode assignment of
            Nothing => findConflictEdge nodeIdx valIdx assignment rest (S eidx)
            Just otherVal =>
              if checkEdge nodeIdx valIdx otherNode otherVal ek
              then findConflictEdge nodeIdx valIdx assignment rest (S eidx)
              else Just eidx

||| Complete backtracking solver that produces a refutation certificate.
||| No fuel limit — terminates because the domain is finite.
||| Returns Left (SAT assignment) or Right (UNSAT certificate).
export
solveWithCert : CSPData -> Either (List (Nat, Nat)) RefutationCert
solveWithCert cspData =
  let nonEmpty = filter (\(_, dom) => length dom > 0) (cspNodes cspData)
      sorted = sortBy (\(_, d1), (_, d2) => compare (length d1) (length d2)) nonEmpty
      nodeOrder = map (\(i, dom) => (i, length dom)) sorted
      ekeys = map mkEdgeKeys (cspEdgeGroups cspData)
      -- Check for empty domains first
      empties = filter (\(_, dom) => length dom == 0) (cspNodes cspData)
  in case empties of
       ((nid, _) :: _) => Right (CertEmpty nid)
       [] => go nodeOrder empty ekeys
  where
    mutual
      go : List (Nat, Nat) -> SortedMap Nat Nat -> List EdgeKeys ->
           Either (List (Nat, Nat)) RefutationCert
      go [] assign _ = Left (SortedMap.toList assign)
      go ((nodeIdx, domSize) :: rest) assign ekeys =
        tryValues nodeIdx 0 domSize rest assign ekeys []

      tryValues : Nat -> Nat -> Nat -> List (Nat, Nat) -> SortedMap Nat Nat ->
                  List EdgeKeys -> List (Nat, RejectReason) ->
                  Either (List (Nat, Nat)) RefutationCert
      tryValues nodeIdx idx domSize rest assign ekeys rejects =
        if idx >= domSize
        then Right (CertBranch nodeIdx domSize (reverse rejects))
        else if isConsistent nodeIdx idx assign ekeys
          then case go rest (insert nodeIdx idx assign) ekeys of
                 Left sol => Left sol  -- SAT found, propagate up
                 Right cert =>
                   tryValues nodeIdx (S idx) domSize rest assign ekeys
                     ((idx, DeeperUnsat cert) :: rejects)
          else case findConflictEdge nodeIdx idx assign ekeys 0 of
                 Just eidx =>
                   tryValues nodeIdx (S idx) domSize rest assign ekeys
                     ((idx, Inconsistent eidx) :: rejects)
                 Nothing =>
                   -- Shouldn't happen (isConsistent said False but no conflict found)
                   -- Use edge 0 as fallback
                   tryValues nodeIdx (S idx) domSize rest assign ekeys
                     ((idx, Inconsistent 0) :: rejects)

--- Fuel-limited solver ---

||| Solve with a total node-count fuel limit. Returns Nothing if fuel runs out.
||| Fuel is threaded through: each cert node consumes 1 fuel. Returns remaining fuel.
export
solveWithCertFuel : CSPData -> Nat -> Maybe (Either (List (Nat, Nat)) RefutationCert)
solveWithCertFuel cspData fuel =
  let nonEmpty = filter (\(_, dom) => length dom > 0) (cspNodes cspData)
      sorted = sortBy (\(_, d1), (_, d2) => compare (length d1) (length d2)) nonEmpty
      nodeOrder = map (\(i, dom) => (i, length dom)) sorted
      ekeys = map mkEdgeKeys (cspEdgeGroups cspData)
      empties = filter (\(_, dom) => length dom == 0) (cspNodes cspData)
  in case empties of
       ((nid, _) :: _) => Just (Right (CertEmpty nid))
       [] => case goFuel nodeOrder empty ekeys fuel of
               Nothing => Nothing
               Just (Left sol, _) => Just (Left sol)
               Just (Right cert, _) => Just (Right cert)
  where
    mutual
      goFuel : List (Nat, Nat) -> SortedMap Nat Nat -> List EdgeKeys -> Nat ->
               Maybe (Either (List (Nat, Nat)) RefutationCert, Nat)
      goFuel _ _ _ 0 = Nothing
      goFuel [] assign _ fl = Just (Left (SortedMap.toList assign), fl)
      goFuel ((nodeIdx, domSize) :: rest) assign ekeys fl =
        tryFuel nodeIdx 0 domSize rest assign ekeys [] fl

      tryFuel : Nat -> Nat -> Nat -> List (Nat, Nat) -> SortedMap Nat Nat ->
                List EdgeKeys -> List (Nat, RejectReason) -> Nat ->
                Maybe (Either (List (Nat, Nat)) RefutationCert, Nat)
      tryFuel _ _ _ _ _ _ _ 0 = Nothing
      tryFuel nodeIdx idx domSize rest assign ekeys rejects fl =
        if idx >= domSize
        then Just (Right (CertBranch nodeIdx domSize (reverse rejects)), minus fl 1)
        else if isConsistent nodeIdx idx assign ekeys
          then case goFuel rest (insert nodeIdx idx assign) ekeys (minus fl 1) of
                 Nothing => Nothing
                 Just (Left sol, _) => Just (Left sol, 0)
                 Just (Right cert, fl') =>
                   tryFuel nodeIdx (S idx) domSize rest assign ekeys
                     ((idx, DeeperUnsat cert) :: rejects) fl'
          else case findConflictEdge nodeIdx idx assign ekeys 0 of
                 Just eidx =>
                   tryFuel nodeIdx (S idx) domSize rest assign ekeys
                     ((idx, Inconsistent eidx) :: rejects) (minus fl 1)
                 Nothing =>
                   tryFuel nodeIdx (S idx) domSize rest assign ekeys
                     ((idx, Inconsistent 0) :: rejects) (minus fl 1)

--- Certificate Checker ---

||| Index into a list by position.
indexAt : Nat -> List a -> Maybe a
indexAt _ [] = Nothing
indexAt 0 (x :: _) = Just x
indexAt (S k) (_ :: xs) = indexAt k xs

||| Look up domain size for a node in the node list.
lookupDomSize : Nat -> List (Nat, Nat) -> Maybe Nat
lookupDomSize _ [] = Nothing
lookupDomSize target ((n, s) :: rest) =
  if n == target then Just s else lookupDomSize target rest

mutual
  ||| Independently verify a refutation certificate against CSP data.
  ||| Rebuilds edge keys from scratch and checks every claim in the certificate.
  |||
  ||| Returns True iff:
  |||   - CertEmpty: the node's domain is genuinely zero
  |||   - CertBranch: all values are covered and each rejection reason is valid
  public export
  checkCert : List (Nat, Nat) ->   -- [(nodeIdx, domSize)] from CSP
              List EdgeKeys ->      -- edge data (independently constructed)
              SortedMap Nat Nat ->  -- current partial assignment
              RefutationCert -> Bool
  checkCert doms ekeys assign (CertEmpty nodeIdx) =
    case lookupDomSize nodeIdx doms of
      Just 0 => True
      _ => False
  checkCert doms ekeys assign (CertBranch nodeIdx domSize reasons) =
    case lookupDomSize nodeIdx doms of
      Nothing => False
      Just actualDom =>
        actualDom == domSize &&
        length reasons == domSize &&
        checkAllReasons doms ekeys assign nodeIdx 0 domSize reasons

  ||| Verify that all values in [0..domSize-1] are covered with valid reasons.
  public export
  checkAllReasons : List (Nat, Nat) -> List EdgeKeys -> SortedMap Nat Nat ->
                    Nat -> Nat -> Nat -> List (Nat, RejectReason) -> Bool
  checkAllReasons doms ekeys assign nodeIdx idx domSize [] = idx >= domSize
  checkAllReasons doms ekeys assign nodeIdx idx domSize ((v, reason) :: rest) =
    v == idx &&
    checkReason doms ekeys assign nodeIdx v reason &&
    checkAllReasons doms ekeys assign nodeIdx (S idx) domSize rest

  ||| Verify a single rejection reason.
  public export
  checkReason : List (Nat, Nat) -> List EdgeKeys -> SortedMap Nat Nat ->
                Nat -> Nat -> RejectReason -> Bool
  checkReason doms ekeys assign nodeIdx valIdx (Inconsistent edgeIdx) =
    -- The claimed edge must actually cause incompatibility
    case indexAt edgeIdx ekeys of
      Nothing => False
      Just ek =>
        let otherNode = if ekNodeI ek == nodeIdx then ekNodeJ ek
                        else if ekNodeJ ek == nodeIdx then ekNodeI ek
                        else nodeIdx
        in case lookup otherNode assign of
             Nothing => False  -- other node not assigned: can't be incompatible
             Just otherVal =>
               not (checkEdge nodeIdx valIdx otherNode otherVal ek)
  checkReason doms ekeys assign nodeIdx valIdx (DeeperUnsat subcert) =
    -- Value must be locally consistent (if not, Inconsistent should be used)
    isConsistent nodeIdx valIdx assign ekeys &&
    -- Sub-certificate must be valid with the extended assignment
    checkCert doms ekeys (insert nodeIdx valIdx assign) subcert

--- Top-level verified UNSAT ---

||| Run complete solver, then independently check the certificate.
||| Returns the certificate + whether it passed verification.
export
exhaustiveVerifiedSolve : CSPData ->
  Either (List (Nat, Nat))   -- SAT assignment
         (RefutationCert, Bool)  -- UNSAT cert + verification result
exhaustiveVerifiedSolve cspData =
  case solveWithCert cspData of
    Left assign => Left assign
    Right cert =>
      let -- Reconstruct edge keys independently for checking
          ekeys = map mkEdgeKeys (cspEdgeGroups cspData)
          doms = map (\(i, dom) => (i, length dom)) (cspNodes cspData)
          valid = checkCert doms ekeys empty cert
      in Right (cert, valid)
