module Verified.Formula

import Circuit.Formula
import Data.Vect
import Data.Fin
import Data.Fin.Properties

%default total

||| Boolean formula with all input indices bounded by dimension d.
public export
data VFormula : (d : Nat) -> Type where
  VInput : Fin d -> VFormula d
  VConst : Bool -> VFormula d
  VNot   : VFormula d -> VFormula d
  VAnd   : VFormula d -> VFormula d -> VFormula d
  VOr    : VFormula d -> VFormula d -> VFormula d

||| Total evaluation — no out-of-bounds fallback needed.
public export
veval : VFormula d -> Vect d Bool -> Bool
veval (VInput i) xs = index i xs
veval (VConst b) _  = b
veval (VNot c)   xs = not (veval c xs)
veval (VAnd l r) xs = veval l xs && veval r xs
veval (VOr l r)  xs = veval l xs || veval r xs

||| Gate count (same semantics as Circuit.Formula.size).
public export
vsize : VFormula d -> Nat
vsize (VInput _) = 0
vsize (VConst _) = 0
vsize (VNot c)   = 1 + vsize c
vsize (VAnd l r) = 1 + vsize l + vsize r
vsize (VOr l r)  = 1 + vsize l + vsize r

||| Erase bounds — convert to raw Formula.
public export
toRaw : VFormula d -> Formula
toRaw (VInput i) = Input (finToNat i)
toRaw (VConst b) = Const b
toRaw (VNot c)   = Not (toRaw c)
toRaw (VAnd l r) = And (toRaw l) (toRaw r)
toRaw (VOr l r)  = Or (toRaw l) (toRaw r)

||| Validate a raw Formula: succeed only if all Input indices < d.
public export
fromRaw : (d : Nat) -> Formula -> Maybe (VFormula d)
fromRaw d (Input idx) = map VInput (natToFin idx d)
fromRaw d (Const b)   = Just (VConst b)
fromRaw d (Not c)     = map VNot (fromRaw d c)
fromRaw d (And l r)   = [| VAnd (fromRaw d l) (fromRaw d r) |]
fromRaw d (Or l r)    = [| VOr (fromRaw d l) (fromRaw d r) |]

||| Show instance for debugging.
export
Show (VFormula d) where
  show (VInput i) = "x" ++ show (finToNat i)
  show (VConst True) = "1"
  show (VConst False) = "0"
  show (VNot c) = "NOT(" ++ show c ++ ")"
  show (VAnd l r) = "AND(" ++ show l ++ "," ++ show r ++ ")"
  show (VOr l r) = "OR(" ++ show l ++ "," ++ show r ++ ")"

--- Correctness proof: veval agrees with eval on the raw Formula ---

||| natToFinLT (finToNat i) = i, by induction on Fin.
||| The 0-quantity proof is pattern-matched to refine types (erased at runtime).
natToFinLTInverse : {d : Nat} -> (i : Fin d) ->
  {0 prf : LT (finToNat i) d} -> natToFinLT (finToNat i) {prf} = i
natToFinLTInverse FZ {prf = LTESucc _} = Refl
natToFinLTInverse (FS k) {prf = LTESucc _} = cong FS (natToFinLTInverse k)

||| natToFin (finToNat i) d = Just i.
||| With-matches on isLT to expose natToFin's case-split, then uses
||| natToFinLTInverse for the Yes branch and elemSmallerThanBound to
||| discharge the impossible No branch.
natToFinRoundtrip : {d : Nat} -> (i : Fin d) ->
  natToFin (finToNat i) d = Just i
natToFinRoundtrip i with (isLT (finToNat i) d)
  natToFinRoundtrip i | Yes prf = cong Just (natToFinLTInverse i {prf})
  natToFinRoundtrip i | No contra = absurd (contra (elemSmallerThanBound i))

||| Base case: index i xs = eval (Input (finToNat i)) xs.
||| With-matches on natToFin to expose eval's case-split, then uses
||| natToFinRoundtrip to unify fi with i (Just branch) or derive
||| a contradiction (Nothing branch).
indexFinLemma : {d : Nat} -> (i : Fin d) -> (xs : Vect d Bool) ->
  index i xs = eval {d} (Input (finToNat i)) xs
indexFinLemma i xs with (natToFin (finToNat i) d) proof eq
  indexFinLemma i xs | Just fi =
    -- eq : natToFin (finToNat i) d = Just fi
    -- natToFinRoundtrip i : natToFin (finToNat i) d = Just i
    -- So Just fi = Just i, giving fi = i by injectivity.
    let combined : (Just fi = Just i) = trans (sym eq) (natToFinRoundtrip i)
    in case combined of Refl => Refl
  indexFinLemma i xs | Nothing =
    -- eq : natToFin (finToNat i) d = Nothing, but roundtrip says Just i
    absurd (the (Nothing = Just i) (trans (sym eq) (natToFinRoundtrip i)))

||| Evaluation of VFormula agrees with evaluation of its raw erasure.
||| veval f xs = eval (toRaw f) xs
public export
evalPreserved : {d : Nat} -> (f : VFormula d) -> (xs : Vect d Bool) ->
  veval f xs = eval {d} (toRaw f) xs
evalPreserved (VInput i) xs = indexFinLemma i xs
evalPreserved (VConst b) xs = Refl
evalPreserved (VNot c) xs = rewrite evalPreserved c xs in Refl
evalPreserved (VAnd l r) xs =
  rewrite evalPreserved l xs in
  rewrite evalPreserved r xs in Refl
evalPreserved (VOr l r) xs =
  rewrite evalPreserved l xs in
  rewrite evalPreserved r xs in Refl
