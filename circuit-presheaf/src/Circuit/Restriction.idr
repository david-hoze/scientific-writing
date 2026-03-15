module Circuit.Restriction

import Circuit.Formula

%default total

||| Substitute input x_i with a constant value.
substitute : Formula -> (i : Nat) -> (v : Bool) -> Formula
substitute (Input j) i v = if j == i then Const v else Input j
substitute (Const b) _ _ = Const b
substitute (Not c) i v = Not (substitute c i v)
substitute (And l r) i v = And (substitute l i v) (substitute r i v)
substitute (Or l r) i v = Or (substitute l i v) (substitute r i v)

||| Propagate constants and simplify bottom-up.
||| Handles: constant folding, double negation, idempotence.
propagate : Formula -> Formula
propagate (Input j) = Input j
propagate (Const b) = Const b
propagate (Not c) = case propagate c of
  Const b => Const (not b)
  Not c'  => c'               -- double negation: NOT(NOT(x)) → x
  c'      => Not c'
propagate (And l r) = case (propagate l, propagate r) of
  (Const False, _) => Const False
  (_, Const False) => Const False
  (Const True, r') => r'
  (l', Const True) => l'
  (l', r')         => And l' r'
propagate (Or l r) = case (propagate l, propagate r) of
  (Const True, _)  => Const True
  (_, Const True)  => Const True
  (Const False, r') => r'
  (l', Const False) => l'
  (l', r')          => Or l' r'

||| Re-index remaining inputs: any index > i decrements by 1.
reindex : (i : Nat) -> Formula -> Formula
reindex i (Input j) = if j > i then Input (minus j 1) else Input j
reindex i (Const b) = Const b
reindex i (Not c) = Not (reindex i c)
reindex i (And l r) = And (reindex i l) (reindex i r)
reindex i (Or l r) = Or (reindex i l) (reindex i r)

||| Restrict a formula by hardwiring input x_i to value v,
||| then propagate constants bottom-up and re-index remaining inputs.
public export
restrict : Formula -> (i : Nat) -> (v : Bool) -> Formula
restrict f i v = reindex i (propagate (substitute f i v))
