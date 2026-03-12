module Circuit.Canonical

import Circuit.Formula
import Data.String

%default total

||| Canonical string representation of a formula.
||| Commutative children (AND, OR) are sorted lexicographically.
||| This is the ONLY equivalence — two formulas are DAG-isomorphic
||| iff they have the same canonical string.
public export
canonical : Formula -> String
canonical (Input i) = "x" ++ show i
canonical (Const True) = "c1"
canonical (Const False) = "c0"
canonical (Not c) = "N(" ++ canonical c ++ ")"
canonical (And l r) =
  let cl = canonical l
      cr = canonical r
      (a, b) = if cl <= cr then (cl, cr) else (cr, cl)
  in "A(" ++ a ++ "," ++ b ++ ")"
canonical (Or l r) =
  let cl = canonical l
      cr = canonical r
      (a, b) = if cl <= cr then (cl, cr) else (cr, cl)
  in "O(" ++ a ++ "," ++ b ++ ")"
