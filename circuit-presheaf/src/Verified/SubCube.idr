module Verified.SubCube

import Analysis.SubCube
import Data.Vect
import Data.Fin
import Data.List
import Data.So

%default total

--- Helper predicates ---

||| Check if all elements in a Vect are distinct (via DecEq on Fin).
public export
allDistinct : {k : Nat} -> Vect k (Fin n) -> Bool
allDistinct [] = True
allDistinct (x :: xs) = not (elem x xs) && allDistinct xs

||| Check that two Vects share no elements.
public export
noOverlap : {k, j : Nat} -> Vect k (Fin n) -> Vect j (Fin n) -> Bool
noOverlap [] _ = True
noOverlap (x :: xs) ys = not (elem x ys) && noOverlap xs ys

||| A verified sub-cube of an n-dimensional space with d free coordinates.
||| Carries erased proofs of well-formedness.
public export
record VSubCube (n : Nat) (d : Nat) where
  constructor MkVSubCube
  freeCoords  : Vect d (Fin n)
  fixedCoords : Vect (minus n d) (Fin n)
  fixedVals   : Vect (minus n d) Bool
  0 freeDistinct : So (allDistinct freeCoords)
  0 disjoint     : So (noOverlap freeCoords fixedCoords)

||| Convert to raw SubCube (erase proofs and bounds).
public export
toRaw : VSubCube n d -> SubCube
toRaw vsc =
  let free = toList (map finToNat (freeCoords vsc))
      fixed = toList (zipWith (\c, v => (finToNat c, v))
                (fixedCoords vsc) (fixedVals vsc))
  in MkSubCube free fixed

||| Attempt to validate a raw SubCube into a VSubCube.
||| Checks: correct dimensions, all indices < n, distinct, disjoint.
public export
fromRaw : (n : Nat) -> (d : Nat) -> SubCube -> Maybe (VSubCube n d)
fromRaw n d sc = do
  freeVect <- toVectFin d (freeCoords sc)
  fixedCVect <- toVectFin (minus n d) (map fst (fixedValues sc))
  fixedBVect <- toVectBool (minus n d) (map snd (fixedValues sc))
  case (choose (allDistinct freeVect), choose (noOverlap freeVect fixedCVect)) of
    (Left soD, Left soJ) => Just (MkVSubCube freeVect fixedCVect fixedBVect soD soJ)
    _ => Nothing
  where
    toVectFin : (k : Nat) -> List Nat -> Maybe (Vect k (Fin n))
    toVectFin 0 [] = Just []
    toVectFin 0 _  = Nothing
    toVectFin (S k) [] = Nothing
    toVectFin (S k) (x :: xs) = do
      fi <- natToFin x n
      rest <- toVectFin k xs
      Just (fi :: rest)

    toVectBool : (k : Nat) -> List Bool -> Maybe (Vect k Bool)
    toVectBool 0 [] = Just []
    toVectBool 0 _  = Nothing
    toVectBool (S k) [] = Nothing
    toVectBool (S k) (x :: xs) = map (x ::) (toVectBool k xs)

||| Check structural adjacency on verified sub-cubes.
public export
vStructurallyAdjacent : VSubCube n d -> VSubCube n d -> Bool
vStructurallyAdjacent a b = structurallyAdjacent (toRaw a) (toRaw b)

||| Generate all verified sub-cubes, filtering out any that fail validation.
public export
allVSubCubes : (n : Nat) -> (d : Nat) -> List (VSubCube n d)
allVSubCubes n d = mapMaybe (fromRaw n d) (allSubCubes n d)

export
Show (VSubCube n d) where
  show vsc = show (toRaw vsc)
