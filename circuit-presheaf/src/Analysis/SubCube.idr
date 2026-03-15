module Analysis.SubCube

import Circuit.Formula
import Circuit.Canonical
import Circuit.Restriction
import Data.List
import Data.Bits
import Data.Fin

%default covering

||| A sub-cube: free coordinates and fixed (coordinate, value) pairs.
public export
record SubCube where
  constructor MkSubCube
  freeCoords : List Nat
  fixedValues : List (Nat, Bool)

||| Choose k elements from a list (all combinations).
export
choose : Nat -> List a -> List (List a)
choose 0 _ = [[]]
choose _ [] = []
choose (S k) (x :: xs) = map (x ::) (choose k xs) ++ choose (S k) xs

||| All 2^k Boolean vectors of length k.
allAssignments : Nat -> List (List Bool)
allAssignments 0 = [[]]
allAssignments (S k) = map (False ::) (allAssignments k) ++ map (True ::) (allAssignments k)

||| All sub-cubes of dimension d for n input variables.
export
allSubCubes : (n : Nat) -> (d : Nat) -> List SubCube
allSubCubes n d =
  let allCoords = [0 .. minus n 1]
      freeChoices = choose d allCoords
  in concatMap (\free =>
    let fixed = filter (\c => not (elem c free)) allCoords
    in map (\assign => MkSubCube free (zip fixed assign)) (allAssignments (length fixed))
  ) freeChoices

||| Do two sub-cubes share at least one free coordinate?
export
sharesFreeDim : SubCube -> SubCube -> Bool
sharesFreeDim a b = any (\c => elem c (freeCoords b)) (freeCoords a)

||| Check if fixed values are compatible on shared fixed coordinates.
||| Two sub-cubes are geometrically compatible if for every coordinate
||| that is fixed in BOTH, the fixed values agree.
export
compatibleFixed : SubCube -> SubCube -> Bool
compatibleFixed a b =
  all checkCoord (fixedValues a)
  where
    lookupFV : Nat -> List (Nat, Bool) -> Maybe Bool
    lookupFV _ [] = Nothing
    lookupFV c ((c', v) :: rest) = if c == c' then Just v else lookupFV c rest

    checkCoord : (Nat, Bool) -> Bool
    checkCoord (c, va) =
      case lookupFV c (fixedValues b) of
        Nothing => True   -- coord is free in b, no conflict
        Just vb => va == vb

||| Structural edge: share >= 1 free coord AND geometrically compatible.
export
structurallyAdjacent : SubCube -> SubCube -> Bool
structurallyAdjacent a b = sharesFreeDim a b && compatibleFixed a b

||| Structural edges: all pairs of sub-cubes sharing >= 1 free coordinate.
||| Returns pairs of indices.
export
structuralEdges : List SubCube -> List (Nat, Nat)
structuralEdges scs =
  let indexed = zip [0 .. minus (length scs) 1] scs
  in concatMap (\(i, sci) =>
    mapMaybe (\(j, scj) =>
      if i < j && structurallyAdjacent sci scj then Just (i, j) else Nothing
    ) indexed
  ) indexed

||| Compute the sub-function truth table for a sub-cube.
||| Given n input variables and a full truth table (Bits32),
||| returns the truth table of the sub-function over the free coords.
export
subFunction : (n : Nat) -> Bits32 -> SubCube -> Bits32
subFunction n tt sc = go (pow2 (length (freeCoords sc))) 0 0
  where
    pow2 : Nat -> Nat
    pow2 0 = 1
    pow2 (S k) = 2 * pow2 k

    ||| Build the full n-bit index from a sub-cube entry index.
    buildIndex : Nat -> Bits32
    buildIndex entry =
      let -- Set fixed coordinate bits
          base = foldl (\acc, (coord, val) =>
            if val then acc .|. shiftL (the Bits32 1) (restrict 31 (natToInteger coord))
                   else acc) (the Bits32 0) (fixedValues sc)
      -- Set free coordinate bits from entry
      in foldl (\acc, (bitIdx, coord) =>
        let entryBits : Bits32 = cast entry
            bitSet = (shiftR entryBits (restrict 31 (natToInteger bitIdx))) .&. 1
        in if bitSet /= 0
           then acc .|. shiftL (the Bits32 1) (restrict 31 (natToInteger coord))
           else acc) base (zip [0 .. minus (length (freeCoords sc)) 1] (freeCoords sc))

    go : Nat -> Nat -> Bits32 -> Bits32
    go 0 _ acc = acc
    go (S r) idx acc =
      let fullIdx = buildIndex idx
          bit = (shiftR tt (restrict 31 (cast fullIdx))) .&. 1
          acc' = if bit /= 0
                 then acc .|. shiftL (the Bits32 1) (restrict 31 (natToInteger idx))
                 else acc
      in go r (S idx) acc'

||| Number of shared free coordinates between two sub-cubes.
export
overlapDim : SubCube -> SubCube -> Nat
overlapDim a b = length (filter (\c => elem c (freeCoords b)) (freeCoords a))

||| Find the index of an element in a list.
export
indexOf : Nat -> List Nat -> Maybe Nat
indexOf x [] = Nothing
indexOf x (y :: ys) = if x == y then Just 0 else map S (indexOf x ys)

||| Look up a fixed value for a coordinate in a sub-cube.
export
lookupFixed : Nat -> SubCube -> Maybe Bool
lookupFixed _ (MkSubCube _ []) = Nothing
lookupFixed coord (MkSubCube free ((c, v) :: rest)) =
  if coord == c then Just v else lookupFixed coord (MkSubCube free rest)

||| Restrict a formula from scSelf's local coordinates to the overlap
||| with scOther. Fixes non-shared free coords using scOther's fixed values.
||| Try to get the local index and fixed value for a non-shared coordinate.
getRestriction : List Nat -> SubCube -> Nat -> Maybe (Nat, Bool)
getRestriction selfFree scOther c =
  case indexOf c selfFree of
    Nothing => Nothing
    Just localIdx =>
      case lookupFixed c scOther of
        Nothing => Nothing
        Just val => Just (localIdx, val)

||| Restrict a formula from scSelf's local coordinates to the overlap
||| with scOther. Fixes non-shared free coords using scOther's fixed values.
export
overlapRestrict : SubCube -> SubCube -> Formula -> Formula
overlapRestrict scSelf scOther f =
  let nonShared = filter (\c => not (elem c (freeCoords scOther))) (freeCoords scSelf)
      restrictions = mapMaybe (getRestriction (freeCoords scSelf) scOther) nonShared
      -- Sort by local index descending (restrict highest first to keep indices stable)
      sorted = sortBy (\(a, _), (b, _) => compare b a) restrictions
  in foldl (\formula, (idx, val) => restrict formula idx val) f sorted

export
Show SubCube where
  show sc = "SubCube(free=" ++ show (freeCoords sc)
    ++ ", fixed=" ++ show (map (\(c,v) => show c ++ "=" ++ if v then "1" else "0") (fixedValues sc)) ++ ")"
