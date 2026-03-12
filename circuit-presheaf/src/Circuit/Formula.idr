module Circuit.Formula

import Data.Vect
import Data.Bits
import Data.Fin

%default total

||| Boolean formula AST over {AND, OR, NOT} basis with d input variables.
public export
data Formula : Type where
  Input : (idx : Nat) -> Formula
  Const : Bool -> Formula
  Not   : Formula -> Formula
  And   : Formula -> Formula -> Formula
  Or    : Formula -> Formula -> Formula

||| Number of internal nodes (gates). Leaves have size 0.
public export
size : Formula -> Nat
size (Input _) = 0
size (Const _) = 0
size (Not c)   = 1 + size c
size (And l r) = 1 + size l + size r
size (Or l r)  = 1 + size l + size r

||| Evaluate a formula on a concrete input assignment.
public export
eval : {d : Nat} -> Formula -> Vect d Bool -> Bool
eval (Input i) xs = case natToFin i d of
                         Just fi => index fi xs
                         Nothing => False
eval (Const b) _  = b
eval (Not c)   xs = not (eval c xs)
eval (And l r) xs = eval l xs && eval r xs
eval (Or l r)  xs = eval l xs || eval r xs

--- Truth table as bit integer (Bits32) for d <= 5 ---

pow2 : Nat -> Nat
pow2 0 = 1
pow2 (S k) = 2 * pow2 k

||| All-ones mask for dimension d: lower 2^d bits set.
public export
mask : (d : Nat) -> Bits32
mask d = shiftL (the Bits32 1) (restrict 31 (natToInteger (pow2 d))) - 1

||| Truth table for input variable i in dimension d.
||| Bit j is set iff bit i of j is set.
public export
inputTT : (d : Nat) -> (i : Nat) -> Bits32
inputTT d i = go (pow2 d) 0 0
  where
    step : Nat
    step = pow2 (i + 1)
    half : Nat
    half = pow2 i
    go : (remaining : Nat) -> (pos : Nat) -> Bits32 -> Bits32
    go 0 _ acc = acc
    go (S r) pos acc =
      let posInPeriod = mod pos step
          bitSet = posInPeriod >= half && posInPeriod < step
          bitVal : Bits32 = if bitSet
                            then shiftL (the Bits32 1) (restrict 31 (natToInteger pos))
                            else 0
      in go r (S pos) (acc .|. bitVal)

||| Compute the truth table of a formula as a Bits32.
||| Bit i of the result = eval(formula, input combo i).
||| For dimension d, only the lower 2^d bits are meaningful.
public export
truthTable : (d : Nat) -> Formula -> Bits32
truthTable d (Input i) = inputTT d i
truthTable d (Const False) = 0
truthTable d (Const True) = mask d
truthTable d (Not c) = xor (truthTable d c) (mask d)
truthTable d (And l r) = (truthTable d l) .&. (truthTable d r)
truthTable d (Or l r) = (truthTable d l) .|. (truthTable d r)

||| Number of distinct functions for dimension d: 2^(2^d).
public export
numFunctions : (d : Nat) -> Nat
numFunctions d = pow2 (pow2 d)

||| Show instance for Formula (for debugging).
export
Show Formula where
  show (Input i) = "x" ++ show i
  show (Const True) = "1"
  show (Const False) = "0"
  show (Not c) = "NOT(" ++ show c ++ ")"
  show (And l r) = "AND(" ++ show l ++ "," ++ show r ++ ")"
  show (Or l r) = "OR(" ++ show l ++ "," ++ show r ++ ")"
