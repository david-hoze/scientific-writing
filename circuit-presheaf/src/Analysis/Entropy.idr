module Analysis.Entropy

import Circuit.Formula
import Circuit.Enumerate
import Data.SortedMap
import Data.List

%default covering

--- Utilities ---

||| Log base 2.
export
log2 : Double -> Double
log2 x = log x / log 2.0

||| Double exponentiation: base^exp.
export
dpow : Double -> Double -> Double
dpow b e = exp (e * log b)

--- Entropy measures ---

||| Shannon entropy H = -sum p_i * log2(p_i) in bits.
export
shannonEntropy : List Double -> Double
shannonEntropy probs =
  negate (foldl (\acc, p => if p > 0.0 then acc + p * log2 p else acc) 0.0 probs)

||| Renyi entropy of order alpha in bits.
||| alpha=0: log2(support size)
||| alpha=1: Shannon entropy (L'Hopital limit)
||| alpha->inf: -log2(max p_i) = log2(sigma)
export
renyiEntropy : Double -> List Double -> Double
renyiEntropy alpha probs =
  if alpha < 1.0e-10 then
    log2 (cast (length (filter (\p => p > 0.0) probs)))
  else if abs (alpha - 1.0) < 1.0e-10 then
    shannonEntropy probs
  else if alpha > 1.0e10 then
    negate (log2 (foldl (\m, p => if p > m then p else m) 0.0 probs))
  else
    let s = foldl (\acc, p => if p > 0.0 then acc + dpow p alpha else acc) 0.0 probs
    in log2 s / (1.0 - alpha)

natToD : Nat -> Double
natToD k = cast k

giniAccum : (Double, Double) -> Nat -> (Double, Double)
giniAccum (cumul, weightedSum) v =
  let dv = natToD v
  in (cumul + dv, weightedSum + cumul + dv)

giniSumAccum : Double -> Nat -> Double
giniSumAccum a v = a + natToD v

giniSum : List Nat -> Double
giniSum xs = foldl giniSumAccum 0.0 xs

||| Gini coefficient (0 = equality, 1 = max inequality).
export
giniCoefficient : List Nat -> Double
giniCoefficient [] = 0.0
giniCoefficient vals =
  let sorted = sortBy compare vals in
  let numV = length sorted in
  let sumAll = giniSum sorted in
  if sumAll == 0.0 || numV == 0 then 0.0
  else let result = foldl giniAccum (the (Double, Double) (0.0, 0.0)) sorted
       in 1.0 - 2.0 * snd result / (natToD numV * sumAll) + 1.0 / natToD numV

--- T_g distribution ---

||| Extract T_g distribution from an EnumResult.
||| Returns list of (truthTable, count) pairs and total formula count.
export
tgDistribution : EnumResult -> (List (Bits32, Nat), Nat)
tgDistribution res =
  let pairs = map (\(tt, fs) => (tt, length fs)) (SortedMap.toList (byTruthTable res))
  in (pairs, totalCount res)

||| Convert T_g counts to probabilities.
export
tgProbs : List Nat -> Nat -> List Double
tgProbs counts totalN =
  let t : Double = cast totalN
  in map (\c => cast c / t) counts

--- Renyi spectrum ---

||| Result of computing Renyi entropy at one alpha value.
public export
record RenyiResult where
  constructor MkRenyiResult
  alphaLabel : String
  alphaVal : Double
  hAlpha : Double
  nAlpha : Double

||| Compute the full Renyi spectrum at standard alpha values.
export
renyiSpectrum : List Double -> List RenyiResult
renyiSpectrum probs =
  map (\(a, label) =>
    let h = renyiEntropy a probs
        n = dpow 2.0 h
    in MkRenyiResult label a h n)
  [ (0.0, "0"), (0.5, "0.5"), (1.0, "1"), (1.5, "1.5")
  , (2.0, "2"), (3.0, "3"), (5.0, "5"), (10.0, "10"), (1.0e12, "inf") ]

--- Exponential fit ---

||| Least-squares exponential fit: y = coeff * base^x.
||| Returns (coefficient, base, R^2).
export
exponentialFit : List (Double, Double) -> (Double, Double, Double)
exponentialFit pts =
  let logPts = mapMaybe (\(x, y) => if y > 0.0 then Just (x, log y) else Nothing) pts
      nf : Double = cast (length logPts)
  in if nf < 2.0 then (1.0, 1.0, 0.0)
     else let sumX = foldl (\a, (x, _) => a + x) 0.0 logPts
              sumLY = foldl (\a, (_, ly) => a + ly) 0.0 logPts
              sumX2 = foldl (\a, (x, _) => a + x * x) 0.0 logPts
              sumXLY = foldl (\a, (x, ly) => a + x * ly) 0.0 logPts
              denom = nf * sumX2 - sumX * sumX
          in if abs denom < 1.0e-15 then (1.0, 1.0, 0.0)
             else let slope = (nf * sumXLY - sumX * sumLY) / denom
                      intercept = (sumLY - slope * sumX) / nf
                      base = exp slope
                      coeff = exp intercept
                      meanLY = sumLY / nf
                      ssTot = foldl (\a, (_, ly) =>
                                a + (ly - meanLY) * (ly - meanLY)) 0.0 logPts
                      ssRes = foldl (\a, (x, ly) =>
                                let pred = intercept + slope * x
                                in a + (ly - pred) * (ly - pred)) 0.0 logPts
                      r2 = if ssTot > 1.0e-15 then 1.0 - ssRes / ssTot else 0.0
                  in (coeff, base, r2)
