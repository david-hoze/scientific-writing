-- | Repetition code — the simplest CSS code for validation.
--
-- The repetition code of distance d encodes 1 logical qubit into d
-- physical qubits. It detects only phase-flip (Z) errors:
-- H_X is empty, H_Z is the chain parity check matrix.
module QEC.Code.Repetition
  ( repetitionCode
  ) where

import QEC.GF2
import QEC.GF2.Matrix
import QEC.Code.CSS

-- | Construct a repetition code of distance @d@.
-- Parameters: [[d, 1, d]] (phase-flip only).
--
-- H_Z is (d-1) x d with entries H_Z[i,i] = H_Z[i,i+1] = 1 (chain parity checks).
-- H_X is 0 x d (no X stabilizers — only detects Z errors).
repetitionCode :: Int -> CSSCode
repetitionCode d
  | d < 1     = error "repetitionCode: d must be >= 1"
  | otherwise = case mkCSSCode hx hz of
      Left err   -> error $ "repetitionCode: unexpected error: " ++ show err
      Right code -> code
  where
    hx = bmZero 0 d
    hz | d <= 1    = bmZero 0 d
       | otherwise = bmFromRows
           [ bvFromList [ if j == i || j == i + 1 then GF2 True else GF2 False
                        | j <- [0 .. d - 1] ]
           | i <- [0 .. d - 2]
           ]
