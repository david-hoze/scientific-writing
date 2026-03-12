module Algebra.M2Parse

import Data.String
import Data.List

%default covering

||| Result of parsing M2 output.
public export
data M2Result
  = Satisfiable
  | Unsatisfiable
  | GBContainsOne
  | HilbertZero
  | ParseError String

export
Show M2Result where
  show Satisfiable = "SAT"
  show Unsatisfiable = "UNSAT"
  show GBContainsOne = "GB contains {1} (unsatisfiable)"
  show HilbertZero = "Hilbert(0) = 0 (unsatisfiable)"
  show (ParseError s) = "Parse error: " ++ s

||| Parse M2 output text.
||| Looks for SAT/UNSAT markers, {1} in GB output, and Hilbert values.
export
parseM2Output : String -> List M2Result
parseM2Output text =
  let ls = lines text
  in mapMaybe parseLine ls
  where
    parseLine : String -> Maybe M2Result
    parseLine l =
      let trimmed = trim l
      in if isInfixOf "UNSAT" trimmed then Just Unsatisfiable
         else if isInfixOf "SAT" trimmed then Just Satisfiable
         else if isInfixOf "| 1 |" trimmed || trimmed == "| 1 |"
           then Just GBContainsOne
         else if trimmed == "0" then Just HilbertZero
         else Nothing

||| Check if any result indicates unsatisfiability.
export
isUnsat : List M2Result -> Bool
isUnsat results = any isU results
  where
    isU : M2Result -> Bool
    isU Unsatisfiable = True
    isU GBContainsOne = True
    isU HilbertZero = True
    isU _ = False
