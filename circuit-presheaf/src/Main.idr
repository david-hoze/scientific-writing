module Main

import Circuit.Formula
import Circuit.Canonical
import Circuit.Restriction
import Circuit.Enumerate
import Analysis.RestrictionImage
import Analysis.SubCube
import Analysis.CompatCSP
import Data.SortedMap
import Data.List
import Data.String
import System

%default covering

showSizeCounts : EnumResult -> String
showSizeCounts res =
  let counts = sizeCounts res
  in fastConcat (intersperse "\n" (map (\(s, n) => "  size " ++ show s ++ ": " ++ show n ++ " formulas") counts))

showRIResult : RIResult -> String
showRIResult ri =
  "  |U| = " ++ show (universeSize ri)
  ++ ", M = " ++ show (maxImage ri)
  ++ ", sigma = " ++ show (sigma ri)
  ++ ", alpha = " ++ show (topShare ri)

main : IO ()
main = do
  args <- getArgs
  case args of
    [_, "enumerate", "--dim", dStr, "--max-size", sStr] =>
      case (parsePositive dStr, parsePositive sStr) of
        (Just d, Just s) => runEnumerate (cast {to=Nat} d) (cast {to=Nat} s)
        _ => putStrLn "Error: --dim and --max-size must be positive integers"
    [_, "enumerate"] => runEnumerate 3 4
    [_, "scaling", "--max-size", sStr] =>
      case parsePositive sStr of
        Just s => runScaling (cast {to=Nat} s)
        _ => putStrLn "Error: --max-size must be a positive integer"
    [_, "scaling"] => runScaling 4
    [_, "convergence", "--dim", dStr, "--max-size", sStr] =>
      case (parsePositive dStr, parsePositive sStr) of
        (Just d, Just s) => runConvergence (cast {to=Nat} d) (cast {to=Nat} s)
        _ => putStrLn "Error: --dim and --max-size must be positive integers"
    [_, "convergence"] => runConvergence 3 5
    [_, "bent", "--size", sStr] =>
      case parsePositive sStr of
        Just s => runBent (cast {to=Nat} s)
        _ => putStrLn "Error: --size must be a positive integer"
    [_, "bent"] => runBent 4
    _ => do putStrLn "circuit-presheaf - Boolean formula presheaf analysis"
            putStrLn ""
            putStrLn "Usage:"
            putStrLn "  circuit-presheaf enumerate --dim D --max-size S"
            putStrLn "  circuit-presheaf scaling --max-size S"
            putStrLn "  circuit-presheaf convergence --dim D --max-size S"
            putStrLn "  circuit-presheaf bent --size S"
  where
    runEnumerate : Nat -> Nat -> IO ()
    runEnumerate d maxS = do
      putStrLn $ "Enumerating d=" ++ show d ++ " up to size " ++ show maxS
      let res = enumerate d maxS
      putStrLn $ "Total formulas: " ++ show (totalCount res)
      putStrLn $ "Functions covered: " ++ show (functionsCovered res)
      putStrLn $ "By size:"
      putStrLn $ showSizeCounts res

    runScaling : Nat -> IO ()
    runScaling maxS = do
      putStrLn $ "Scaling law sigma(s,d) for s<=" ++ show maxS
      putStrLn $ "d | |U| | M | sigma | alpha"
      putStrLn $ "--|-----|---|-------|------"
      scaleDim 2 maxS
      scaleDim 3 maxS
      scaleDim 4 maxS
      where
        scaleDim : Nat -> Nat -> IO ()
        scaleDim d ms = do
          let res = enumerate d ms
          let ri = analyzeRestrictions d res
          putStrLn $ show d ++ " | " ++ show (universeSize ri)
            ++ " | " ++ show (maxImage ri)
            ++ " | " ++ show (sigma ri)
            ++ " | " ++ show (topShare ri)

    runConvergence : Nat -> Nat -> IO ()
    runConvergence d maxS = do
      putStrLn $ "Size convergence at d=" ++ show d
      putStrLn $ "s<= | |U| | M | sigma"
      putStrLn $ "----|------|---|------"
      let results = analyzeBySize d maxS
      traverse_ printRow results
      where
        printRow : (Nat, RIResult) -> IO ()
        printRow (s, ri) =
          putStrLn $ show s ++ " | " ++ show (universeSize ri)
            ++ " | " ++ show (maxImage ri)
            ++ " | " ++ show (sigma ri)

    runBent : Nat -> IO ()
    runBent maxS = do
      -- Inner product bent function: (x0 AND x1) XOR (x2 AND x3)
      -- Truth table: 0x7888
      let bentTT : Bits32 = 0x7888
      putStrLn $ "BENT analysis: n=4, d=2, s<=" ++ show maxS
      putStrLn $ "Function: (x0 AND x1) XOR (x2 AND x3), TT=0x7888"
      let csp = buildCSPForFunction 4 2 maxS bentTT
      putStrLn $ "Sub-cubes: " ++ show (nodeCount csp)
      putStrLn $ "Structural edges: " ++ show (edgeCount csp)
      putStrLn $ "Fully compatible: " ++ show (fullyCompatible csp)
      putStrLn $ "Partially compatible: " ++ show (partiallyCompatible csp)
      putStrLn $ "Fully incompatible: " ++ show (fullyIncompatible csp)
