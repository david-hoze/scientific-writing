module Main

import Circuit.Formula
import Circuit.Canonical
import Circuit.Restriction
import Circuit.Enumerate
import Analysis.RestrictionImage
import Analysis.SubCube
import Analysis.CompatCSP
import Algebra.M2Gen
import Algebra.M2Parse
import Algebra.NSDriver
import Data.SortedMap
import Data.List
import Data.String
import System
import System.File

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
        Just s => runBent (cast {to=Nat} s) Nothing
        _ => putStrLn "Error: --size must be a positive integer"
    [_, "bent", "--size", sStr, "--m2gen", outFile] =>
      case parsePositive sStr of
        Just s => runBent (cast {to=Nat} s) (Just outFile)
        _ => putStrLn "Error: --size must be a positive integer"
    [_, "bent"] => runBent 4 Nothing
    [_, "m2run", scriptFile] => runM2Command scriptFile
    _ => do putStrLn "circuit-presheaf - Boolean formula presheaf analysis"
            putStrLn ""
            putStrLn "Usage:"
            putStrLn "  circuit-presheaf enumerate --dim D --max-size S"
            putStrLn "  circuit-presheaf scaling --max-size S"
            putStrLn "  circuit-presheaf convergence --dim D --max-size S"
            putStrLn "  circuit-presheaf bent --size S"
            putStrLn "  circuit-presheaf bent --size S --m2gen FILE.m2"
            putStrLn "  circuit-presheaf m2run FILE.m2"
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

    runBentM2 : Nat -> String -> IO ()
    runBentM2 maxS outFile = do
      putStrLn $ "runBentM2 called with maxS=" ++ show maxS
      let bentTT : Bits32 = 0x7888
      let subRes = enumerate 2 maxS
      putStrLn $ "  Total: " ++ show (totalCount subRes)
      let scs = allSubCubes 4 2
      let domains = map (getDomain 4 bentTT subRes) scs
      putStrLn $ "  Sizes: " ++ show (map length domains)
      let nodes = zipWith (\i, dom => MkCSPNode i (map canonical dom))
                    [0 .. minus (length scs) 1] domains
      putStrLn $ "  Nodes: " ++ show (length nodes)
      let edges = structuralEdges scs
      putStrLn $ "  Edges: " ++ show (length edges)
      putStrLn "Computing overlap groups..."
      let edgeGrps = map (\(i, j) =>
            let sci = orDefault (MkSubCube [] []) (indexList i scs)
                scj = orDefault (MkSubCube [] []) (indexList j scs)
                domI = orDefault [] (indexList i domains)
                domJ = orDefault [] (indexList j domains)
            in MkCSPEdgeGroups i j
                 (overlapGroups sci scj domI)
                 (overlapGroups scj sci domJ)) edges
      putStrLn $ "  Groups: " ++ show (length edgeGrps)
      putStrLn "Generating M2 script..."
      let script = generateM2FromCSP nodes edgeGrps
      putStrLn $ "Script length: " ++ show (length script)
      Right () <- writeFile outFile script
        | Left err => putStrLn $ "Error writing: " ++ show err
      putStrLn $ "M2 script written to: " ++ outFile
      let totalVars = foldl (\acc, n => acc + length (domain n)) 0 nodes
      if totalVars <= 100
        then do
          putStrLn "Running M2 (small instance)..."
          Right output <- runM2 outFile
            | Left msg => putStrLn msg
          let results = parseM2Output output
          putStrLn $ "M2 results: " ++ show results
          if isUnsat results
            then putStrLn "System is UNSATISFIABLE"
            else putStrLn "System is SATISFIABLE (or inconclusive)"
        else putStrLn $ "Skipping M2 execution (" ++ show totalVars ++ " variables, run manually: M2 --script " ++ outFile

    runBent : Nat -> Maybe String -> IO ()
    runBent maxS m2file = do
      let bentTT : Bits32 = 0x7888
      putStrLn $ "BENT analysis: n=4, d=2, s<=" ++ show maxS
      putStrLn $ "Function: (x0 AND x1) XOR (x2 AND x3), TT=0x7888"
      let csp = buildCSPForFunction 4 2 maxS bentTT
      putStrLn $ "Sub-cubes: " ++ show (nodeCount csp)
      putStrLn $ "Structural edges: " ++ show (edgeCount csp)
      putStrLn $ "Fully compatible: " ++ show (fullyCompatible csp)
      putStrLn $ "Partially compatible: " ++ show (partiallyCompatible csp)
      putStrLn $ "Fully incompatible: " ++ show (fullyIncompatible csp)
      case m2file of
        Nothing => pure ()
        Just outFile => runBentM2 maxS outFile

    runM2Command : String -> IO ()
    runM2Command scriptFile = do
      Right output <- runM2 scriptFile
        | Left msg => putStrLn msg
      let results = parseM2Output output
      putStrLn $ "M2 results: " ++ show results
      if isUnsat results
        then putStrLn "System is UNSATISFIABLE"
        else putStrLn "System is SATISFIABLE (or inconclusive)"
