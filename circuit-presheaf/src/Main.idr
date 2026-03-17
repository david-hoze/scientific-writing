module Main

import Circuit.Formula
import Circuit.Canonical
import Circuit.Restriction
import Circuit.Enumerate
import Analysis.RestrictionImage
import Analysis.SubCube
import Analysis.CompatCSP
import Analysis.Entropy
import Analysis.AffineRestriction
import Analysis.GraphTopology
import Analysis.VerifiedStats
import Algebra.M2Gen
import Algebra.M2Parse
import Algebra.NSDriver
import Verified.CSP
import Verified.Solver
import Verified.Exhaustive
import Data.Vect
import Data.SortedMap
import Data.SortedSet
import Data.List
import Data.Bits
import Data.String
import System
import System.File

%default covering

||| Infer the number of variables from a truth table value.
||| A truth table of an n-variable function uses 2^n bits,
||| so the TT value is < 2^(2^n).
||| Default: n=4 for TT <= 65535 (main research target),
||| n=5 for larger TT values.
inferN : Bits32 -> Nat
inferN tt = if tt <= 65535 then 4 else 5

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
    [_, "bent-sub", "--size", sStr, "--nodes", nStr, "--m2gen", outFile] =>
      case (parsePositive sStr, parsePositive nStr) of
        (Just s, Just n) => runBentSub (cast {to=Nat} s) (cast {to=Nat} n) outFile
        _ => putStrLn "Error: --size and --nodes must be positive integers"
    [_, "bent-sub", "--size", sStr, "--m2gen", outFile] =>
      case parsePositive sStr of
        Just s => runBentSub (cast {to=Nat} s) 6 outFile
        _ => putStrLn "Error: --size must be a positive integer"
    [_, "test", "--tt", ttStr, "--dim", dStr, "--size", sStr] =>
      case (parsePositive ttStr, parsePositive dStr, parsePositive sStr) of
        (Just tt, Just d, Just s) => runTest (cast {to=Bits32} tt) (cast {to=Nat} d) (cast {to=Nat} s)
        _ => putStrLn "Error: --tt, --dim, --size must be positive integers"
    [_, "test", "--tt", ttStr, "--size", sStr] =>
      case (parsePositive ttStr, parsePositive sStr) of
        (Just tt, Just s) => runTest (cast {to=Bits32} tt) 2 (cast {to=Nat} s)
        _ => putStrLn "Error: --tt and --size must be positive integers"
    [_, "test", "--tt", ttStr] =>
      case parsePositive ttStr of
        Just tt => runTest (cast {to=Bits32} tt) 2 4
        _ => putStrLn "Error: --tt must be a positive integer"
    [_, "test-m2", "--tt", ttStr, "--dim", dStr, "--size", sStr, "--nodes", nStr, "--m2gen", outFile] =>
      case (parsePositive ttStr, parsePositive dStr, parsePositive sStr, parsePositive nStr) of
        (Just tt, Just d, Just s, Just n) => runTestM2 (cast {to=Bits32} tt) (cast {to=Nat} d) (cast {to=Nat} s) (cast {to=Nat} n) outFile
        _ => putStrLn "Error: all numeric args must be positive integers"
    [_, "test-m2", "--tt", ttStr, "--dim", dStr, "--size", sStr, "--m2gen", outFile] =>
      case (parsePositive ttStr, parsePositive dStr, parsePositive sStr) of
        (Just tt, Just d, Just s) => runTestM2 (cast {to=Bits32} tt) (cast {to=Nat} d) (cast {to=Nat} s) 24 outFile
        _ => putStrLn "Error: all numeric args must be positive integers"
    [_, "test-m2", "--tt", ttStr, "--size", sStr, "--nodes", nStr, "--m2gen", outFile] =>
      case (parsePositive ttStr, parsePositive sStr, parsePositive nStr) of
        (Just tt, Just s, Just n) => runTestM2 (cast {to=Bits32} tt) 2 (cast {to=Nat} s) (cast {to=Nat} n) outFile
        _ => putStrLn "Error: --tt, --size, --nodes must be positive integers"
    [_, "test-m2", "--tt", ttStr, "--size", sStr, "--m2gen", outFile] =>
      case (parsePositive ttStr, parsePositive sStr) of
        (Just tt, Just s) => runTestM2 (cast {to=Bits32} tt) 2 (cast {to=Nat} s) 24 outFile
        _ => putStrLn "Error: --tt and --size must be positive integers"
    [_, "solve", "--tt", ttStr, "--dim", dStr, "--size", sStr] =>
      case (parsePositive ttStr, parsePositive dStr, parsePositive sStr) of
        (Just tt, Just d, Just s) => runSolve (cast {to=Bits32} tt) (cast {to=Nat} d) (cast {to=Nat} s)
        _ => putStrLn "Error: all numeric args must be positive integers"
    [_, "solve", "--tt", ttStr, "--size", sStr] =>
      case (parsePositive ttStr, parsePositive sStr) of
        (Just tt, Just s) => runSolve (cast {to=Bits32} tt) 2 (cast {to=Nat} s)
        _ => putStrLn "Error: --tt and --size must be positive integers"
    [_, "vsolve", "--tt", ttStr, "--dim", dStr, "--size", sStr] =>
      case (parsePositive ttStr, parsePositive dStr, parsePositive sStr) of
        (Just tt, Just d, Just s) => runVSolve (cast {to=Bits32} tt) (cast {to=Nat} d) (cast {to=Nat} s)
        _ => putStrLn "Error: all numeric args must be positive integers"
    [_, "vsolve", "--tt", ttStr, "--size", sStr] =>
      case (parsePositive ttStr, parsePositive sStr) of
        (Just tt, Just s) => runVSolve (cast {to=Bits32} tt) 2 (cast {to=Nat} s)
        _ => putStrLn "Error: --tt and --size must be positive integers"
    [_, "cert", "--tt", ttStr, "--dim", dStr, "--size", sStr] =>
      case (parsePositive ttStr, parsePositive dStr, parsePositive sStr) of
        (Just tt, Just d, Just s) => runCert (cast {to=Bits32} tt) (cast {to=Nat} d) (cast {to=Nat} s)
        _ => putStrLn "Error: all numeric args must be positive integers"
    [_, "cert", "--tt", ttStr, "--size", sStr] =>
      case (parsePositive ttStr, parsePositive sStr) of
        (Just tt, Just s) => runCert (cast {to=Bits32} tt) 2 (cast {to=Nat} s)
        _ => putStrLn "Error: --tt and --size must be positive integers"
    [_, "profiles", "--tt", ttStr, "--dim", dStr, "--size", sStr] =>
      case (parsePositive ttStr, parsePositive dStr, parsePositive sStr) of
        (Just tt, Just d, Just s) => runProfiles (cast {to=Bits32} tt) (cast {to=Nat} d) (cast {to=Nat} s) Nothing
        _ => putStrLn "Error: all numeric args must be positive integers"
    [_, "profiles", "--tt", ttStr, "--dim", dStr, "--size", sStr, "--m2gen", outFile] =>
      case (parsePositive ttStr, parsePositive dStr, parsePositive sStr) of
        (Just tt, Just d, Just s) => runProfiles (cast {to=Bits32} tt) (cast {to=Nat} d) (cast {to=Nat} s) (Just outFile)
        _ => putStrLn "Error: all numeric args must be positive integers"
    [_, "scan-solve", "--vars", nStr, "--dim", dStr, "--size", sStr] =>
      case (parsePositive nStr, parsePositive dStr, parsePositive sStr) of
        (Just nv, Just d, Just s) => runScanSolve (cast {to=Nat} nv) (cast {to=Nat} d) (cast {to=Nat} s)
        _ => putStrLn "Error: --vars, --dim and --size must be positive integers"
    [_, "scan-solve", "--dim", dStr, "--size", sStr] =>
      case (parsePositive dStr, parsePositive sStr) of
        (Just d, Just s) => runScanSolve 4 (cast {to=Nat} d) (cast {to=Nat} s)
        _ => putStrLn "Error: --dim and --size must be positive integers"
    [_, "scan-solve"] => runScanSolve 4 3 4
    [_, "analyze", "--tt", ttStr, "--dim", dStr, "--size", sStr] =>
      case (parsePositive ttStr, parsePositive dStr, parsePositive sStr) of
        (Just tt, Just d, Just s) => runAnalyze (cast {to=Bits32} tt) (cast {to=Nat} d) (cast {to=Nat} s)
        _ => putStrLn "Error: all numeric args must be positive integers"
    [_, "analyze", "--tt", ttStr, "--size", sStr] =>
      case (parsePositive ttStr, parsePositive sStr) of
        (Just tt, Just s) => runAnalyze (cast {to=Bits32} tt) 3 (cast {to=Nat} s)
        _ => putStrLn "Error: --tt and --size must be positive integers"
    [_, "scan"] => runScan 4 2
    [_, "scan", "--top", kStr] =>
      case parsePositive kStr of
        Just k => runScanTop 4 2 (cast {to=Nat} k)
        _ => putStrLn "Error: --top must be a positive integer"
    [_, "m2run", scriptFile] => runM2Command scriptFile
    -- New analysis commands
    [_, "entropy", "--dim", dStr, "--max-size", sStr] =>
      case (parsePositive dStr, parsePositive sStr) of
        (Just d, Just s) => runEntropy (cast {to=Nat} d) (cast {to=Nat} s)
        _ => putStrLn "Error: --dim and --max-size must be positive integers"
    [_, "entropy"] => runEntropy 3 4
    [_, "renyi", "--dim", dStr, "--max-size", sStr] =>
      case (parsePositive dStr, parsePositive sStr) of
        (Just d, Just s) => runRenyi (cast {to=Nat} d) (cast {to=Nat} s)
        _ => putStrLn "Error: --dim and --max-size must be positive integers"
    [_, "renyi"] => runRenyi 3 4
    [_, "neff-scale", "--max-size", sStr] =>
      case parsePositive sStr of
        Just s => runNeffScale (cast {to=Nat} s)
        _ => putStrLn "Error: --max-size must be a positive integer"
    [_, "neff-scale"] => runNeffScale 4
    [_, "sigma-affine", "--dim", dStr, "--max-size", sStr, "--mode", modeStr] =>
      case (parsePositive dStr, parsePositive sStr) of
        (Just d, Just s) =>
          let mode = if modeStr == "affine" then AffineMode else ValueMode
          in runSigmaAffine (cast {to=Nat} d) (cast {to=Nat} s) mode
        _ => putStrLn "Error: --dim and --max-size must be positive integers"
    [_, "sigma-affine", "--dim", dStr, "--max-size", sStr] =>
      case (parsePositive dStr, parsePositive sStr) of
        (Just d, Just s) => runSigmaAffine (cast {to=Nat} d) (cast {to=Nat} s) AffineMode
        _ => putStrLn "Error: --dim and --max-size must be positive integers"
    [_, "sigma-depth", "--max-size", sStr] =>
      case parsePositive sStr of
        Just s => runSigmaDepth (cast {to=Nat} s)
        _ => putStrLn "Error: --max-size must be a positive integer"
    [_, "sigma-depth"] => runSigmaDepth 4
    [_, "sigma-detail", "--dim", dStr, "--max-size", sStr] =>
      case (parsePositive dStr, parsePositive sStr) of
        (Just d, Just s) => runSigmaDetail (cast {to=Nat} d) (cast {to=Nat} s)
        _ => putStrLn "Error: --dim and --max-size must be positive integers"
    [_, "graph-topo", "--tt", ttStr, "--dim", dStr, "--size", sStr] =>
      case (parsePositive ttStr, parsePositive dStr, parsePositive sStr) of
        (Just tt, Just d, Just s) => runGraphTopo (cast {to=Bits32} tt) (cast {to=Nat} d) (cast {to=Nat} s)
        _ => putStrLn "Error: --tt, --dim, --size must be positive integers"
    [_, "graph-topo", "--tt", ttStr, "--size", sStr] =>
      case (parsePositive ttStr, parsePositive sStr) of
        (Just tt, Just s) => runGraphTopo (cast {to=Bits32} tt) 3 (cast {to=Nat} s)
        _ => putStrLn "Error: --tt and --size must be positive integers"
    [_, "comm", "--tt", ttStr, "--dim", dStr, "--size", sStr] =>
      case (parsePositive ttStr, parsePositive dStr, parsePositive sStr) of
        (Just tt, Just d, Just s) => runComm (cast {to=Bits32} tt) (cast {to=Nat} d) (cast {to=Nat} s)
        _ => putStrLn "Error: --tt, --dim, --size must be positive integers"
    [_, "comm", "--tt", ttStr, "--size", sStr] =>
      case (parsePositive ttStr, parsePositive sStr) of
        (Just tt, Just s) => runComm (cast {to=Bits32} tt) 3 (cast {to=Nat} s)
        _ => putStrLn "Error: --tt and --size must be positive integers"
    [_, "vstats", "--file", fPath, "--sample", nStr] =>
      case parsePositive nStr of
        Just n => runVStats fPath (cast {to=Nat} n)
        _ => putStrLn "Error: --sample must be a positive integer"
    [_, "vstats", "--file", fPath] => runVStats fPath 50
    [_, "scan5", "--dim", dStr, "--size", sStr, "--strategy", strat, "--count", cStr] =>
      case (parsePositive dStr, parsePositive sStr, parsePositive cStr) of
        (Just d, Just s, Just c) => runScan5 (cast {to=Nat} d) (cast {to=Nat} s) strat (cast {to=Nat} c)
        _ => putStrLn "Error: --dim, --size, --count must be positive integers"
    [_, "scan5", "--dim", dStr, "--size", sStr, "--strategy", strat] =>
      case (parsePositive dStr, parsePositive sStr) of
        (Just d, Just s) => runScan5 (cast {to=Nat} d) (cast {to=Nat} s) strat 100
        _ => putStrLn "Error: --dim and --size must be positive integers"
    [_, "scan5"] => runScan5 3 4 "lifted" 100
    [_, "compression", "--dim", dStr, "--max-size", sStr] =>
      case (parsePositive dStr, parsePositive sStr) of
        (Just d, Just s) => runCompression (cast {to=Nat} d) (cast {to=Nat} s)
        _ => putStrLn "Error: --dim and --max-size must be positive integers"
    [_, "compression"] => runCompression 3 4
    _ => do putStrLn "circuit-presheaf - Boolean formula presheaf analysis"
            putStrLn ""
            putStrLn "Usage:"
            putStrLn "  circuit-presheaf enumerate --dim D --max-size S"
            putStrLn "  circuit-presheaf scaling --max-size S"
            putStrLn "  circuit-presheaf convergence --dim D --max-size S"
            putStrLn "  circuit-presheaf bent --size S [--m2gen FILE.m2]"
            putStrLn "  circuit-presheaf test --tt TT [--dim D] [--size S]"
            putStrLn "  circuit-presheaf solve --tt TT [--dim D] --size S"
            putStrLn "  circuit-presheaf vsolve --tt TT [--dim D] --size S"
            putStrLn "  circuit-presheaf cert --tt TT [--dim D] --size S"
            putStrLn "  circuit-presheaf profiles --tt TT --dim D --size S"
            putStrLn "  circuit-presheaf analyze --tt TT [--dim D] --size S"
            putStrLn "  circuit-presheaf scan [--top K]"
            putStrLn "  circuit-presheaf scan-solve [--vars N] [--dim D] --size S"
            putStrLn "  circuit-presheaf m2run FILE.m2"
            putStrLn ""
            putStrLn "  -- Analysis (ported from Python) --"
            putStrLn "  circuit-presheaf entropy [--dim D --max-size S]"
            putStrLn "  circuit-presheaf renyi [--dim D --max-size S]"
            putStrLn "  circuit-presheaf neff-scale [--max-size S]"
            putStrLn "  circuit-presheaf sigma-affine --dim D --max-size S [--mode value|affine]"
            putStrLn "  circuit-presheaf sigma-depth [--max-size S]"
            putStrLn "  circuit-presheaf sigma-detail --dim D --max-size S"
            putStrLn "  circuit-presheaf graph-topo --tt TT [--dim D] --size S"
            putStrLn "  circuit-presheaf comm --tt TT [--dim D] --size S"
            putStrLn "  circuit-presheaf vstats --file F [--sample N]"
            putStrLn "  circuit-presheaf scan5 [--dim D --size S --strategy lifted|range --count N]"
            putStrLn "  circuit-presheaf compression [--dim D --max-size S]"
  where
    hexDigit : Bits32 -> Char
    hexDigit 0 = '0'; hexDigit 1 = '1'; hexDigit 2 = '2'; hexDigit 3 = '3'
    hexDigit 4 = '4'; hexDigit 5 = '5'; hexDigit 6 = '6'; hexDigit 7 = '7'
    hexDigit 8 = '8'; hexDigit 9 = '9'; hexDigit 10 = 'a'; hexDigit 11 = 'b'
    hexDigit 12 = 'c'; hexDigit 13 = 'd'; hexDigit 14 = 'e'; hexDigit 15 = 'f'
    hexDigit _ = '?'

    toHex : Bits32 -> String
    toHex n = "0x" ++ go n ""
      where
        go : Bits32 -> String -> String
        go 0 "" = "0"
        go 0 acc = acc
        go v acc = go (v `shiftR` 4) (singleton (hexDigit (v .&. 0xF)) ++ acc)
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
                    (case length scs of Z => []; S k => [0 .. k]) domains
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
      -- Structural (DAG-isomorphism) classification
      let csp = buildCSPForFunction 4 2 maxS bentTT
      putStrLn $ "Sub-cubes: " ++ show (nodeCount csp)
      putStrLn $ "Empty-domain nodes: " ++ show (emptyDomainNodes csp)
      putStrLn $ "Structural edges: " ++ show (edgeCount csp)
      putStrLn $ "  [structural] compat/partial/incompat: "
        ++ show (fullyCompatible csp) ++ " / "
        ++ show (partiallyCompatible csp) ++ " / "
        ++ show (fullyIncompatible csp)
      -- Semantic (function-level) classification
      let sem = buildCSPSemantic 4 2 maxS bentTT
      putStrLn $ "  [semantic]   compat/partial/incompat: "
        ++ show (fullyCompatible sem) ++ " / "
        ++ show (partiallyCompatible sem) ++ " / "
        ++ show (fullyIncompatible sem)
      case m2file of
        Nothing => pure ()
        Just outFile => runBentM2 maxS outFile

    runBentSub : Nat -> Nat -> String -> IO ()
    runBentSub maxS numNodes outFile = do
      let bentTT : Bits32 = 0x7888
      putStrLn $ "BENT sub-instance: s<=" ++ show maxS ++ ", extracting " ++ show numNodes ++ " nodes"
      let cspData = buildCSPData 4 2 maxS bentTT
      let res = cspResult cspData
      putStrLn $ "Full CSP: " ++ show (nodeCount res) ++ " nodes, " ++ show (edgeCount res) ++ " edges"
      putStrLn $ "  Empty domains: " ++ show (emptyDomainNodes res)
      -- Find the node with the largest domain as seed
      let topNodes = topNodesByDomain 1 cspData
      let seed : Nat = case topNodes of (s :: _) => s; [] => 0
      putStrLn $ "Seed node: " ++ show seed
      -- BFS expand to numNodes neighbors
      let subNodes = bfsExpand seed numNodes cspData
      putStrLn $ "Sub-instance nodes: " ++ show subNodes
      let subCSP = extractSubInstance subNodes cspData
      -- Build CSPNode list for M2Gen
      let m2Nodes = map (\(i, dom) => MkCSPNode i dom) (cspNodes subCSP)
      let totalVars = foldl (\acc, n => acc + length (domain n)) 0 m2Nodes
      putStrLn $ "Variables: " ++ show totalVars
      putStrLn $ "Edges: " ++ show (length (cspEdgeGroups subCSP))
      let script = generateM2FromCSP m2Nodes (cspEdgeGroups subCSP)
      Right () <- writeFile outFile script
        | Left err => putStrLn $ "Error writing: " ++ show err
      putStrLn $ "M2 script written to: " ++ outFile
      if totalVars <= 100
        then do
          putStrLn "Running M2..."
          Right output <- runM2 outFile
            | Left msg => putStrLn msg
          let results = parseM2Output output
          putStrLn $ "M2 results: " ++ show results
          if isUnsat results
            then putStrLn "Sub-instance is UNSATISFIABLE"
            else putStrLn "Sub-instance is SATISFIABLE"
        else putStrLn $ "Too large for auto-run (" ++ show totalVars ++ " vars). Run: M2 --script " ++ outFile

    ||| Count distinct sub-function truth tables for a given global function.
    countDistinctSubFunctions : (n : Nat) -> (d : Nat) -> Bits32 -> Nat
    countDistinctSubFunctions n d tt =
      let scs = allSubCubes n d
          tts = map (subFunction n tt) scs
          unique = foldl (\s, t => SortedSet.insert t s) SortedSet.empty tts
      in length (SortedSet.toList unique)

    runTest : Bits32 -> Nat -> Nat -> IO ()
    runTest targetTT d maxS = do
      let n : Nat = inferN targetTT
      putStrLn $ "Function TT=" ++ toHex targetTT ++ " (" ++ show targetTT ++ " dec)"
      putStrLn $ "  n=" ++ show n ++ ", d=" ++ show d
      let nDistinct = countDistinctSubFunctions n d targetTT
      let maxSubs : Nat = numFunctions d
      putStrLn $ "  Distinct sub-functions: " ++ show nDistinct ++ "/" ++ show maxSubs
      putStrLn $ "s<= | empty | struct C/P/I | semantic C/I"
      putStrLn $ "----|-------|-------------|-------------"
      let printSize : Nat -> IO ()
          printSize s = do
            let csp = buildCSPForFunction n d s targetTT
            let sem = buildCSPSemantic n d s targetTT
            putStrLn $ show s
              ++ " | " ++ show (emptyDomainNodes csp)
              ++ " | " ++ show (fullyCompatible csp) ++ "/" ++ show (partiallyCompatible csp) ++ "/" ++ show (fullyIncompatible csp)
              ++ " | " ++ show (fullyCompatible sem) ++ "/" ++ show (fullyIncompatible sem)
      let sizes : List Nat = case maxS of Z => [0]; S k => [0 .. S k]
      traverse_ printSize sizes

    runTestM2 : Bits32 -> Nat -> Nat -> Nat -> String -> IO ()
    runTestM2 targetTT d maxS numNodes outFile = do
      putStrLn $ "M2 test: TT=" ++ toHex targetTT ++ ", d=" ++ show d ++ ", s<=" ++ show maxS ++ ", nodes=" ++ show numNodes
      let cspData = buildCSPData (inferN targetTT) d maxS targetTT
      let res = cspResult cspData
      putStrLn $ "Full CSP: " ++ show (nodeCount res) ++ " nodes, " ++ show (edgeCount res) ++ " edges"
      putStrLn $ "  Empty domains: " ++ show (emptyDomainNodes res)
      let topNodes = topNodesByDomain 1 cspData
      let seed : Nat = case topNodes of (s :: _) => s; [] => 0
      putStrLn $ "Seed node: " ++ show seed
      let subNodes = bfsExpand seed numNodes cspData
      putStrLn $ "Sub-instance nodes (" ++ show (length subNodes) ++ "): " ++ show subNodes
      let subCSP = extractSubInstance subNodes cspData
      let m2Nodes = map (\(i, dom) => MkCSPNode i dom) (cspNodes subCSP)
      let totalVars = foldl (\acc, n => acc + length (domain n)) 0 m2Nodes
      putStrLn $ "Variables: " ++ show totalVars
      putStrLn $ "Edges: " ++ show (length (cspEdgeGroups subCSP))
      let script = generateM2FromCSP m2Nodes (cspEdgeGroups subCSP)
      Right () <- writeFile outFile script
        | Left err => putStrLn $ "Error writing: " ++ show err
      putStrLn $ "M2 script written to: " ++ outFile
      if totalVars <= 100
        then do
          putStrLn "Running M2..."
          Right output <- runM2 outFile
            | Left msg => putStrLn msg
          let results = parseM2Output output
          putStrLn $ "M2 results: " ++ show results
          if isUnsat results
            then putStrLn "UNSATISFIABLE"
            else putStrLn "SATISFIABLE (or inconclusive)"
        else putStrLn $ "Too large for auto-run (" ++ show totalVars ++ " vars). Run: M2 --script " ++ outFile

    runProfiles : Bits32 -> Nat -> Nat -> Maybe String -> IO ()
    runProfiles targetTT d maxS m2file = do
      let n : Nat = inferN targetTT
      putStrLn $ "Profile reduction: TT=" ++ toHex targetTT ++ ", d=" ++ show d ++ ", s<=" ++ show maxS
      let cspData = buildCSPData n d maxS targetTT
      let res = cspResult cspData
      putStrLn $ "  Nodes: " ++ show (nodeCount res) ++ " (" ++ show (emptyDomainNodes res) ++ " empty)"
      putStrLn $ "  Edges: " ++ show (edgeCount res)
      let totalElems : Nat = foldl (\acc, (_, dom) => acc + length dom) (the Nat 0) (cspNodes cspData)
      putStrLn $ "  Domain elements (dedup): " ++ show totalElems
      -- Compute profiles per node
      putStrLn "  Profiles per node:"
      let edgeGrps = cspEdgeGroups cspData
      let computeInfo : (Nat, List String) -> (Nat, Nat, Nat)
          computeInfo (nodeIdx, dom) =
            let domSize = length dom
                profMap = computeNodeProfiles nodeIdx domSize edgeGrps
                nProfiles = length (SortedMap.toList profMap)
            in (nodeIdx, domSize, nProfiles)
      let profileInfo = map computeInfo (cspNodes cspData)
      let printInfo : (Nat, Nat, Nat) -> IO ()
          printInfo (ni, ds, np) =
            putStrLn $ "    node " ++ show ni ++ ": " ++ show ds ++ " elements -> " ++ show np ++ " profiles"
      traverse_ printInfo profileInfo
      let totalProfiles : Nat = foldl (\acc, (_, _, np) => acc + np) (the Nat 0) profileInfo
      putStrLn $ "  Total variables (profile-reduced): " ++ show totalProfiles
      putStrLn $ "  Reduction: " ++ show totalElems ++ " -> " ++ show totalProfiles
      case m2file of
        Nothing => pure ()
        Just outFile => do
          let dump = dumpCSP cspData
          Right () <- writeFile outFile dump
            | Left err => putStrLn $ "Error writing: " ++ show err
          putStrLn $ "  CSP data written to: " ++ outFile

    runSolve : Bits32 -> Nat -> Nat -> IO ()
    runSolve targetTT d maxS = do
      let n : Nat = inferN targetTT
      putStrLn $ "Solving structural CSP: TT=" ++ toHex targetTT ++ ", n=" ++ show n ++ ", d=" ++ show d ++ ", s<=" ++ show maxS
      let cspData = buildCSPData n d maxS targetTT
      let res = cspResult cspData
      putStrLn $ "  Nodes: " ++ show (nodeCount res) ++ " (" ++ show (emptyDomainNodes res) ++ " empty)"
      putStrLn $ "  Edges: " ++ show (edgeCount res)
      let totalVars : Nat = foldl (\acc, (_, dom) => acc + length dom) (the Nat 0) (cspNodes cspData)
      putStrLn $ "  Domain elements (after dedup): " ++ show totalVars
      if emptyDomainNodes res > 0
        then putStrLn "  UNSAT (trivial: empty domain)"
        else do
          let result = solveCSP cspData 1000000
          case result of
            SatResult sol => putStrLn $ "  SAT — compatible family found (" ++ show (length sol) ++ " assignments)"
            UnsatResult => putStrLn "  UNSAT (or fuel exhausted at 1M backtracks)"

    runVSolve : Bits32 -> Nat -> Nat -> IO ()
    runVSolve targetTT d maxS = do
      let n : Nat = inferN targetTT
      putStrLn $ "Verified solve: TT=" ++ toHex targetTT ++ ", n=" ++ show n ++ ", d=" ++ show d ++ ", s<=" ++ show maxS
      let cspData = buildCSPData n d maxS targetTT
      let res = cspResult cspData
      putStrLn $ "  Nodes: " ++ show (nodeCount res) ++ " (" ++ show (emptyDomainNodes res) ++ " empty)"
      putStrLn $ "  Edges: " ++ show (edgeCount res)
      let totalVars : Nat = foldl (\acc, (_, dom) => acc + length dom) (the Nat 0) (cspNodes cspData)
      putStrLn $ "  Domain elements (after dedup): " ++ show totalVars
      -- Run unverified solver for comparison
      let rawResult = solveCSP cspData 1000000
      putStrLn $ "  Unverified solver: " ++ show rawResult
      -- Run verified solver
      putStrLn "  Running verified solver..."
      case verifiedSolve cspData 1000000 of
        Left err => putStrLn $ "  Verification FAILED: " ++ err
        Right (_ ** _ ** _ ** vresult) => do
          putStrLn $ "  Verified result: " ++ show vresult
          -- Check agreement
          case (rawResult, vresult) of
            (SatResult _, VSat _) =>
              putStrLn "  MATCH: both SAT, witness type-checked by Idris2"
            (UnsatResult, VUnsat (TrivialUnsat _ _)) =>
              putStrLn "  MATCH: both UNSAT, genuine proof (empty domain)"
            (UnsatResult, VUnsat (ExhaustiveUnsat _)) =>
              putStrLn "  MATCH: both UNSAT, genuine proof (exhaustive search + verified certificate)"
            (UnsatResult, VInconclusive _) =>
              putStrLn "  MATCH: raw=UNSAT, verified=INCONCLUSIVE (honest: no proof)"
            (SatResult _, VUnsat _) =>
              putStrLn "  CONFLICT: raw=SAT but verified=UNSAT (should not happen)"
            (SatResult _, VInconclusive _) =>
              putStrLn "  CONFLICT: raw=SAT but verified=INCONCLUSIVE (validation issue)"
            (UnsatResult, VSat _) =>
              putStrLn "  CONFLICT: raw=UNSAT but verified=SAT (solver bug?)"

    runCert : Bits32 -> Nat -> Nat -> IO ()
    runCert targetTT d maxS = do
      let n : Nat = inferN targetTT
      putStrLn $ "Exhaustive certified solve: TT=" ++ toHex targetTT
                 ++ ", n=" ++ show n ++ ", d=" ++ show d ++ ", s<=" ++ show maxS
      let cspData = buildCSPData n d maxS targetTT
      let res = cspResult cspData
      putStrLn $ "  Nodes: " ++ show (nodeCount res) ++ " (" ++ show (emptyDomainNodes res) ++ " empty)"
      putStrLn $ "  Edges: " ++ show (edgeCount res)
      let totalVars : Nat = foldl (\acc, (_, dom) => acc + length dom) (the Nat 0) (cspNodes cspData)
      putStrLn $ "  Domain elements (after dedup): " ++ show totalVars
      putStrLn "  Running complete solver (no fuel limit)..."
      case exhaustiveVerifiedSolve cspData of
        Left assign => do
          putStrLn $ "  Result: SAT"
          putStrLn $ "  Assignment: " ++ show assign
        Right (cert, valid) => do
          putStrLn $ "  Result: UNSAT"
          putStrLn $ "  Certificate size: " ++ show (certSize cert)
          putStrLn $ "  Certificate verified: " ++ show valid
          if valid
            then putStrLn "  VERIFIED UNSAT: certificate independently checked by Idris2"
            else putStrLn "  WARNING: certificate FAILED verification (bug in solver or checker)"

    ||| Scan all n=4 functions, find those with all domains non-empty at (d, s),
    ||| then solve their structural CSP.
    runScanSolve : Nat -> Nat -> Nat -> IO ()
    runScanSolve n d maxS = do
      putStrLn $ "Scan-solve: n=" ++ show n ++ ", d=" ++ show d ++ ", s<=" ++ show maxS
      putStrLn "Enumerating..."
      let subRes = enumerate d maxS
      putStrLn $ "  " ++ show (totalCount subRes) ++ " formulas, " ++ show (functionsCovered subRes) ++ "/" ++ show (numFunctions d) ++ " functions covered"
      let coveredTTs : SortedSet Bits32 = SortedSet.fromList (map fst (SortedMap.toList (byTruthTable subRes)))
      let scs = allSubCubes n d
      putStrLn $ "  " ++ show (length scs) ++ " sub-cubes"
      fflush stdout
      -- Scan and solve in IO loop
      let numFuncs : Bits32 = cast (numFunctions n)
      scanLoop n d scs coveredTTs subRes 0 numFuncs 0 0
    where
      isCovered : Nat -> Bits32 -> List SubCube -> SortedSet Bits32 -> Bool
      isCovered n tt scs coveredTTs =
        all (\sc => contains (subFunction n tt sc) coveredTTs) scs

      scanLoop : Nat -> Nat -> List SubCube -> SortedSet Bits32 -> EnumResult -> Bits32 -> Bits32 -> Nat -> Nat -> IO ()
      scanLoop n d scs coveredTTs subRes i limit nSat nUnsat =
        if i >= limit
          then putStrLn $ "Done: " ++ show nSat ++ " SAT, " ++ show nUnsat ++ " UNSAT (of " ++ show (nSat + nUnsat) ++ " eligible)"
          else do
            let tt : Bits32 = i
            if isCovered n tt scs coveredTTs
              then do
                let cspData = buildCSPDataWith n d subRes tt
                let result = solveCSP cspData 1000000
                case result of
                  SatResult _ => do
                    scanLoop n d scs coveredTTs subRes (i + 1) limit (S nSat) nUnsat
                  UnsatResult => do
                    putStrLn $ "  *** UNSAT: " ++ toHex tt ++ " (" ++ show tt ++ ")"
                    fflush stdout
                    scanLoop n d scs coveredTTs subRes (i + 1) limit nSat (S nUnsat)
              else scanLoop n d scs coveredTTs subRes (i + 1) limit nSat nUnsat

    ||| Scan all 2^(2^n) functions for sub-function diversity.
    ||| Functions using more of the 2^(2^d) possible sub-functions are
    ||| structurally more complex and better candidates for obstruction witnesses.
    runScan : Nat -> Nat -> IO ()
    runScan n d = do
      let numFuncs : Nat = numFunctions n
      putStrLn $ "Scanning " ++ show numFuncs ++ " functions at n=" ++ show n ++ ", d=" ++ show d
      let scs = allSubCubes n d
      putStrLn $ "Sub-cubes: " ++ show (length scs) ++ " (max possible sub-functions: " ++ show (numFunctions d) ++ ")"
      let indices : List Nat = case numFuncs of Z => []; S m => [0 .. m]
      let mkPair : Nat -> (Bits32, Nat)
          mkPair i = let tt : Bits32 = cast i in (tt, countDistinctSubFunctions n d tt)
      let results = map mkPair indices
      let maxDist : Nat = foldl (\mx, (_, nd) => if nd > mx then nd else mx) (the Nat 0) results
      putStrLn $ "Max distinct sub-functions: " ++ show maxDist
      putStrLn "Distinct sub-funcs | # functions"
      putStrLn "-------------------|------------"
      let levels : List Nat = case maxDist of Z => [0]; S m => [0 .. S m]
      let printLevel : Nat -> IO ()
          printLevel level =
            let count = length (filter (\(_, nd) => nd == level) results)
            in when (count > 0) $
              putStrLn $ show level ++ " | " ++ show count
      traverse_ printLevel levels

    ||| Show top-K functions with most diverse sub-functions.
    ||| Also shows specific candidate functions (parity, majority, etc.)
    runScanTop : Nat -> Nat -> Nat -> IO ()
    runScanTop n d k = do
      let numFuncs : Nat = numFunctions n
      putStrLn $ "Scanning " ++ show numFuncs ++ " functions, showing top " ++ show k
      let scs = allSubCubes n d
      let indices : List Nat = case numFuncs of Z => []; S k' => [0 .. k']
      let mkPair : Nat -> (Bits32, Nat)
          mkPair i = let tt : Bits32 = cast i in (tt, countDistinctSubFunctions n d tt)
      let results = map mkPair indices
      let sorted = sortBy (\(_, a), (_, b) => compare b a) results
      let top = take k sorted
      putStrLn "TT (hex) | distinct sub-funcs"
      putStrLn "---------|-------------------"
      let printRow : (Bits32, Nat) -> IO ()
          printRow (tt, nd) = putStrLn $ "0x" ++ show tt ++ " | " ++ show nd
      traverse_ printRow top
      -- Named candidates
      putStrLn ""
      putStrLn "Named candidates:"
      let candidates : List (String, Bits32) =
            [ ("BENT (x0∧x1)⊕(x2∧x3)", 0x7888)
            , ("Parity x0⊕x1⊕x2⊕x3", 0x6996)
            , ("Majority (>=3 of 4)", 0xFEE8)
            , ("Threshold-2 (>=2)", 0x7F80)
            ]
      let printCandidate : (String, Bits32) -> IO ()
          printCandidate (name, tt) =
            putStrLn $ "  " ++ name ++ " (0x" ++ show tt ++ "): " ++ show (countDistinctSubFunctions n d tt) ++ " distinct sub-funcs"
      traverse_ printCandidate candidates

    ||| Compute overlap ratio for one edge: fraction of keys at src matching any key at dst.
    overlapRatio : CSPEdgeGroups -> (Nat, Nat, Nat, Nat)  -- (srcKeys, dstKeys, matching, srcNodeId)
    overlapRatio eg =
      let srcKeys = SortedMap.toList (groupsI eg)
          dstKeys = SortedMap.toList (groupsJ eg)
          srcKeySet = SortedSet.fromList (map fst srcKeys)
          dstKeySet = SortedSet.fromList (map fst dstKeys)
          matching = length (filter (\(k, _) => SortedSet.contains k dstKeySet) srcKeys)
      in (length srcKeys, length dstKeys, matching, edgeI eg)

    ||| Analyze a single function: edge classifications, overlap ratios, type diversity.
    ||| Outputs machine-readable data for verified post-processing.
    runAnalyze : Bits32 -> Nat -> Nat -> IO ()
    runAnalyze targetTT d maxS = do
      let n : Nat = inferN targetTT
      putStrLn $ "ANALYZE " ++ show (cast {to=Nat} targetTT)
      let cspData = buildCSPData n d maxS targetTT
      let res = cspResult cspData
      -- Node data: node_id, domain_size, num_canonical_keys
      putStrLn $ "NODES " ++ show (nodeCount res)
      let egs = cspEdgeGroups cspData
      let printNode : (Nat, List String) -> IO ()
          printNode (nid, dom) = do
            -- Count distinct canonical keys across all edges for this node
            let keySets = map (\eg =>
                  if edgeI eg == nid then SortedSet.fromList (map fst (SortedMap.toList (groupsI eg)))
                  else if edgeJ eg == nid then SortedSet.fromList (map fst (SortedMap.toList (groupsJ eg)))
                  else SortedSet.empty) egs
            let allKeys = foldl SortedSet.union SortedSet.empty keySets
            putStrLn $ "N " ++ show nid ++ " " ++ show (length dom)
                       ++ " " ++ show (length (SortedSet.toList allKeys))
      traverse_ printNode (cspNodes cspData)
      -- Edge data: src dst srcKeys dstKeys matchingKeys classification
      putStrLn $ "EDGES " ++ show (edgeCount res)
      let printEdge : CSPEdgeGroups -> IO ()
          printEdge eg = do
            let (sk, dk, mk, _) = overlapRatio eg
            let cls = if mk == 0 then "FI"
                      else if mk == sk && mk == dk then "FC"
                      else "PC"
            let rSrc = if sk == 0 then "0" else show mk ++ "/" ++ show sk
            let rDst = if dk == 0 then "0" else show mk ++ "/" ++ show dk
            putStrLn $ "E " ++ show (edgeI eg) ++ " " ++ show (edgeJ eg)
                       ++ " " ++ show sk ++ " " ++ show dk ++ " " ++ show mk
                       ++ " " ++ cls ++ " " ++ rSrc ++ " " ++ rDst
      traverse_ printEdge egs
      -- Type diversity: distinct sub-function truth tables
      let nDistinct = countDistinctSubFunctions n d targetTT
      putStrLn $ "DIVERSITY " ++ show nDistinct ++ "/" ++ show (numFunctions d)
      -- Solve result
      if emptyDomainNodes res > 0
        then putStrLn "RESULT TRIVIAL_UNSAT"
        else do
          let result = solveCSP cspData 1000000
          case result of
            SatResult _ => putStrLn "RESULT SAT"
            UnsatResult => putStrLn "RESULT UNSAT"

    runM2Command : String -> IO ()
    runM2Command scriptFile = do
      Right output <- runM2 scriptFile
        | Left msg => putStrLn msg
      let results = parseM2Output output
      putStrLn $ "M2 results: " ++ show results
      if isUnsat results
        then putStrLn "System is UNSATISFIABLE"
        else putStrLn "System is SATISFIABLE (or inconclusive)"

    --- New analysis commands ---

    showDouble4 : Double -> String
    showDouble4 x = show x

    runEntropy : Nat -> Nat -> IO ()
    runEntropy d maxS = do
      putStrLn $ "Entropy analysis: d=" ++ show d ++ ", s<=" ++ show maxS
      let res = enumerate d maxS
      let (tgPairs, totalF) = tgDistribution res
      let counts = map snd tgPairs
      let probs = tgProbs counts totalF
      let nFuncs = length tgPairs
      let h = shannonEntropy probs
      let nEff = dpow 2.0 h
      let gini = giniCoefficient counts
      let sorted = sortBy (\a, b => compare b a) counts
      putStrLn $ "  Formulas: " ++ show totalF
      putStrLn $ "  Functions: " ++ show nFuncs ++ "/" ++ show (numFunctions d)
      let maxTg : Nat = case sorted of (x :: _) => x; [] => 0
      let medTg : Nat = case drop (assert_total (divNat nFuncs 2)) sorted of (x :: _) => x; [] => 0
      let minTg : Nat = case sorted of [] => 0; _ => foldl min 999999 sorted
      putStrLn $ "  T_g: max=" ++ show maxTg
        ++ ", median=" ++ show medTg
        ++ ", min=" ++ show minTg
      putStrLn $ "  H_func (Shannon): " ++ showDouble4 h ++ " bits"
      putStrLn $ "  N_eff = 2^H: " ++ showDouble4 nEff
      putStrLn $ "  Gini coefficient: " ++ showDouble4 gini
      -- Sigma for comparison
      let ri = analyzeRestrictions d res
      putStrLn $ "  sigma: " ++ showDouble4 (sigma ri)
      putStrLn $ "  log2(sigma): " ++ showDouble4 (log2 (sigma ri))
      putStrLn $ "  H_func/log2(sigma): " ++ showDouble4 (h / log2 (sigma ri))

    runRenyi : Nat -> Nat -> IO ()
    runRenyi d maxS = do
      putStrLn $ "Renyi spectrum: d=" ++ show d ++ ", s<=" ++ show maxS
      let res = enumerate d maxS
      let (tgPairs, totalF) = tgDistribution res
      let counts = map snd tgPairs
      let probs = tgProbs counts totalF
      let spectrum = renyiSpectrum probs
      putStrLn $ "  Formulas: " ++ show totalF ++ ", Functions: " ++ show (length tgPairs)
      putStrLn $ "  alpha    | H_alpha    | N_alpha = 2^H"
      putStrLn $ "  ---------+------------+--------------"
      traverse_ (\r => putStrLn $ "  " ++ alphaLabel r
        ++ "       | " ++ showDouble4 (hAlpha r)
        ++ " | " ++ showDouble4 (nAlpha r)) spectrum

    runNeffScale : Nat -> IO ()
    runNeffScale maxS = do
      putStrLn $ "N_eff scaling at s<=" ++ show maxS
      putStrLn $ "  d | T(forms) | N_funcs  | N_eff      | N_2        | sigma"
      putStrLn $ "  --+----------+----------+------------+------------+---------"
      neffDim 2 maxS
      neffDim 3 maxS
      neffDim 4 maxS
      neffDim 5 maxS
      where
        neffDim : Nat -> Nat -> IO ()
        neffDim d ms = do
          let res = enumerate d ms
          let (tgPairs, totalF) = tgDistribution res
          let counts = map snd tgPairs
          let probs = tgProbs counts totalF
          let h1 = renyiEntropy 1.0 probs
          let h2 = renyiEntropy 2.0 probs
          let hInf = renyiEntropy 1.0e12 probs
          putStrLn $ "  " ++ show d
            ++ " | " ++ show totalF
            ++ " | " ++ show (length tgPairs)
            ++ " | " ++ showDouble4 (dpow 2.0 h1)
            ++ " | " ++ showDouble4 (dpow 2.0 h2)
            ++ " | " ++ showDouble4 (dpow 2.0 hInf)

    runSigmaAffine : Nat -> Nat -> RestrictionMode -> IO ()
    runSigmaAffine d maxS mode = do
      let modeStr = case mode of ValueMode => "value"; AffineMode => "affine"
      putStrLn $ "Sigma (" ++ modeStr ++ "): d=" ++ show d ++ ", s<=" ++ show maxS
      let results = computeSigmaBySize d maxS mode
      putStrLn $ "  s | formulas | functions | |U|     | M      | sigma"
      putStrLn $ "  --+----------+-----------+---------+--------+--------"
      traverse_ (\(s, sr) => putStrLn $ "  " ++ show s
        ++ " | " ++ show (srFormulas sr)
        ++ " | " ++ show (srFunctions sr)
        ++ " | " ++ show (srUniverseSize sr)
        ++ " | " ++ show (srMaxImage sr)
        ++ " | " ++ showDouble4 (srSigma sr)) results

    runSigmaDepth : Nat -> IO ()
    runSigmaDepth maxS = do
      putStrLn $ "Sigma depth analysis: s<=" ++ show maxS
      putStrLn $ "Testing: does sigma depend on target dimension or restriction depth?"
      -- d=3, k=1 value (target dim 2)
      let sr31 = computeSigma 3 maxS ValueMode
      putStrLn $ "  d=3 k=1 value (target=2): sigma=" ++ showDouble4 (srSigma sr31)
      -- d=4, k=1 value (target dim 3)
      let sr41 = computeSigma 4 maxS ValueMode
      putStrLn $ "  d=4 k=1 value (target=3): sigma=" ++ showDouble4 (srSigma sr41)
      -- d=4, k=1 affine (target dim 3)
      let sr41a = computeSigma 4 maxS AffineMode
      putStrLn $ "  d=4 k=1 affine (target=3): sigma=" ++ showDouble4 (srSigma sr41a)
      -- Comparison
      let ratio = srSigma sr41a / srSigma sr41
      putStrLn $ "  Affine/value ratio at d=4: " ++ showDouble4 ratio
      if ratio > 1.1
        then putStrLn "  -> Affine restrictions significantly increase sigma"
        else putStrLn "  -> Affine restrictions do NOT significantly increase sigma"

    runSigmaDetail : Nat -> Nat -> IO ()
    runSigmaDetail d maxS = do
      putStrLn $ "Per-function restriction detail: d=" ++ show d ++ ", s<=" ++ show maxS
      let stats = perFunctionStats d maxS ValueMode
      let sorted = sortBy (\a, b => compare (friMaxDirImage b) (friMaxDirImage a)) stats
      let top = take 15 sorted
      putStrLn $ "  TT       | T_g      | maxDir | union  | share"
      putStrLn $ "  ---------+----------+--------+--------+--------"
      traverse_ (\fs => putStrLn $ "  " ++ show (friTT fs)
        ++ " | " ++ show (friTg fs)
        ++ " | " ++ show (friMaxDirImage fs)
        ++ " | " ++ show (friUnionImage fs)
        ++ " | " ++ showDouble4 (friShare fs)) top

    runGraphTopo : Bits32 -> Nat -> Nat -> IO ()
    runGraphTopo targetTT d maxS = do
      let n : Nat = inferN targetTT
      putStrLn $ "Graph topology: TT=" ++ toHex targetTT ++ ", n=" ++ show n ++ ", d=" ++ show d ++ ", s<=" ++ show maxS
      let cspData = buildCSPData n d maxS targetTT
      let egs = cspEdgeGroups cspData
      let (fc, pc, fi) = classifyEdges egs
      putStrLn $ "  Edges: FC=" ++ show fc ++ " PC=" ++ show pc ++ " FI=" ++ show fi
      let g = extractObstructionGraph egs
      let nNodes = length (SortedSet.toList (cgNodes g))
      let nEdges = length (cgEdges g)
      putStrLn $ "  Obstruction graph: " ++ show nNodes ++ " nodes, " ++ show nEdges ++ " edges"
      let comp = connectedComponents g
      let b1 = betti1 g
      putStrLn $ "  Components: " ++ show comp
      putStrLn $ "  Betti B1: " ++ show b1
      let tri = countTriangles g
      putStrLn $ "  Triangles: " ++ show tri
      -- Type diversity
      let scs = allSubCubes n d
      let nDist = typeDiversity n targetTT scs
      putStrLn $ "  Type diversity: " ++ show nDist ++ "/" ++ show (numFunctions d)

    printCommEdge : CSPEdgeGroups -> IO ()
    printCommEdge eg =
      let cr = communicationComplexity eg
      in putStrLn $ "  " ++ show (edgeI eg) ++ "-" ++ show (edgeJ eg)
        ++ " | " ++ show (crNGroupsI cr)
        ++ " | " ++ show (crNGroupsJ cr)
        ++ " | " ++ show (crNCommon cr)
        ++ " | " ++ show (crOneWayCC cr)
        ++ " | " ++ showDouble4 (crCompatFraction cr * 100.0) ++ "%"

    runComm : Bits32 -> Nat -> Nat -> IO ()
    runComm targetTT d maxS = do
      let n : Nat = inferN targetTT
      putStrLn $ "Communication complexity: TT=" ++ toHex targetTT ++ ", d=" ++ show d ++ ", s<=" ++ show maxS
      let cspData = buildCSPData n d maxS targetTT
      let egs = cspEdgeGroups cspData
      let sample = take 20 egs
      putStrLn $ "  edge       | keys_i | keys_j | common | CC_1way | compat%"
      putStrLn $ "  -----------+--------+--------+--------+---------+--------"
      traverse_ (printCommEdge) sample
      -- Aggregate
      let allCR = map communicationComplexity egs
      let avgCC : Double = if length allCR == 0 then 0.0
            else cast (foldl (\a, cr => a + crOneWayCC cr) (the Nat 0) allCR) / cast (length allCR)
      putStrLn $ "  Avg one-way CC: " ++ showDouble4 avgCC ++ " bits"

    runVStats : String -> Nat -> IO ()
    runVStats filePath sampleN = do
      putStrLn $ "Verified stats from: " ++ filePath
      Right content <- readFile filePath
        | Left err => putStrLn $ "Error reading file: " ++ show err
      let allTTs = mapMaybe parseTTLine (lines content)
      putStrLn $ "  " ++ show (length allTTs) ++ " truth tables in file"
      let sample = take sampleN allTTs
      putStrLn $ "  Analyzing " ++ show (length sample) ++ " instances..."
      -- Analyze each
      let results = map (\tt => analyzeInstance (inferN tt) tt 3 4) sample
      -- Edge classification
      let (fc, pc, fi) = edgeClassDist results
      let totalE = fc + pc + fi
      putStrLn $ "\n  Edge Classification:"
      putStrLn $ "    FC: " ++ show fc ++ " (" ++ showDouble4 (if totalE == 0 then 0.0 else cast fc / cast totalE * 100.0) ++ "%)"
      putStrLn $ "    PC: " ++ show pc ++ " (" ++ showDouble4 (if totalE == 0 then 0.0 else cast pc / cast totalE * 100.0) ++ "%)"
      putStrLn $ "    FI: " ++ show fi ++ " (" ++ showDouble4 (if totalE == 0 then 0.0 else cast fi / cast totalE * 100.0) ++ "%)"
      -- Overlap ratios
      let (meanR, minR, maxR) = overlapStats results
      putStrLn $ "\n  Overlap ratios (PC edges):"
      putStrLn $ "    mean=" ++ showDouble4 meanR ++ " min=" ++ showDouble4 minR ++ " max=" ++ showDouble4 maxR
      -- Cycle loss
      let (avgGeo, nInst) = cycleLossAnalysis results
      putStrLn $ "\n  Cycle loss analysis:"
      putStrLn $ "    Avg geometric mean r_src: " ++ showDouble4 avgGeo
      putStrLn $ "    Instances analyzed: " ++ showDouble4 nInst
      -- Type diversity
      let diversities = map (\r => fst (irDiversity r)) results
      putStrLn $ "\n  Type diversity:"
      putStrLn $ "    min=" ++ show (foldl min 999 diversities)
        ++ " max=" ++ show (foldl max 0 diversities)

    runScan5 : Nat -> Nat -> String -> Nat -> IO ()
    runScan5 d maxS strategy count = do
      putStrLn $ "n=5 scan: d=" ++ show d ++ ", s<=" ++ show maxS ++ ", strategy=" ++ strategy ++ ", count=" ++ show count
      if strategy == "lifted"
        then do
          -- Read n=4 UNSAT TTs and lift to n=5
          Right content <- readFile "scripts/genuine_unsat_n4d3s4.txt"
            | Left err => putStrLn $ "Error reading UNSAT file: " ++ show err
          let n4tts = take count (mapMaybe parseTTLine (lines content))
          putStrLn $ "  Lifting " ++ show (length n4tts) ++ " n=4 UNSAT instances to n=5"
          let subRes = enumerate d maxS
          putStrLn $ "  Enumerated " ++ show (totalCount subRes) ++ " formulas"
          scanLifted subRes d n4tts 0 0
        else do
          -- Range-based scan
          putStrLn $ "  Scanning range [0, " ++ show count ++ ")"
          let subRes = enumerate d maxS
          putStrLn $ "  Enumerated " ++ show (totalCount subRes) ++ " formulas"
          let scs = allSubCubes 5 d
          let coveredTTs : SortedSet Bits32 = SortedSet.fromList (map fst (SortedMap.toList (byTruthTable subRes)))
          scanRange 5 d scs coveredTTs subRes 0 (cast count) 0 0
      where
        liftTT : Bits32 -> Bits32
        liftTT tt = tt .|. (shiftL tt 16)

        scanLifted : EnumResult -> Nat -> List Bits32 -> Nat -> Nat -> IO ()
        scanLifted _ _ [] nSat nUnsat =
          putStrLn $ "  Done: " ++ show nSat ++ " SAT, " ++ show nUnsat ++ " UNSAT"
        scanLifted subRes d (tt4 :: rest) nSat nUnsat = do
          let tt5 = liftTT tt4
          let cspData = buildCSPDataWith 5 d subRes tt5
          let result = solveCSP cspData 1000000
          case result of
            SatResult _ => scanLifted subRes d rest (S nSat) nUnsat
            UnsatResult => do
              putStrLn $ "  UNSAT: " ++ toHex tt5 ++ " (lifted from " ++ toHex tt4 ++ ")"
              fflush stdout
              scanLifted subRes d rest nSat (S nUnsat)

        scanRange : Nat -> Nat -> List SubCube -> SortedSet Bits32 -> EnumResult -> Bits32 -> Bits32 -> Nat -> Nat -> IO ()
        scanRange n d scs covered subRes i limit nSat nUnsat =
          if i >= limit
            then putStrLn $ "  Done: " ++ show nSat ++ " SAT, " ++ show nUnsat ++ " UNSAT"
            else do
              let allCovered = all (\sc => contains (subFunction n i sc) covered) scs
              if allCovered
                then do
                  let cspData = buildCSPDataWith n d subRes i
                  let result = solveCSP cspData 1000000
                  case result of
                    SatResult _ => scanRange n d scs covered subRes (i + 1) limit (S nSat) nUnsat
                    UnsatResult => do
                      putStrLn $ "  UNSAT: " ++ toHex i
                      fflush stdout
                      scanRange n d scs covered subRes (i + 1) limit nSat (S nUnsat)
                else scanRange n d scs covered subRes (i + 1) limit nSat nUnsat

    runCompression : Nat -> Nat -> IO ()
    runCompression d maxS = do
      putStrLn $ "Compression bound: d=" ++ show d ++ ", s<=" ++ show maxS
      let res = enumerate d maxS
      let (tgPairs, totalF) = tgDistribution res
      let counts = map snd tgPairs
      let probs = tgProbs counts totalF
      let h1 = shannonEntropy probs
      let nEff = dpow 2.0 h1
      -- Sigma
      let ri = analyzeRestrictions d res
      let sig = sigma ri
      let m = maxImage ri
      let u = universeSize ri
      -- sigma_eff = |U| / avg_image, where avg_image = T / N_eff
      let avgImage = cast totalF / nEff
      let sigmaEff = cast u / avgImage
      putStrLn $ "  |U| = " ++ show u
      putStrLn $ "  M (max image) = " ++ show m
      putStrLn $ "  sigma (max) = " ++ showDouble4 sig
      putStrLn $ "  N_eff = " ++ showDouble4 nEff
      putStrLn $ "  Avg image = T/N_eff = " ++ showDouble4 avgImage
      putStrLn $ "  sigma_eff = |U|/avg_image = " ++ showDouble4 sigmaEff
      putStrLn $ "  Ratio sigma_eff/sigma = " ++ showDouble4 (sigmaEff / sig)
