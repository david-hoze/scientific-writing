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
import Data.SortedSet
import Data.List
import Data.Bits
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
    [_, "profiles", "--tt", ttStr, "--dim", dStr, "--size", sStr] =>
      case (parsePositive ttStr, parsePositive dStr, parsePositive sStr) of
        (Just tt, Just d, Just s) => runProfiles (cast {to=Bits32} tt) (cast {to=Nat} d) (cast {to=Nat} s) Nothing
        _ => putStrLn "Error: all numeric args must be positive integers"
    [_, "profiles", "--tt", ttStr, "--dim", dStr, "--size", sStr, "--m2gen", outFile] =>
      case (parsePositive ttStr, parsePositive dStr, parsePositive sStr) of
        (Just tt, Just d, Just s) => runProfiles (cast {to=Bits32} tt) (cast {to=Nat} d) (cast {to=Nat} s) (Just outFile)
        _ => putStrLn "Error: all numeric args must be positive integers"
    [_, "scan-solve", "--dim", dStr, "--size", sStr] =>
      case (parsePositive dStr, parsePositive sStr) of
        (Just d, Just s) => runScanSolve (cast {to=Nat} d) (cast {to=Nat} s)
        _ => putStrLn "Error: --dim and --size must be positive integers"
    [_, "scan-solve"] => runScanSolve 3 4
    [_, "scan"] => runScan 4 2
    [_, "scan", "--top", kStr] =>
      case parsePositive kStr of
        Just k => runScanTop 4 2 (cast {to=Nat} k)
        _ => putStrLn "Error: --top must be a positive integer"
    [_, "m2run", scriptFile] => runM2Command scriptFile
    _ => do putStrLn "circuit-presheaf - Boolean formula presheaf analysis"
            putStrLn ""
            putStrLn "Usage:"
            putStrLn "  circuit-presheaf enumerate --dim D --max-size S"
            putStrLn "  circuit-presheaf scaling --max-size S"
            putStrLn "  circuit-presheaf convergence --dim D --max-size S"
            putStrLn "  circuit-presheaf bent --size S"
            putStrLn "  circuit-presheaf bent --size S --m2gen FILE.m2"
            putStrLn "  circuit-presheaf bent-sub --size S [--nodes N] --m2gen FILE.m2"
            putStrLn "  circuit-presheaf test --tt TT [--size S]"
            putStrLn "  circuit-presheaf test-m2 --tt TT [--dim D] --size S [--nodes N] --m2gen FILE.m2"
            putStrLn "  circuit-presheaf solve --tt TT [--dim D] --size S"
            putStrLn "  circuit-presheaf profiles --tt TT --dim D --size S [--m2gen FILE.m2]"
            putStrLn "  circuit-presheaf scan [--top K]"
            putStrLn "  circuit-presheaf m2run FILE.m2"
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
      let n : Nat = 4
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
      let cspData = buildCSPData 4 d maxS targetTT
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
      let n : Nat = 4
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
      let n : Nat = 4
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

    ||| Scan all n=4 functions, find those with all domains non-empty at (d, s),
    ||| then solve their structural CSP.
    runScanSolve : Nat -> Nat -> IO ()
    runScanSolve d maxS = do
      let n : Nat = 4
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

    runM2Command : String -> IO ()
    runM2Command scriptFile = do
      Right output <- runM2 scriptFile
        | Left msg => putStrLn msg
      let results = parseM2Output output
      putStrLn $ "M2 results: " ++ show results
      if isUnsat results
        then putStrLn "System is UNSATISFIABLE"
        else putStrLn "System is SATISFIABLE (or inconclusive)"
