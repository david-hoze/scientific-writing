module QEC.Stabilizer.Tableau.Test (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import System.Random (mkStdGen)

import QEC.Stabilizer.Tableau

tests :: TestTree
tests = testGroup "QEC.Stabilizer.Tableau"
  [ testCase "|0> measures deterministically as 0" $ do
      let tab = newTableau 1
          (result, _tab', _gen') = measure 0 tab (mkStdGen 42)
      result @?= False

  , testCase "X|0> measures deterministically as 1" $ do
      -- X = H S S H  (up to phase)
      -- Actually: X gate via tableau = H then Z then H, but we don't have X/Z gates.
      -- Instead: apply H, then measure (random), then the state is determined.
      -- Better approach: use CNOT trick or just test via H.
      -- X|0> = |1>. We can get |1> by: start |0>, apply H (get |+>), apply S twice
      -- to get H|0>=|+>, S|+>=|+i>, S|+i>=|->. Then H|-> = |1>.
      -- Actually S^2 = Z, so H Z H = X, meaning H S S H |0> = X|0> = |1>
      let tab = newTableau 1
          tab1 = applyH 0 tab
          tab2 = applyS 0 tab1
          tab3 = applyS 0 tab2
          tab4 = applyH 0 tab3
          (result, _tab', _gen') = measure 0 tab4 (mkStdGen 42)
      result @?= True

  , testCase "H|0> gives random outcome (statistical)" $ do
      -- H|0> = |+>, measurement should be 50/50
      let tab = newTableau 1
          tab1 = applyH 0 tab
          outcomes = [ fst3 (measure 0 tab1 (mkStdGen seed)) | seed <- [0..999] ]
          trueCount = length (filter id outcomes)
      -- Should be roughly 500 +/- 50
      assertBool ("H|0> true count out of range: " ++ show trueCount)
        (trueCount > 400 && trueCount < 600)

  , testCase "H^2 = identity" $ do
      let tab = newTableau 2
          tab1 = applyH 0 tab
          tab2 = applyH 0 tab1
      -- After H^2, tableau should be back to initial state
      -- Measure both qubits: should get |00>
      let (r0, tab3, gen1) = measure 0 tab2 (mkStdGen 42)
          (r1, _tab4, _gen2) = measure 1 tab3 gen1
      r0 @?= False
      r1 @?= False

  , testCase "Bell state: CNOT(H|0>|0>) gives correlated outcomes" $ do
      -- Prepare Bell state: H on qubit 0, CNOT(0,1)
      let tab = newTableau 2
          tab1 = applyH 0 tab
          tab2 = applyCNOT 0 1 tab1
          -- Measure both qubits with many seeds; outcomes should always agree
          results = [ let (r0, t', g') = measure 0 tab2 (mkStdGen seed)
                          (r1, _, _) = measure 1 t' g'
                      in r0 == r1
                    | seed <- [0..99] ]
      assertBool "Bell state outcomes not always correlated"
        (and results)

  , testCase "Measure |0> twice gives same result" $ do
      let tab = newTableau 1
          (r1, tab1, gen1) = measure 0 tab (mkStdGen 42)
          (r2, _tab2, _gen2) = measure 0 tab1 gen1
      r1 @?= False
      r2 @?= False

  , testCase "S^4 = identity" $ do
      let tab = newTableau 1
          tab1 = applyS 0 $ applyS 0 $ applyS 0 $ applyS 0 tab
          (result, _, _) = measure 0 tab1 (mkStdGen 42)
      result @?= False

  , testCase "3-qubit GHZ state" $ do
      -- |GHZ> = (|000> + |111>) / sqrt(2)
      -- Prepare: H(0), CNOT(0,1), CNOT(0,2)
      let tab = newTableau 3
          tab1 = applyH 0 tab
          tab2 = applyCNOT 0 1 tab1
          tab3 = applyCNOT 0 2 tab2
          -- All three measurements should agree
          results = [ let (r0, t1, g1) = measure 0 tab3 (mkStdGen seed)
                          (r1, t2, g2) = measure 1 t1 g1
                          (r2, _, _) = measure 2 t2 g2
                      in r0 == r1 && r1 == r2
                    | seed <- [0..99] ]
      assertBool "GHZ outcomes not always correlated"
        (and results)
  ]

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
