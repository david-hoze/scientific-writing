module QEC.Simulation.Test (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import QEC.Code.Repetition
import QEC.Simulation

tests :: TestTree
tests = testGroup "QEC.Simulation"
  [ testCase "rep code d=5 at low noise has low logical error rate" $ do
      let code = repetitionCode 5
          config = defaultSimConfig { simNumTrials = 500, simNumChunks = 2 }
          result = runSimulation config code 0.01 42
          rate = logicalErrorRate result
      assertBool ("logical error rate too high: " ++ show rate) (rate < 0.05)

  , testCase "rep code d=5 at high noise has high logical error rate" $ do
      let code = repetitionCode 5
          config = defaultSimConfig { simNumTrials = 500, simNumChunks = 2 }
          result = runSimulation config code 0.4 42
          rate = logicalErrorRate result
      assertBool ("logical error rate too low: " ++ show rate) (rate > 0.1)

  , testCase "rep code d=3 at very low noise has near-zero logical error rate" $ do
      let code = repetitionCode 3
          config = defaultSimConfig { simNumTrials = 1000, simNumChunks = 2 }
          result = runSimulation config code 0.005 42
          rate = logicalErrorRate result
      assertBool ("logical error rate too high: " ++ show rate) (rate < 0.01)

  , testCase "simulation result counts are consistent" $ do
      let code = repetitionCode 5
          config = defaultSimConfig { simNumTrials = 100, simNumChunks = 2 }
          result = runSimulation config code 0.1 42
      simTotalTrials result @?= 100
      assertBool "more logical errors than trials"
        (simLogicalErrors result <= simTotalTrials result)
  ]
