module QEC.Resource.Test (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import QEC.Noise.CatQubit
import QEC.Resource
import QEC.Resource.Algorithm
import QEC.Resource.MagicState
import QEC.Resource.Layout

tests :: TestTree
tests = testGroup "QEC.Resource"
  [ testCase "ECDLP-256 with rep-cat gives reasonable qubit count" $ do
      let est = estimateResources ecdlp256 defaultCatParams RepetitionCat
                  defaultFactory defaultLayoutParams
      -- Gouzien et al. target: ~126,000 cat qubits
      -- We allow a wide range since our model is simplified
      assertBool ("total qubits out of range: " ++ show (reTotalQubits est))
        (reTotalQubits est > 10000 && reTotalQubits est < 1000000)

  , testCase "code distance is odd and >= 3" $ do
      let est = estimateResources ecdlp256 defaultCatParams RepetitionCat
                  defaultFactory defaultLayoutParams
      assertBool "distance < 3" (reCodeDistance est >= 3)
      assertBool "distance is even" (odd (reCodeDistance est))

  , testCase "RSA-2048 with rep-cat produces result" $ do
      let est = estimateResources shorRSA2048 defaultCatParams RepetitionCat
                  defaultFactory defaultLayoutParams
      assertBool ("total qubits too small: " ++ show (reTotalQubits est))
        (reTotalQubits est > 1000)

  , testCase "surface code requires more qubits than rep-cat" $ do
      let estRep = estimateResources ecdlp256 defaultCatParams RepetitionCat
                     defaultFactory defaultLayoutParams
          estSurf = estimateResources ecdlp256 defaultCatParams SurfaceCode
                     defaultFactory defaultLayoutParams
      -- Surface code should need more qubits due to d^2 scaling
      assertBool "surface code didn't use more data qubits"
        (reDataQubits estSurf >= reDataQubits estRep)

  , testCase "runtime is positive" $ do
      let est = estimateResources ecdlp256 defaultCatParams RepetitionCat
                  defaultFactory defaultLayoutParams
      assertBool "runtime <= 0" (reRuntimeSeconds est > 0)

  , testCase "factory count >= 1" $ do
      let est = estimateResources ecdlp256 defaultCatParams RepetitionCat
                  defaultFactory defaultLayoutParams
      assertBool "no factories" (reNumFactories est >= 1)
  ]
