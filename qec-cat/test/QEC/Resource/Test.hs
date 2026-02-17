module QEC.Resource.Test (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import QEC.Noise.CatQubit
import QEC.Resource
import QEC.Resource.Algorithm
import QEC.Resource.MagicState

tests :: TestTree
tests = testGroup "QEC.Resource"
  [ testCase "ECDLP-256 with rep-cat within 10% of 126,133" $ do
      let est = estimateResources ecdlp256 defaultCatParams RepetitionCat
                  defaultFactory
          total = reTotalQubits est
          target = 126133 :: Int
          lo = target - div target 10   -- 113,520
          hi = target + div target 10   -- 138,746
      assertBool ("total qubits " ++ show total ++ " not within 10% of " ++ show target)
        (total >= lo && total <= hi)

  , testCase "code distance is odd and >= 3" $ do
      let est = estimateResources ecdlp256 defaultCatParams RepetitionCat
                  defaultFactory
      assertBool "distance < 3" (reCodeDistance est >= 3)
      assertBool "distance is even" (odd (reCodeDistance est))

  , testCase "RSA-2048 with rep-cat produces result" $ do
      let est = estimateResources shorRSA2048 defaultCatParams RepetitionCat
                  defaultFactory
      assertBool ("total qubits too small: " ++ show (reTotalQubits est))
        (reTotalQubits est > 1000)

  , testCase "ECDLP-256 LDPCCat uses fewer qubits than RepetitionCat" $ do
      let estRep  = estimateResources ecdlp256 defaultCatParams RepetitionCat
                      defaultFactory
          estLDPC = estimateResources ecdlp256 defaultCatParams LDPCCat
                      defaultFactory
      assertBool ("LDPCCat (" ++ show (reTotalQubits estLDPC)
                ++ ") not fewer than RepetitionCat (" ++ show (reTotalQubits estRep) ++ ")")
        (reTotalQubits estLDPC < reTotalQubits estRep)

  , testCase "RSA-2048 LDPCCat produces result" $ do
      let est = estimateResources shorRSA2048 defaultCatParams LDPCCat
                  defaultFactory
      assertBool ("total qubits too small: " ++ show (reTotalQubits est))
        (reTotalQubits est > 1000)

  , testCase "surface code requires more qubits than rep-cat" $ do
      let estRep = estimateResources ecdlp256 defaultCatParams RepetitionCat
                     defaultFactory
          estSurf = estimateResources ecdlp256 defaultCatParams SurfaceCode
                     defaultFactory
      assertBool "surface code didn't use more data qubits"
        (reDataQubits estSurf >= reDataQubits estRep)

  , testCase "runtime is positive" $ do
      let est = estimateResources ecdlp256 defaultCatParams RepetitionCat
                  defaultFactory
      assertBool "runtime <= 0" (reRuntimeSeconds est > 0)

  , testCase "factory count >= 1" $ do
      let est = estimateResources ecdlp256 defaultCatParams RepetitionCat
                  defaultFactory
      assertBool "no factories" (reNumFactories est >= 1)

  , testCase "LDPCCat code distance is odd and >= 3" $ do
      let est = estimateResources ecdlp256 defaultCatParams LDPCCat
                  defaultFactory
      assertBool "distance < 3" (reCodeDistance est >= 3)
      assertBool "distance is even" (odd (reCodeDistance est))
  ]
