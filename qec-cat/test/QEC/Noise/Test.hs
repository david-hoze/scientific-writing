module QEC.Noise.Test (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import QEC.Noise
import QEC.Noise.Depolarizing
import QEC.Noise.Biased
import QEC.Noise.CatQubit

tests :: TestTree
tests = testGroup "QEC.Noise"
  [ depolarizingTests
  , biasedTests
  , catQubitTests
  ]

depolarizingTests :: TestTree
depolarizingTests = testGroup "Depolarizing"
  [ testCase "symmetric channel" $ do
      let ch = depolarizing 0.03
      assertApprox "pX" (pX ch) 0.01
      assertApprox "pY" (pY ch) 0.01
      assertApprox "pZ" (pZ ch) 0.01

  , testCase "total error rate" $ do
      let ch = depolarizing 0.03
      assertApprox "total" (totalErrorRate ch) 0.03
  ]

biasedTests :: TestTree
biasedTests = testGroup "Biased"
  [ testCase "bias = 100" $ do
      let ch = biasedChannel 0.01 100.0
      assertApprox "pZ" (pZ ch) 0.01
      assertApprox "pX" (pX ch) 0.0001
      assertApprox "catBias" (catBias ch) 100.0

  , testCase "channelFromBias roundtrip" $ do
      let ch = channelFromBias 0.03 10.0
      assertApprox "bias" (catBias ch) 10.0
      assertApprox "total" (totalErrorRate ch) 0.03
  ]

catQubitTests :: TestTree
catQubitTests = testGroup "CatQubit"
  [ testCase "default params produce extreme bias" $ do
      let ch = catQubitChannel defaultCatParams
          bias = catBias ch
      -- Bias = exp(gamma * |alpha|^2) = exp(2 * 19) = exp(38) ~ 3.2e16
      -- This is extremely biased, as expected for cat qubits
      assertBool ("bias should be > 1e10, got " ++ show bias) (bias > 1e10)

  , testCase "pZ is reasonable at default params" $ do
      let ch = catQubitChannel defaultCatParams
      -- p_Z = kappa_1 * |alpha|^2 * T_cycle = 1e3 * 19 * 500e-9 = 9.5e-3
      assertApprox "pZ" (pZ ch) 9.5e-3

  , testCase "pX is exponentially suppressed" $ do
      let ch = catQubitChannel defaultCatParams
      -- p_X should be very small (< 1e-6)
      assertBool ("pX should be < 1e-6, got " ++ show (pX ch)) (pX ch < 1e-6)

  , testCase "increasing alpha^2 suppresses pX further" $ do
      let params1 = defaultCatParams { cqAlphaSq = 10 }
          params2 = defaultCatParams { cqAlphaSq = 20 }
          px1 = pX (catQubitChannel params1)
          px2 = pX (catQubitChannel params2)
      assertBool "pX decreases with alpha^2" (px2 < px1)

  , testCase "increasing alpha^2 increases pZ" $ do
      let params1 = defaultCatParams { cqAlphaSq = 10 }
          params2 = defaultCatParams { cqAlphaSq = 20 }
          pz1 = pZ (catQubitChannel params1)
          pz2 = pZ (catQubitChannel params2)
      assertBool "pZ increases with alpha^2" (pz2 > pz1)
  ]

-- | Assert that two doubles are approximately equal (within 1%).
assertApprox :: String -> Double -> Double -> IO ()
assertApprox label actual expected =
  assertBool (label ++ ": expected ~" ++ show expected
              ++ " but got " ++ show actual)
             (abs (actual - expected) < 0.01 * abs expected + 1e-15)
