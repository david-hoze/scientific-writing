module QEC.Code.CSS.Test (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import QEC.GF2
import QEC.GF2.Matrix
import QEC.Code.CSS
import QEC.Code.Repetition

tests :: TestTree
tests = testGroup "QEC.Code.CSS"
  [ constructionTests
  , repetitionTests
  ]

constructionTests :: TestTree
constructionTests = testGroup "Construction"
  [ testCase "orthogonality violation rejected" $ do
      -- H_X = [1 0], H_Z = [1 0] -> H_X * H_Z^T = [1] /= 0
      let hx = bmFromList [[GF2 True, GF2 False]]
          hz = bmFromList [[GF2 True, GF2 False]]
      mkCSSCode hx hz @?= Left OrthogonalityViolation

  , testCase "dimension mismatch rejected" $ do
      let hx = bmFromList [[GF2 True, GF2 False]]
          hz = bmFromList [[GF2 True, GF2 False, GF2 True]]
      mkCSSCode hx hz @?= Left (DimensionMismatch 2 3)

  , testCase "valid orthogonal code accepted" $ do
      -- H_X = [1 1 0], H_Z = [0 1 1] -> H_X * H_Z^T = [1*0 + 1*1 + 0*1] = [1]
      -- Nope, that's 1. Let's use properly orthogonal ones.
      -- H_X = [1 1 0 0], H_Z = [0 0 1 1] -> product = [0]
      let hx = bmFromList [[GF2 True, GF2 True, GF2 False, GF2 False]]
          hz = bmFromList [[GF2 False, GF2 False, GF2 True, GF2 True]]
      case mkCSSCode hx hz of
        Left err   -> assertFailure $ "Should succeed: " ++ show err
        Right code -> cssNumQubits code @?= 4

  , testCase "empty H_X is valid" $ do
      let hx = bmZero 0 3
          hz = bmFromList [[GF2 True, GF2 True, GF2 False]]
      case mkCSSCode hx hz of
        Left err   -> assertFailure $ "Should succeed: " ++ show err
        Right _    -> return ()
  ]

repetitionTests :: TestTree
repetitionTests = testGroup "Repetition code"
  [ testCase "repetitionCode 3: n=3, k=1" $ do
      let code = repetitionCode 3
      cssNumQubits code @?= 3
      cssNumLogical code @?= 1

  , testCase "repetitionCode 5: n=5, k=1" $ do
      let code = repetitionCode 5
      cssNumQubits code @?= 5
      cssNumLogical code @?= 1

  , testCase "repetitionCode 7: n=7, k=1" $ do
      let code = repetitionCode 7
      cssNumQubits code @?= 7
      cssNumLogical code @?= 1

  , testCase "repetitionCode d has X-distance d" $ do
      -- Repetition code detects bit-flip (X) errors via H_Z checks
      cssXDistance (repetitionCode 3) @?= 3
      cssXDistance (repetitionCode 5) @?= 5

  , testCase "repetitionCode 1: trivial" $ do
      let code = repetitionCode 1
      cssNumQubits code @?= 1
      cssNumLogical code @?= 1
  ]
