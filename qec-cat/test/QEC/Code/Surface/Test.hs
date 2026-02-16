module QEC.Code.Surface.Test (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import QEC.Code.CSS
import QEC.Code.Surface

tests :: TestTree
tests = testGroup "QEC.Code.Surface"
  [ testCase "surfaceCode 3: k=1, n=9" $ do
      let code = surfaceCode 3
      cssNumQubits code @?= 9
      cssNumLogical code @?= 1

  , testCase "surfaceCode 5: k=1, n=25" $ do
      let code = surfaceCode 5
      cssNumQubits code @?= 25
      cssNumLogical code @?= 1

  , testCase "surfaceCode 7: k=1, n=49" $ do
      let code = surfaceCode 7
      cssNumQubits code @?= 49
      cssNumLogical code @?= 1

  , testCase "surfaceCode 9: k=1, n=81" $ do
      let code = surfaceCode 9
      cssNumQubits code @?= 81
      cssNumLogical code @?= 1

  , testCase "surfaceCode 2: k=1, n=4" $ do
      let code = surfaceCode 2
      cssNumQubits code @?= 4
      cssNumLogical code @?= 1
  ]
