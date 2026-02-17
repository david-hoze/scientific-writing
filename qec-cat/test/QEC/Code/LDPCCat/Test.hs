module QEC.Code.LDPCCat.Test (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import QEC.GF2
import QEC.GF2.Matrix (bmNumRows, bmNumCols, bmGetRow)
import QEC.GF2.Gauss (rank)
import QEC.Code.CSS
import QEC.Code.LDPCCat

tests :: TestTree
tests = testGroup "QEC.Code.LDPCCat"
  [ testCase "ldpcCatCode 0: n=136" $ do
      let code = ldpcCatCode 0
      cssNumQubits code @?= 136

  , testCase "ldpcCatCode 0: k=34" $ do
      let code = ldpcCatCode 0
      cssNumLogical code @?= 34

  , testCase "ldpcCatCode 0: rank(H_Z)=102" $ do
      let code = ldpcCatCode 0
      rank (cssHZ code) @?= 102

  , testCase "ldpcCatCode 0: each check has weight 4" $ do
      let code = ldpcCatCode 0
          hz = cssHZ code
          m = bmNumRows hz
          weights = [ bvWeight (bmGetRow hz i) | i <- [0 .. m - 1] ]
      mapM_ (\(i, w) ->
        assertEqual ("check " ++ show i ++ " weight") 4 w
        ) (zip [0::Int ..] weights)

  , testCase "ldpcCatCode 0: H_X is empty (CSS orthogonality trivial)" $ do
      let code = ldpcCatCode 0
      bmNumRows (cssHX code) @?= 0

  , testCase "ldpcCatCode 1: n=144, k=36" $ do
      let code = ldpcCatCode 1
      cssNumQubits code @?= 144
      cssNumLogical code @?= 36

  , testCase "torusCode dimensions" $ do
      let stab = replicate 9 True
          h = torusCode 5 7 stab
      bmNumCols h @?= 35
      bmNumRows h @?= 35

  , testCase "torusCode with weight-2 stabilizer produces redundancy" $ do
      let stab = [True, True, False, False, False, False, False, False, False]
          h = torusCode 3 3 stab
          r = rank h
      assertBool ("rank should be < 9, got " ++ show r) (r < 9)

  , testCase "torusCode periodic boundary conditions" $ do
      let stab = [ True,  False, False
                 , False, False, False
                 , False, False, True ]
          h = torusCode 3 3 stab
          row0 = bmGetRow h 0
      bvGetBit row0 0 @?= GF2 True
      bvGetBit row0 8 @?= GF2 True
      bvWeight row0 @?= 2
  ]
