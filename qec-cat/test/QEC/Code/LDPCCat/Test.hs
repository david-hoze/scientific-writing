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
  [ testCase "ldpcCatCode 0: n=165" $ do
      let code = ldpcCatCode 0
      cssNumQubits code @?= 165

  , testCase "ldpcCatCode 0: orthogonality (H_X is empty)" $ do
      let code = ldpcCatCode 0
      bmNumRows (cssHX code) @?= 0

  , testCase "torusCode dimensions" $ do
      let stab = replicate 9 True
          h = torusCode 5 7 stab
      bmNumCols h @?= 35
      bmNumRows h @?= 35

  , testCase "torusCode with weight-2 stabilizer produces redundancy" $ do
      -- A weight-2 stabilizer [[1,1,0],[0,0,0],[0,0,0]] on a 3x3 torus
      -- generates 9 rows but they should have redundancy (rank < 9)
      let stab = [True, True, False, False, False, False, False, False, False]
          h = torusCode 3 3 stab
          r = rank h
      assertBool ("rank should be < 9, got " ++ show r) (r < 9)

  , testCase "torusCode periodic boundary conditions" $ do
      -- Stabilizer at (0,0) should wrap around
      let stab = [ True,  False, False
                 , False, False, False
                 , False, False, True ]
          -- On a 3x3 torus, the check at position (0,0) should
          -- have bits at (0,0) and (2,2) [from wrapping]
          h = torusCode 3 3 stab
          row0 = bmGetRow h 0  -- check at (i=0, j=0)
      bvGetBit row0 0 @?= GF2 True  -- (0,0)
      bvGetBit row0 8 @?= GF2 True  -- (2,2)
      bvWeight row0 @?= 2
  ]
