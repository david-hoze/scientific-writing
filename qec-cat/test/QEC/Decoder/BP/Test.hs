module QEC.Decoder.BP.Test (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Vector.Unboxed as VU

import QEC.GF2
import QEC.GF2.Matrix
import QEC.Decoder.BP
import QEC.Code.CSS
import QEC.Code.Repetition

tests :: TestTree
tests = testGroup "QEC.Decoder.BP"
  [ testCase "decode single error on rep code d=5" $ do
      let code = repetitionCode 5
          hz = cssHZ code
          n = 5
          -- Error on qubit 2
          err = bvFromList [GF2 False, GF2 False, GF2 True, GF2 False, GF2 False]
          syn = bmMulVec hz err
          -- Channel LLR: log((1-p)/p) for p=0.1
          p = 0.1
          llr = log ((1 - p) / p)
          channelLLR = VU.replicate n llr
          result = bpDecode defaultBPConfig hz syn channelLLR
      bpConverged result @?= True
      -- Correction should satisfy H * correction = syndrome
      bmMulVec hz (bpCorrection result) @?= syn

  , testCase "decode no-error syndrome" $ do
      let code = repetitionCode 5
          hz = cssHZ code
          n = 5
          syn = bvZero (bmNumRows hz)
          p = 0.1
          llr = log ((1 - p) / p)
          channelLLR = VU.replicate n llr
          result = bpDecode defaultBPConfig hz syn channelLLR
      bpConverged result @?= True
      bpCorrection result @?= bvZero n

  , testCase "correction satisfies syndrome equation" $ do
      let code = repetitionCode 7
          hz = cssHZ code
          n = 7
          -- Error on qubit 0
          err = bvFromList [GF2 True, GF2 False, GF2 False, GF2 False,
                           GF2 False, GF2 False, GF2 False]
          syn = bmMulVec hz err
          p = 0.1
          llr = log ((1 - p) / p)
          channelLLR = VU.replicate n llr
          result = bpDecode defaultBPConfig hz syn channelLLR
      bmMulVec hz (bpCorrection result) @?= syn
  ]
