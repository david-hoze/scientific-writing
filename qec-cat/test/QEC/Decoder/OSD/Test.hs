module QEC.Decoder.OSD.Test (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Vector.Unboxed as VU

import QEC.GF2
import QEC.GF2.Matrix
import QEC.Decoder.BP
import QEC.Decoder.OSD
import QEC.Code.CSS
import QEC.Code.Repetition

tests :: TestTree
tests = testGroup "QEC.Decoder.OSD"
  [ testCase "OSD decodes single error on rep code d=5" $ do
      let code = repetitionCode 5
          hz = cssHZ code
          n = 5
          err = bvFromList [GF2 False, GF2 False, GF2 True, GF2 False, GF2 False]
          syn = bmMulVec hz err
          p = 0.1
          llr = log ((1 - p) / p)
          softOut = VU.replicate n llr
          correction = osdDecode hz syn softOut
      bmMulVec hz correction @?= syn

  , testCase "BP+OSD decodes single error on rep code d=7" $ do
      let code = repetitionCode 7
          hz = cssHZ code
          n = 7
          err = bvFromList [GF2 False, GF2 False, GF2 False, GF2 True,
                           GF2 False, GF2 False, GF2 False]
          syn = bmMulVec hz err
          p = 0.1
          llr = log ((1 - p) / p)
          channelLLR = VU.replicate n llr
          result = bpOsdDecode defaultBPConfig hz syn channelLLR
      bmMulVec hz (bpCorrection result) @?= syn

  , testCase "OSD all single errors on rep code d=5" $ do
      let code = repetitionCode 5
          hz = cssHZ code
          n = 5
          p = 0.1
          llr = log ((1 - p) / p)
          softOut = VU.replicate n llr
      sequence_
        [ do let err = bvFromList [ if j == i then GF2 True else GF2 False
                                  | j <- [0..n-1] ]
                 syn = bmMulVec hz err
                 correction = osdDecode hz syn softOut
             bmMulVec hz correction @?= syn
        | i <- [0..n-1]
        ]

  , testCase "BP+OSD no-error syndrome" $ do
      let code = repetitionCode 5
          hz = cssHZ code
          n = 5
          syn = bvZero (bmNumRows hz)
          p = 0.1
          channelLLR = VU.replicate n (log ((1 - p) / p))
          result = bpOsdDecode defaultBPConfig hz syn channelLLR
      bpCorrection result @?= bvZero n
  ]
