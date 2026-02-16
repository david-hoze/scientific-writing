module Main (main) where

import Test.Tasty.Bench
import Control.DeepSeq (rnf)

import QEC.GF2

main :: IO ()
main = defaultMain
  [ bgroup "BinVec"
    [ bench "bvXor 10000" $ nf (uncurry bvXor) (v10k, v10k')
    , bench "bvWeight 10000" $ nf bvWeight v10k
    , bench "bvInnerGF2 10000" $ nf (uncurry bvInnerGF2) (v10k, v10k')
    ]
  ]
  where
    v10k  = bvFromList (replicate 10000 (GF2 True))
    v10k' = bvFromList (take 10000 $ cycle [GF2 True, GF2 False])
