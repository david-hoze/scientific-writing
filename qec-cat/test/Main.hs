module Main (main) where

import Test.Tasty
import qualified QEC.GF2.Test

main :: IO ()
main = defaultMain $ testGroup "qec-cat"
  [ QEC.GF2.Test.tests
  ]
