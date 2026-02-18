module Main where

import Test.Tasty
import qualified Test.Unit.Protocol
import qualified Test.Unit.Eval
import qualified Test.Integration.WebSocket

main :: IO ()
main = do
  integ <- Test.Integration.WebSocket.tests
  defaultMain $ testGroup "qec-notebook"
    [ testGroup "Unit"
        [ Test.Unit.Eval.tests
        , Test.Unit.Protocol.tests
        ]
    , integ
    ]
