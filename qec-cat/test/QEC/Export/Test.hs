module QEC.Export.Test (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Aeson (Value, decode)

import QEC.Noise.CatQubit
import QEC.Resource
import QEC.Resource.Algorithm
import QEC.Resource.MagicState
import QEC.Resource.Layout
import QEC.Export

tests :: TestTree
tests = testGroup "QEC.Export"
  [ testCase "exportJSON produces valid JSON" $ do
      let est = estimateResources ecdlp256 defaultCatParams RepetitionCat
                  defaultFactory defaultLayoutParams
          json = exportJSON [est]
      assertBool "JSON is empty" (BL.length json > 0)
      -- Verify it parses back as JSON
      case decode json :: Maybe [Value] of
        Nothing -> assertFailure "Failed to parse JSON output"
        Just vs -> length vs @?= 1

  , testCase "exportCSV has header and data row" $ do
      let est = estimateResources ecdlp256 defaultCatParams RepetitionCat
                  defaultFactory defaultLayoutParams
          csv = exportCSV [est]
          ls = BLC.lines csv
      assertBool "CSV has fewer than 2 lines" (length ls >= 2)
      -- Header should contain expected fields
      let headerLine = BLC.unpack (head ls)
      assertBool "header missing total_qubits" ("total_qubits" `isInfixOf` headerLine)

  , testCase "exportCSV with multiple estimates" $ do
      let est1 = estimateResources ecdlp256 defaultCatParams RepetitionCat
                   defaultFactory defaultLayoutParams
          est2 = estimateResources shorRSA2048 defaultCatParams RepetitionCat
                   defaultFactory defaultLayoutParams
          csv = exportCSV [est1, est2]
          ls = BLC.lines csv
      -- 1 header + 2 data rows
      assertBool ("expected 3 lines, got " ++ show (length ls))
        (length ls >= 3)
  ]

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(_:xs') = xs : tails xs'
