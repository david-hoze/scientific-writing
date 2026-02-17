-- | Resource comparison across code families for ECDLP-256 and RSA-2048.
--
-- Produces a comparison table and exports results to JSON.
module Main (main) where

import qualified Data.ByteString.Lazy as BL
import System.Directory (createDirectoryIfMissing)

import QEC.Noise.CatQubit
import QEC.Resource
import QEC.Resource.Algorithm
import QEC.Resource.MagicState
import QEC.Export

main :: IO ()
main = do
  let families = [RepetitionCat, LDPCCat, SurfaceCode]
      algos    = [ecdlp256, shorRSA2048]

      estimates = [ estimateResources algo defaultCatParams fam defaultFactory
                  | algo <- algos
                  , fam  <- families
                  ]

  -- Print comparison table
  putStrLn $ padRight 20 "Algorithm"
          ++ padRight 16 "Code Family"
          ++ padRight 10 "Distance"
          ++ padRight 12 "Data"
          ++ padRight 12 "Syndrome"
          ++ padRight 10 "Routing"
          ++ padRight 12 "Factory"
          ++ padRight 12 "Total"
          ++ "Factories"
  putStrLn (replicate 104 '-')

  mapM_ printRow
    [ (algo, fam, estimateResources algo defaultCatParams fam defaultFactory)
    | algo <- algos
    , fam  <- families
    ]

  -- Export JSON
  createDirectoryIfMissing True "results"
  let json = exportJSON estimates
  BL.writeFile "results/resource-comparison.json" json
  putStrLn ""
  putStrLn "Wrote results/resource-comparison.json"

printRow :: (Algorithm, CodeFamily, ResourceEstimate) -> IO ()
printRow (algo, _fam, est) =
  putStrLn $ padRight 20 (algoName algo)
          ++ padRight 16 (reCodeFamily est)
          ++ padRight 10 (show (reCodeDistance est))
          ++ padRight 12 (show (reDataQubits est))
          ++ padRight 12 (show (reSyndromeQubits est))
          ++ padRight 10 (show (reRoutingQubits est))
          ++ padRight 12 (show (reFactoryQubits est))
          ++ padRight 12 (show (reTotalQubits est))
          ++ show (reNumFactories est)

padRight :: Int -> String -> String
padRight n s = s ++ replicate (max 0 (n - length s)) ' '
