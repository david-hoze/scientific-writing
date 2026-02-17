-- | Surface code threshold validation.
--
-- Sweeps the rotated surface code at d = {3, 5, 7} under depolarizing
-- code-capacity noise and checks that curves cross near p ~ 10.3%.
module Main (main) where

import Data.Word (Word64)
import Text.Printf (printf)

import QEC.Code.Surface (surfaceCode)
import QEC.Decoder.BP (defaultBPConfig, bpOsdOrder)
import QEC.Simulation

distances :: [Int]
distances = [3, 5, 7]

-- | Physical error rates to sweep around the expected ~10.3% threshold.
pValues :: [Double]
pValues = [0.04, 0.06, 0.07, 0.08, 0.085, 0.09, 0.095, 0.10, 0.103, 0.105, 0.11, 0.115, 0.12]

numTrials :: Int
numTrials = 10000

baseSeed :: Word64
baseSeed = 42

main :: IO ()
main = do
  putStrLn "Surface Code Threshold Validation (depolarizing code-capacity)"
  putStrLn "==============================================================="
  printf "Trials per point: %d\n" numTrials
  putStrLn "Decoder: BP + OSD-w  (w = (d-1)/2)"
  putStrLn ""

  -- Header
  printf "    p    "
  mapM_ (\d -> printf "   d=%-2d   " d) distances
  putStrLn ""
  putStrLn (replicate (9 + 10 * length distances) '-')

  -- Compute all results
  let results = [ (p, [ runPoint d p | d <- distances ]) | p <- pValues ]

  -- Print table
  mapM_ (\(p, lers) -> do
    printf "%7.4f  " p
    mapM_ (\ler -> printf "%8.4f  " ler) lers
    putStrLn ""
    ) results

  -- Threshold analysis
  putStrLn ""
  putStrLn "Threshold analysis:"
  putStrLn "-------------------"
  mapM_ (\(p, lers) -> do
    let ordering
          | and (zipWith (<=) lers (tail lers)) = "d3 <= d5 <= d7 (ABOVE threshold)" :: String
          | and (zipWith (>=) lers (tail lers)) = "d3 >= d5 >= d7 (BELOW threshold)"
          | otherwise                           = "non-monotonic  (NEAR threshold)"
    printf "  p = %.4f : %s\n" p ordering
    ) results

runPoint :: Int -> Double -> Double
runPoint d p =
  let code   = surfaceCode d
      osdOrd = (d - 1) `div` 2  -- OSD order matches code's correction capability
      config = defaultSimConfig
        { simNumTrials = numTrials
        , simBPConfig  = defaultBPConfig { bpOsdOrder = osdOrd }
        }
      pEff   = 2.0 * p / 3.0
      result = runCSSSimulation config code pEff pEff (baseSeed + fromIntegral d)
  in logicalErrorRate result
