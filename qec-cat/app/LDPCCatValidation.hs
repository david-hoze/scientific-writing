-- | LDPC-cat code validation (Milestone 3).
--
-- Runs Z-only Monte Carlo simulation on the [136, 34, 22] fractal LDPC code
-- from Ruiz et al. and sweeps physical error rate to check error correction.
module Main (main) where

import Data.Word (Word64)
import Text.Printf (printf)

import QEC.GF2.Matrix (bmNumRows)
import QEC.GF2.Gauss (rank)
import QEC.Code.CSS (cssNumQubits, cssNumLogical, cssHZ)
import QEC.Code.LDPCCat (ldpcCatCode)
import QEC.Decoder.BP (defaultBPConfig, bpOsdOrder)
import QEC.Simulation

pValues :: [Double]
pValues = [0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.10]

numTrials :: Int
numTrials = 10000

baseSeed :: Word64
baseSeed = 2401

main :: IO ()
main = do
  let code = ldpcCatCode 0
      hz = cssHZ code
      n = cssNumQubits code
      k = cssNumLogical code
      r = rank hz

  putStrLn "LDPC-Cat Code Validation (Ruiz et al. arXiv:2401.09541)"
  putStrLn "======================================================="
  printf "Code parameters: [n=%d, k=%d, d=22]\n" n k
  printf "H_Z dimensions:  %d x %d,  rank = %d\n" (bmNumRows hz) n r
  printf "Trials per point: %d\n" numTrials
  printf "Decoder: BP + OSD-%d\n" osdOrd
  putStrLn ""

  -- Header
  printf "%8s  %12s  %10s\n" ("pZ" :: String) ("logical_err" :: String) ("status" :: String)
  putStrLn (replicate 34 '-')

  -- Sweep
  mapM_ (\pZ -> do
    let config = defaultSimConfig
          { simNumTrials = numTrials
          , simBPConfig  = defaultBPConfig { bpOsdOrder = osdOrd }
          }
        result = runSimulation config code pZ (baseSeed + round (pZ * 1000))
        ler = logicalErrorRate result
        status :: String
        status
          | ler < pZ  = "CORRECTING"
          | otherwise = "above_phys"
    printf "%8.4f  %12.6f  %10s\n" pZ ler status
    ) pValues

  putStrLn ""
  putStrLn "Expected: error correction (CORRECTING) for pZ below threshold (~5-8%)."

-- | OSD order: min(floor((d-1)/2), 5) to keep runtime practical.
-- d=22 => floor(21/2) = 10, capped at 5.
osdOrd :: Int
osdOrd = 5
