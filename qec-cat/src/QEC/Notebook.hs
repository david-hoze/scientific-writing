-- | Notebook convenience module: sweep combinators, comparison, and re-exports.
--
-- Auto-imported in the qec-notebook session so researchers can use
-- ldpcCatCode, sweep, compareArchitectures, etc. without setup.
module QEC.Notebook
  ( -- * Sweep combinators
    sweep
  , sweepIO
  , sweepCodes
  , noiseRange
  , SweepResult(..)

    -- * Cross-architecture comparison
  , compareArchitectures
  , compareAll

    -- * Convenience constructors
  , catNoise
  , catNoiseAt
  , catPZ

    -- * Re-exports (auto-available in notebook cells)
  , module QEC.Code.CSS
  , module QEC.Code.Repetition
  , module QEC.Code.Surface
  , module QEC.Code.LDPCCat
  , module QEC.Decoder.BP
  , module QEC.Noise
  , module QEC.Noise.CatQubit
  , module QEC.Simulation
  , module QEC.Resource
  , module QEC.Resource.Algorithm
  , module QEC.Resource.MagicState
  ) where

import Control.DeepSeq (NFData(..), force)
import Control.Monad (forM)
import Data.Hashable (hash)
import Data.Word (Word64)
import System.IO (hFlush, stdout)

import QEC.Code.CSS
import QEC.Code.LDPCCat
import QEC.Code.Repetition
import QEC.Code.Surface
import QEC.Decoder.BP
import QEC.Noise
import QEC.Noise.CatQubit
import QEC.Resource
import QEC.Resource.Algorithm
import QEC.Resource.MagicState
import QEC.Simulation

------------------------------------------------------------------------
-- Sweep result
------------------------------------------------------------------------

-- | Result of a single (code, noise) simulation point in a sweep.
data SweepResult = SweepResult
  { swCodeLabel  :: !String
  , swPhysicalPZ :: !Double
  , swLogicalErr :: !Double
  , swStdError   :: !Double
  , swNumTrials  :: !Int
  , swNumErrors  :: !Int
  } deriving stock (Show)

instance NFData SweepResult where
  rnf (SweepResult a b c d e f) =
    a `seq` b `seq` c `seq` d `seq` e `seq` f `seq` ()

------------------------------------------------------------------------
-- Sweep
------------------------------------------------------------------------

-- | Sweep codes × noise points with simulation.
--
-- Runs Monte Carlo simulation for each (code, noise) pair.
-- Returns results suitable for threshold_plot rendering.
sweep
  :: [(String, CSSCode)]
  -> [Double]
  -> SimConfig
  -> [SweepResult]
sweep codes pzValues config =
  map (force . runPoint)
    [ (label, code, pz) | (label, code) <- codes, pz <- pzValues ]
  where
    runPoint (label, code, pz) =
      let seed   = hashSweepPoint label pz
          result = runSimulation config code pz seed
          rate   = logicalErrorRate result
          n      = fromIntegral (simTotalTrials result)
          stdErr = sqrt (rate * (1 - rate) / n)
      in SweepResult
        { swCodeLabel  = label
        , swPhysicalPZ = pz
        , swLogicalErr = rate
        , swStdError   = stdErr
        , swNumTrials  = simTotalTrials result
        , swNumErrors  = simLogicalErrors result
        }

    hashSweepPoint label pz =
      fromIntegral (hash (label, round (pz * 1e6) :: Int)) :: Word64

-- | Like 'sweep' but prints progress markers to stdout after each point.
-- The markers have the format @___QEC_PROGRESS___ <completed> <total>@
-- and are intercepted by the notebook server to send progress messages.
sweepIO :: [(String, CSSCode)] -> [Double] -> SimConfig -> IO [SweepResult]
sweepIO codes pzValues config = do
  let allPoints = [(label, code, pz) | (label, code) <- codes, pz <- pzValues]
      total = length allPoints
  forM (zip [1..] allPoints) $ \(i, (label, code, pz)) -> do
    let result = force (runPoint (label, code, pz))
    putStrLn ("___QEC_PROGRESS___ " ++ show i ++ " " ++ show total)
    hFlush stdout
    return result
  where
    runPoint (label, code, pz) =
      let seed   = hashSweepPoint label pz
          result = runSimulation config code pz seed
          rate   = logicalErrorRate result
          n      = fromIntegral (simTotalTrials result)
          stdErr = sqrt (rate * (1 - rate) / n)
      in SweepResult
        { swCodeLabel  = label
        , swPhysicalPZ = pz
        , swLogicalErr = rate
        , swStdError   = stdErr
        , swNumTrials  = simTotalTrials result
        , swNumErrors  = simLogicalErrors result
        }

    hashSweepPoint label pz =
      fromIntegral (hash (label, round (pz * 1e6) :: Int)) :: Word64

-- | Build labeled code list from a code family and parameter range.
sweepCodes :: String -> (Int -> CSSCode) -> [Int] -> [(String, CSSCode)]
sweepCodes family mkCode params =
  [ (family ++ " ℓ=" ++ show p, mkCode p) | p <- params ]

-- | Build noise points from a range of p_Z values.
-- Generates n logarithmically spaced points between lo and hi.
noiseRange :: Double -> Double -> Int -> [Double]
noiseRange lo hi n =
  [ exp (logLo + fromIntegral i * step) | i <- [0 .. n - 1] ]
  where
    logLo = log lo
    logHi = log hi
    step  = (logHi - logLo) / fromIntegral (max 1 (n - 1))

------------------------------------------------------------------------
-- Compare architectures
------------------------------------------------------------------------

-- | Compare resource estimates across code families for a given algorithm.
compareArchitectures
  :: Algorithm
  -> CatQubitParams
  -> [CodeFamily]
  -> [ResourceEstimate]
compareArchitectures algo params families =
  [ estimateResources algo params fam defaultFactory
  | fam <- families
  ]

-- | Compare across multiple algorithms AND code families.
compareAll
  :: [Algorithm]
  -> CatQubitParams
  -> [CodeFamily]
  -> [ResourceEstimate]
compareAll algos params families =
  [ estimateResources algo params fam defaultFactory
  | algo <- algos
  , fam  <- families
  ]

------------------------------------------------------------------------
-- Convenience aliases
------------------------------------------------------------------------

-- | Compute the Pauli channel for cat qubit parameters.
catNoise :: CatQubitParams -> PauliChannel
catNoise = catQubitChannel

-- | Cat qubit noise at a specific |α|², using defaults for everything else.
catNoiseAt :: Double -> PauliChannel
catNoiseAt alphaSq = catQubitChannel (defaultCatParams { cqAlphaSq = alphaSq })

-- | Extract just p_Z from cat qubit parameters.
catPZ :: CatQubitParams -> Double
catPZ = pZ . catQubitChannel
