-- | End-to-end resource estimation pipeline.
--
-- Takes an algorithm, cat qubit physical parameters, and a code family
-- specification to produce total physical qubit count and computation time.
module QEC.Resource
  ( ResourceEstimate(..)
  , CodeFamily(..)
  , estimateResources
  ) where

import QEC.Noise
import QEC.Noise.CatQubit
import QEC.Resource.Algorithm
import QEC.Resource.MagicState
import QEC.Resource.Layout

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | Code family specification for resource estimation.
data CodeFamily
  = RepetitionCat    -- ^ Repetition code (phase-flip only, cat qubit)
  | SurfaceCode      -- ^ Standard surface code
  deriving stock (Show, Eq)

-- | Result of resource estimation.
data ResourceEstimate = ResourceEstimate
  { reDataQubits           :: {-# UNPACK #-} !Int
  , reSyndromeQubits       :: {-# UNPACK #-} !Int
  , reRoutingQubits        :: {-# UNPACK #-} !Int
  , reFactoryQubits        :: {-# UNPACK #-} !Int
  , reTotalQubits          :: {-# UNPACK #-} !Int
  , reRuntimeSeconds       :: {-# UNPACK #-} !Double
  , reCodeDistance          :: {-# UNPACK #-} !Int
  , reLogicalErrorPerCycle :: {-# UNPACK #-} !Double
  , reNumFactories         :: {-# UNPACK #-} !Int
  , reCodeFamily           :: !String
  } deriving stock (Show)

------------------------------------------------------------------------
-- Pipeline
------------------------------------------------------------------------

-- | Estimate physical resources for a quantum algorithm.
--
-- Pipeline:
-- 1. Compute per-logical-qubit error budget
-- 2. Find minimum code distance d
-- 3. Compute qubit counts (data + syndrome + routing + factory)
-- 4. Compute runtime and factory count (fixed-point iteration)
estimateResources :: Algorithm -> CatQubitParams -> CodeFamily
                  -> FactoryParams -> LayoutParams -> ResourceEstimate
estimateResources algo catParams codeFamily factory layout =
  ResourceEstimate
    { reDataQubits           = nData
    , reSyndromeQubits       = nSyndrome
    , reRoutingQubits        = nRouting
    , reFactoryQubits        = nFactory
    , reTotalQubits          = nData + nSyndrome + nRouting + nFactory
    , reRuntimeSeconds       = runtime
    , reCodeDistance          = d
    , reLogicalErrorPerCycle = pLogical
    , reNumFactories         = nFactories
    , reCodeFamily           = show codeFamily
    }
  where
    nLogical = algoLogicalQubits algo

    -- Physical error rate (dominant channel)
    ch = catQubitChannel catParams
    pPhys = case codeFamily of
      RepetitionCat -> pZ ch            -- phase-flip dominant
      SurfaceCode   -> pZ ch + pX ch    -- both channels

    -- Physical cycle time in seconds
    physCycleTimeSec = cqTCycle catParams

    -- Number of logical cycles
    numCycles = algoTDepth algo

    -- Per-logical-qubit-per-cycle error budget
    pBudget = algoErrorBudget algo / (fromIntegral nLogical * numCycles)

    -- Find minimum code distance d
    d = findMinDistance codeFamily pPhys pBudget

    -- Logical error rate at chosen distance
    pLogical = logicalErrorRate' codeFamily pPhys d

    -- Runtime = T-depth * logical cycle time
    -- Logical cycle time = d * physical cycle time
    logicalCycleTime = fromIntegral d * physCycleTimeSec
    runtime = algoTDepth algo * logicalCycleTime

    -- Qubit layout
    nData = dataQubits nLogical d
    nSyndrome = syndromeQubits layout nData
    nRouting = routingQubits layout nData

    -- Factory count (fixed-point: runtime depends on factory, factory depends on runtime)
    nFactories = factoriesNeeded factory d physCycleTimeSec
                   (algoToffoliCount algo) runtime
    nFactory = nFactories * factoryPhysicalQubits factory

------------------------------------------------------------------------
-- Code distance search
------------------------------------------------------------------------

-- | Find the minimum odd code distance d such that
-- the logical error rate is below the budget.
findMinDistance :: CodeFamily -> Double -> Double -> Int
findMinDistance codeFamily pPhys pBudget = go 3
  where
    go d
      | d > 1001  = d  -- safety cap
      | logicalErrorRate' codeFamily pPhys d <= pBudget = d
      | otherwise = go (d + 2)  -- only odd distances

-- | Approximate logical error rate for a code family at distance d.
-- Uses the standard scaling formula: p_L ~ A * (p/p_th)^((d+1)/2)
logicalErrorRate' :: CodeFamily -> Double -> Int -> Double
logicalErrorRate' codeFamily pPhys d =
  let (a, pTh) = codeParams codeFamily
      exponent = fromIntegral (d + 1) / 2.0
  in a * (pPhys / pTh) ** exponent

-- | Code-family-specific parameters (prefactor A, threshold p_th).
codeParams :: CodeFamily -> (Double, Double)
codeParams RepetitionCat = (0.1, 0.11)   -- repetition code threshold ~11%
codeParams SurfaceCode   = (0.1, 0.01)   -- surface code threshold ~1%
