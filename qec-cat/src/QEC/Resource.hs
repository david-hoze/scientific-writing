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
import QEC.Resource.Layout (CodeFamily(..))
import qualified QEC.Resource.Layout as Layout

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

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
                  -> FactoryParams -> ResourceEstimate
estimateResources algo catParams codeFamily factory =
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
      LDPCCat       -> pZ ch            -- Z-only, like repetition cat

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

    -- Qubit layout (code-family-aware)
    nData     = Layout.dataQubits     codeFamily nLogical d
    nSyndrome = Layout.syndromeQubits codeFamily nLogical d
    nRouting  = Layout.routingQubits  codeFamily nLogical

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
--
-- RepetitionCat uses a circuit-level phenomenological threshold (~2.4%)
-- rather than the code-capacity threshold (~11%) to account for noisy
-- syndrome extraction, matching the Gouzien et al. noise model.
--
-- LDPCCat uses a circuit-level threshold (~4%) estimated from the
-- phenomenological results of Ruiz et al.
codeParams :: CodeFamily -> (Double, Double)
codeParams RepetitionCat = (0.1, 0.024)   -- circuit-level threshold ~2.4%
codeParams SurfaceCode   = (0.1, 0.01)    -- surface code threshold ~1%
codeParams LDPCCat       = (0.1, 0.04)    -- LDPC-cat circuit-level ~4%
