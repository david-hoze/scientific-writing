-- | Magic state distillation factory models.
module QEC.Resource.MagicState
  ( FactoryParams(..)
  , defaultFactory
  , factoryCycleTime
  , factoriesNeeded
  ) where

-- | Parameters for a magic state distillation factory.
data FactoryParams = FactoryParams
  { factoryPhysicalQubits  :: {-# UNPACK #-} !Int    -- ^ Qubits per factory
  , factoryRounds          :: {-# UNPACK #-} !Double  -- ^ QEC rounds per distillation
  , factoryOutputErrorRate :: {-# UNPACK #-} !Double  -- ^ Error rate of output state
  } deriving stock (Show)

-- | Default factory parameters (Ruiz et al., 2025).
-- 53 physical qubits, 5.5 QEC rounds, 3e-7 output error rate.
defaultFactory :: FactoryParams
defaultFactory = FactoryParams
  { factoryPhysicalQubits  = 53
  , factoryRounds          = 5.5
  , factoryOutputErrorRate = 3e-7
  }

-- | Factory cycle time in seconds.
-- factory_cycle_time = d * physical_cycle_time * rounds_per_distillation
factoryCycleTime :: FactoryParams -> Int -> Double -> Double
factoryCycleTime factory codeDistance physCycleTimeSec =
  fromIntegral codeDistance * physCycleTimeSec * factoryRounds factory

-- | Number of factories needed to produce all magic states in time.
-- factories = ceil(toffoli_count * factory_cycle_time / runtime)
factoriesNeeded :: FactoryParams -> Int -> Double -> Double -> Double -> Int
factoriesNeeded factory codeDistance physCycleTimeSec toffoliCount runtime =
  let fct = factoryCycleTime factory codeDistance physCycleTimeSec
      rate = 1.0 / fct  -- states per factory per second
  in max 1 (ceiling (toffoliCount / (rate * runtime)))
