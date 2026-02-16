-- | Pre-defined quantum algorithms with resource parameters.
module QEC.Resource.Algorithm
  ( Algorithm(..)
  , shorRSA2048
  , ecdlp256
  ) where

-- | A quantum algorithm's resource requirements.
data Algorithm = Algorithm
  { algoName          :: !String
  , algoLogicalQubits :: {-# UNPACK #-} !Int     -- ^ Number of logical qubits
  , algoToffoliCount  :: {-# UNPACK #-} !Double  -- ^ Total Toffoli gates
  , algoTDepth        :: {-# UNPACK #-} !Double  -- ^ T/Toffoli depth (sequential)
  , algoErrorBudget   :: {-# UNPACK #-} !Double  -- ^ Target total failure probability
  } deriving stock (Show)

-- | Shor's algorithm for RSA-2048 (Gidney, May 2025).
shorRSA2048 :: Algorithm
shorRSA2048 = Algorithm
  { algoName          = "Shor RSA-2048"
  , algoLogicalQubits = 1400
  , algoToffoliCount  = 6.5e9
  , algoTDepth        = 1.0e9
  , algoErrorBudget   = 1.0 / 3.0
  }

-- | ECDLP 256-bit (Gouzien et al., 2023).
ecdlp256 :: Algorithm
ecdlp256 = Algorithm
  { algoName          = "ECDLP 256-bit"
  , algoLogicalQubits = 768    -- 2 * 256 + 256
  , algoToffoliCount  = 1.28e11
  , algoTDepth        = 1.0e9
  , algoErrorBudget   = 1.0 / 3.0
  }
