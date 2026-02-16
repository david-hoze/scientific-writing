-- | Physical qubit layout computation.
module QEC.Resource.Layout
  ( LayoutParams(..)
  , defaultLayoutParams
  , dataQubits
  , syndromeQubits
  , routingQubits
  ) where

-- | Layout parameters for qubit overhead computation.
data LayoutParams = LayoutParams
  { lpQubitsPerLogical :: {-# UNPACK #-} !Int     -- ^ Physical qubits per logical qubit at distance d
  , lpSyndromeOverhead :: {-# UNPACK #-} !Double  -- ^ Syndrome extraction overhead factor
  , lpRoutingOverhead  :: {-# UNPACK #-} !Double  -- ^ Routing/lattice surgery overhead factor
  } deriving stock (Show)

-- | Default layout parameters for repetition-cat architecture.
-- For a repetition code of distance d, each logical qubit uses d data qubits
-- plus d-1 syndrome qubits.
defaultLayoutParams :: LayoutParams
defaultLayoutParams = LayoutParams
  { lpQubitsPerLogical = 1   -- multiplied by d at runtime
  , lpSyndromeOverhead = 1.0 -- syndrome qubits ≈ data qubits
  , lpRoutingOverhead  = 0.5 -- 50% routing overhead
  }

-- | Number of data qubits.
-- For repetition code: nLogical * d.
-- For surface code: nLogical * d^2.
dataQubits :: Int -> Int -> Int
dataQubits nLogical d = nLogical * d

-- | Number of syndrome extraction qubits.
-- Approximately equal to data qubits for most architectures.
syndromeQubits :: LayoutParams -> Int -> Int
syndromeQubits lp nData = ceiling (fromIntegral nData * lpSyndromeOverhead lp)

-- | Number of routing qubits for lattice surgery.
routingQubits :: LayoutParams -> Int -> Int
routingQubits lp nData = ceiling (fromIntegral nData * lpRoutingOverhead lp)
