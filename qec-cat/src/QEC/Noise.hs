-- | Pauli noise channel abstraction.
module QEC.Noise
  ( PauliChannel(..)
  , channelFromBias
  , totalErrorRate
  ) where

import Control.DeepSeq (NFData(..))

-- | A Pauli noise channel specifying independent X, Y, Z error probabilities.
data PauliChannel = PauliChannel
  { pX :: {-# UNPACK #-} !Double
  , pY :: {-# UNPACK #-} !Double
  , pZ :: {-# UNPACK #-} !Double
  } deriving stock (Show, Eq)

instance NFData PauliChannel where
  rnf (PauliChannel x y z) = x `seq` y `seq` z `seq` ()

-- | Total error probability: p_X + p_Y + p_Z.
totalErrorRate :: PauliChannel -> Double
totalErrorRate ch = pX ch + pY ch + pZ ch

-- | Construct a channel from total error rate and Z-bias.
-- Bias eta = p_Z / p_X. Assumes p_Y = p_X.
-- p_X = p_Y = p_total / (2 + eta), p_Z = eta * p_X.
channelFromBias :: Double -> Double -> PauliChannel
channelFromBias pTotal eta = PauliChannel px px pz
  where
    px = pTotal / (2.0 + eta)
    pz = eta * px
