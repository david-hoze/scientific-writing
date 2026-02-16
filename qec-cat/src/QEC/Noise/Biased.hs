-- | Asymmetric (biased) Pauli noise channel.
module QEC.Noise.Biased
  ( biasedChannel
  , catBias
  ) where

import QEC.Noise

-- | Construct a biased noise channel.
-- @biasedChannel pz eta@ creates a channel with Z-error probability @pz@,
-- and X/Y error probabilities @pz / eta@ each.
biasedChannel :: Double -> Double -> PauliChannel
biasedChannel pz eta = PauliChannel px px pz
  where px = pz / eta

-- | Compute the bias ratio eta = p_Z / p_X for a channel.
-- Returns 'Infinity' if p_X == 0.
catBias :: PauliChannel -> Double
catBias ch
  | pX ch == 0 = 1 / 0  -- Infinity
  | otherwise  = pZ ch / pX ch
