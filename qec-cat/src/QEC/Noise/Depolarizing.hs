-- | Standard depolarizing noise channel.
module QEC.Noise.Depolarizing
  ( depolarizing
  ) where

import QEC.Noise

-- | Symmetric depolarizing channel with physical error rate @p@.
-- Each of X, Y, Z occurs with probability p/3.
depolarizing :: Double -> PauliChannel
depolarizing p = PauliChannel (p / 3) (p / 3) (p / 3)
