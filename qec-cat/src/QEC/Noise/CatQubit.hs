-- | Cat qubit noise model.
--
-- Cat qubits exponentially suppress bit-flip (X) errors while
-- phase-flip (Z) errors grow linearly with photon number.
-- This produces an extreme noise bias that can be exploited
-- by tailored QEC codes.
module QEC.Noise.CatQubit
  ( CatQubitParams(..)
  , defaultCatParams
  , catQubitChannel
  ) where

import Control.DeepSeq (NFData(..))
import QEC.Noise

-- | Physical parameters for a cat qubit.
data CatQubitParams = CatQubitParams
  { cqAlphaSq   :: {-# UNPACK #-} !Double  -- ^ Mean photon number |alpha|^2
  , cqKappa1    :: {-# UNPACK #-} !Double  -- ^ Single-photon loss rate (Hz)
  , cqKappa2    :: {-# UNPACK #-} !Double  -- ^ Two-photon stabilization rate (Hz)
  , cqTCycle    :: {-# UNPACK #-} !Double  -- ^ QEC cycle time (seconds)
  , cqGamma     :: {-# UNPACK #-} !Double  -- ^ Squeezing parameter for X suppression
  } deriving stock (Show, Eq)

instance NFData CatQubitParams where
  rnf (CatQubitParams a k1 k2 t g) =
    a `seq` k1 `seq` k2 `seq` t `seq` g `seq` ()

-- | Default cat qubit parameters from the literature.
-- |alpha|^2 = 19, kappa_1 = 1 kHz, kappa_2 = 100 MHz,
-- T_cycle = 500 ns, gamma = 2.0.
defaultCatParams :: CatQubitParams
defaultCatParams = CatQubitParams
  { cqAlphaSq = 19.0
  , cqKappa1  = 1.0e3       -- 1 kHz
  , cqKappa2  = 1.0e8       -- 100 MHz
  , cqTCycle  = 500.0e-9    -- 500 ns
  , cqGamma   = 2.0
  }

-- | Compute the Pauli noise channel for a cat qubit.
--
-- Formulas (from Puri et al. 2020, Guillaud & Mirrahimi 2019):
--
-- @
-- p_X ~ (kappa_1 / kappa_2) * |alpha|^2 * exp(-gamma * |alpha|^2) * kappa_2 * T_cycle
-- p_Z ~ kappa_1 * |alpha|^2 * T_cycle
-- p_Y ~ p_X * p_Z  (independent X and Z errors)
-- @
--
-- At default parameters (|alpha|^2 = 19), the bias p_Z/p_X ~ 10^6.
catQubitChannel :: CatQubitParams -> PauliChannel
catQubitChannel params = PauliChannel px py pz
  where
    alpha2 = cqAlphaSq params
    k1     = cqKappa1 params
    k2     = cqKappa2 params
    tCyc   = cqTCycle params
    gamma  = cqGamma params

    -- Bit-flip rate: exponentially suppressed
    px = (k1 / k2) * alpha2 * exp (negate (gamma * alpha2)) * k2 * tCyc

    -- Phase-flip rate: linear in photon number
    pz = k1 * alpha2 * tCyc

    -- Y error rate: product of independent X and Z
    py = px * pz
