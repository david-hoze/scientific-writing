-- | Symplectic algebra for Pauli operators.
--
-- Pauli strings are represented in the binary symplectic form:
-- each n-qubit Pauli is a pair of 'BinVec' (x-bits, z-bits) plus
-- a phase (0..3 representing i^phase). The symplectic inner product
-- determines commutativity.
module QEC.Symplectic
  ( PauliOp(..)
  , PauliString(..)
  , pauliFromXZ
  , pauliFromList
  , pauliToList
  , pauliNumQubits
  , symplecticInner
  , pauliCommutes
  , pauliMul
  ) where

import Control.DeepSeq (NFData(..))

import QEC.GF2

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | Single-qubit Pauli operator.
data PauliOp = I | X | Y | Z
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | n-qubit Pauli string: product of single-qubit Paulis with phase i^psPhase.
-- Represented as x-bits and z-bits where:
-- x=0,z=0 -> I; x=1,z=0 -> X; x=1,z=1 -> Y; x=0,z=1 -> Z
data PauliString = PauliString
  { psXBits :: !BinVec
  , psZBits :: !BinVec
  , psPhase :: {-# UNPACK #-} !Int
  } deriving stock (Show, Eq)

instance NFData PauliString where
  rnf (PauliString x z p) = rnf x `seq` rnf z `seq` p `seq` ()

------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------

-- | Construct a Pauli string from x-bits and z-bits with phase 0.
pauliFromXZ :: BinVec -> BinVec -> PauliString
pauliFromXZ xbits zbits = PauliString xbits zbits 0

-- | Construct from a list of single-qubit Paulis (phase 0).
pauliFromList :: [PauliOp] -> PauliString
pauliFromList ops = PauliString xbits zbits 0
  where
    xbits = bvFromList [ GF2 (op == X || op == Y) | op <- ops ]
    zbits = bvFromList [ GF2 (op == Z || op == Y) | op <- ops ]

-- | Convert to a list of single-qubit Paulis (phase is discarded).
pauliToList :: PauliString -> [PauliOp]
pauliToList ps =
  [ toOp (bvGetBit (psXBits ps) i) (bvGetBit (psZBits ps) i)
  | i <- [0 .. pauliNumQubits ps - 1]
  ]
  where
    toOp (GF2 False) (GF2 False) = I
    toOp (GF2 True)  (GF2 False) = X
    toOp (GF2 True)  (GF2 True)  = Y
    toOp (GF2 False) (GF2 True)  = Z

-- | Number of qubits.
pauliNumQubits :: PauliString -> Int
pauliNumQubits = bvLength . psXBits

------------------------------------------------------------------------
-- Symplectic inner product
------------------------------------------------------------------------

-- | Symplectic inner product:
-- @symplecticInner p1 p2 = sum_i (x1_i*z2_i + z1_i*x2_i) mod 2@.
-- Two Paulis commute iff this is @GF2 False@.
symplecticInner :: PauliString -> PauliString -> GF2
symplecticInner p1 p2 =
  bvInnerGF2 (psXBits p1) (psZBits p2)
  + bvInnerGF2 (psZBits p1) (psXBits p2)

-- | Two Pauli strings commute iff their symplectic inner product is 0.
pauliCommutes :: PauliString -> PauliString -> Bool
pauliCommutes p1 p2 = symplecticInner p1 p2 == GF2 False

------------------------------------------------------------------------
-- Pauli multiplication
------------------------------------------------------------------------

-- | Multiply two Pauli strings. The result x-bits and z-bits are XORed,
-- and the phase is updated according to the commutation rules.
pauliMul :: PauliString -> PauliString -> PauliString
pauliMul (PauliString x1 z1 ph1) (PauliString x2 z2 ph2) =
  PauliString xr zr phr
  where
    xr = bvXor x1 x2
    zr = bvXor z1 z2
    -- Phase contribution: for each qubit, compute the phase from
    -- multiplying single-qubit Paulis. The total extra phase (mod 4)
    -- comes from sum of per-qubit contributions.
    -- Per qubit: phase = 2 * (x1&z1&~x2&~z2 type terms)
    -- Simplified: use the formula phase += 2*(x1&z2) - 2*(z1&x2)
    -- which mod 4 gives the right answer.
    -- Actually the exact rule: when multiplying sigma_a * sigma_b,
    -- the extra phase is determined by the Levi-Civita structure.
    -- We use: extra = sum_i f(x1_i,z1_i, x2_i,z2_i) where
    -- f computes the phase from the single-qubit product.
    n = bvLength x1
    extraPhase = sum [ singlePhase
                         (unGF2 $ bvGetBit x1 i) (unGF2 $ bvGetBit z1 i)
                         (unGF2 $ bvGetBit x2 i) (unGF2 $ bvGetBit z2 i)
                     | i <- [0 .. n - 1]
                     ]
    phr = (ph1 + ph2 + extraPhase) `mod` 4

-- | Phase contribution from multiplying two single-qubit Paulis.
-- Returns 0, 1, 2, or 3 representing i^result.
-- I*P = P (0), P*I = P (0), P*P = I (0),
-- X*Y = iZ (1), Y*X = -iZ (3), Y*Z = iX (1), Z*Y = -iX (3),
-- Z*X = iY (1), X*Z = -iY (3)
singlePhase :: Bool -> Bool -> Bool -> Bool -> Int
singlePhase x1 z1 x2 z2 = case (pauliIdx x1 z1, pauliIdx x2 z2) of
  (0, _) -> 0  -- I * anything
  (_, 0) -> 0  -- anything * I
  (a, b) | a == b -> 0  -- P * P = I
  (1, 2) -> 1  -- X * Y = iZ
  (2, 1) -> 3  -- Y * X = -iZ
  (2, 3) -> 1  -- Y * Z = iX
  (3, 2) -> 3  -- Z * Y = -iX
  (3, 1) -> 1  -- Z * X = iY
  (1, 3) -> 3  -- X * Z = -iY
  _      -> 0  -- shouldn't happen
  where
    pauliIdx False False = 0 :: Int  -- I
    pauliIdx True  False = 1  -- X
    pauliIdx True  True  = 2  -- Y
    pauliIdx False True  = 3  -- Z
