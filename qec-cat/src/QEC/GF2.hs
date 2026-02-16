-- | GF(2) arithmetic and bit-packed binary vectors.
--
-- 'GF2' is a newtype over 'Bool' with addition = XOR and multiplication = AND.
-- 'BinVec' stores binary vectors as packed 'Word64' arrays (LSB-first).
--
-- __Critical invariant__: unused high bits in the last 'Word64' of a 'BinVec'
-- MUST be zero. 'bvCleanup' enforces this after operations that may violate it.
module QEC.GF2
  ( GF2(..)
  , BinVec(..)
  , bvZero, bvFromList, bvToList
  , bvXor, bvAnd, bvWeight, bvInnerGF2
  , bvGetBit, bvSetBit, bvClearBit, bvFlipBit
  , bvLength, bvCleanup, wordsNeeded
  ) where

import Control.DeepSeq (NFData(..))
import Data.Bits
import Data.Word (Word64)
import qualified Data.Vector.Unboxed as VU

------------------------------------------------------------------------
-- GF(2)
------------------------------------------------------------------------

-- | Element of the Galois field GF(2).
newtype GF2 = GF2 { unGF2 :: Bool }
  deriving newtype (Eq, Ord, Show, NFData)

instance Num GF2 where
  GF2 a + GF2 b = GF2 (a /= b)
  GF2 a * GF2 b = GF2 (a && b)
  abs    = id
  signum = id
  fromInteger n = GF2 (odd n)
  negate = id

------------------------------------------------------------------------
-- BinVec
------------------------------------------------------------------------

-- | Bit-packed binary vector of length @bvLen@.
-- Bits are stored LSB-first in 'Word64' chunks.
data BinVec = BinVec
  { bvLen   :: {-# UNPACK #-} !Int
  , bvWords :: !(VU.Vector Word64)
  } deriving stock (Show)

instance Eq BinVec where
  a == b = bvLen a == bvLen b && bvWords a == bvWords b

instance NFData BinVec where
  rnf (BinVec n ws) = n `seq` rnf ws

-- | Number of 'Word64' values needed to store @n@ bits.
wordsNeeded :: Int -> Int
wordsNeeded n = (n + 63) `unsafeShiftR` 6
{-# INLINE wordsNeeded #-}

-- | Zero vector of length @n@.
bvZero :: Int -> BinVec
bvZero n = BinVec n (VU.replicate (wordsNeeded n) 0)

-- | Construct a 'BinVec' from a list of 'GF2' values.
bvFromList :: [GF2] -> BinVec
bvFromList xs = bvCleanup $ BinVec n ws
  where
    n  = length xs
    nw = wordsNeeded n
    ws = VU.generate nw $ \wi ->
      let base = wi * 64
          buildWord _ []     = 0
          buildWord !bi (GF2 b : rest)
            | bi >= 64  = 0
            | b         = setBit (buildWord (bi + 1) rest) bi
            | otherwise = buildWord (bi + 1) rest
      in buildWord 0 (drop base xs)

-- | Convert a 'BinVec' to a list of 'GF2' values.
bvToList :: BinVec -> [GF2]
bvToList v = [ bvGetBit v i | i <- [0 .. bvLen v - 1] ]

-- | XOR of two vectors (pointwise addition in GF(2)).
bvXor :: BinVec -> BinVec -> BinVec
bvXor (BinVec n1 w1) (BinVec n2 w2)
  | n1 /= n2 = error "bvXor: length mismatch"
  | otherwise = BinVec n1 (VU.zipWith xor w1 w2)
{-# INLINE bvXor #-}

-- | AND of two vectors (pointwise multiplication in GF(2)).
bvAnd :: BinVec -> BinVec -> BinVec
bvAnd (BinVec n1 w1) (BinVec n2 w2)
  | n1 /= n2 = error "bvAnd: length mismatch"
  | otherwise = BinVec n1 (VU.zipWith (.&.) w1 w2)
{-# INLINE bvAnd #-}

-- | Hamming weight (number of set bits).
bvWeight :: BinVec -> Int
bvWeight (BinVec _ ws) = VU.foldl' (\acc w -> acc + popCount w) 0 ws
{-# INLINE bvWeight #-}

-- | Inner product over GF(2): @dot(a,b) = popCount(a AND b) mod 2@.
bvInnerGF2 :: BinVec -> BinVec -> GF2
bvInnerGF2 a b = GF2 (odd (bvWeight (bvAnd a b)))
{-# INLINE bvInnerGF2 #-}

-- | Get bit at position @i@ (0-indexed).
bvGetBit :: BinVec -> Int -> GF2
bvGetBit (BinVec _ ws) i =
  let (wordIdx, bitIdx) = i `quotRem` 64
  in GF2 (testBit (ws VU.! wordIdx) bitIdx)
{-# INLINE bvGetBit #-}

-- | Set bit at position @i@ to 1.
bvSetBit :: Int -> BinVec -> BinVec
bvSetBit i (BinVec n ws) =
  let (wordIdx, bitIdx) = i `quotRem` 64
  in BinVec n (ws VU.// [(wordIdx, setBit (ws VU.! wordIdx) bitIdx)])

-- | Clear bit at position @i@ to 0.
bvClearBit :: Int -> BinVec -> BinVec
bvClearBit i (BinVec n ws) =
  let (wordIdx, bitIdx) = i `quotRem` 64
  in BinVec n (ws VU.// [(wordIdx, clearBit (ws VU.! wordIdx) bitIdx)])

-- | Flip bit at position @i@.
bvFlipBit :: Int -> BinVec -> BinVec
bvFlipBit i (BinVec n ws) =
  let (wordIdx, bitIdx) = i `quotRem` 64
  in BinVec n (ws VU.// [(wordIdx, complementBit (ws VU.! wordIdx) bitIdx)])

-- | Length of the binary vector.
bvLength :: BinVec -> Int
bvLength = bvLen
{-# INLINE bvLength #-}

-- | Mask off unused high bits in the last 'Word64'.
-- Call after any operation that might set padding bits.
bvCleanup :: BinVec -> BinVec
bvCleanup bv@(BinVec n ws)
  | n == 0    = bv
  | r == 0    = bv
  | otherwise = BinVec n (ws VU.// [(lastIdx, ws VU.! lastIdx .&. mask)])
  where
    r       = n `rem` 64
    mask    = (1 `unsafeShiftL` r) - 1
    lastIdx = VU.length ws - 1
