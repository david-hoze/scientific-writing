-- | Dense binary matrix over GF(2).
--
-- Row-major storage: each row is @bmWordsPerRow@ 'Word64' values packed
-- contiguously in a flat unboxed vector. Row operations ('bmAddRows',
-- 'bmSwapRows') operate at 'Word64' granularity for speed.
module QEC.GF2.Matrix
  ( BinMatrix(..)
  , bmZero, bmIdentity, bmFromRows, bmFromList
  , bmNumRows, bmNumCols
  , bmGetRow, bmSetRow, bmGetEntry
  , bmAddRows, bmSwapRows
  , bmTranspose, bmMul, bmMulVec
  ) where

import Control.DeepSeq (NFData(..))
import Data.Bits
import Data.Word (Word64)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import QEC.GF2

------------------------------------------------------------------------
-- Type
------------------------------------------------------------------------

-- | Dense binary matrix: @bmRows@ x @bmCols@.
data BinMatrix = BinMatrix
  { bmRows       :: {-# UNPACK #-} !Int
  , bmCols       :: {-# UNPACK #-} !Int
  , bmWordsPerRow :: {-# UNPACK #-} !Int
  , bmData       :: !(VU.Vector Word64)
  } deriving stock (Show)

instance Eq BinMatrix where
  a == b = bmRows a == bmRows b
        && bmCols a == bmCols b
        && bmData a == bmData b

instance NFData BinMatrix where
  rnf (BinMatrix r c w d) = r `seq` c `seq` w `seq` rnf d

------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------

-- | Number of rows.
bmNumRows :: BinMatrix -> Int
bmNumRows = bmRows
{-# INLINE bmNumRows #-}

-- | Number of columns.
bmNumCols :: BinMatrix -> Int
bmNumCols = bmCols
{-# INLINE bmNumCols #-}

-- | @m x n@ zero matrix.
bmZero :: Int -> Int -> BinMatrix
bmZero m n = BinMatrix m n wpr (VU.replicate (m * wpr) 0)
  where wpr = wordsNeeded n

-- | @n x n@ identity matrix.
bmIdentity :: Int -> BinMatrix
bmIdentity n = BinMatrix n n wpr dat
  where
    wpr = wordsNeeded n
    dat = VU.create $ do
      v <- VUM.replicate (n * wpr) 0
      let go i
            | i >= n    = return ()
            | otherwise = do
                let (wi, bi) = i `quotRem` 64
                    off = i * wpr + wi
                VUM.write v off (setBit 0 bi)
                go (i + 1)
      go 0
      return v

-- | Build from a list of 'BinVec' rows. All rows must have the same length.
bmFromRows :: [BinVec] -> BinMatrix
bmFromRows [] = bmZero 0 0
bmFromRows rows@(r:_) = BinMatrix m n wpr dat
  where
    m   = length rows
    n   = bvLength r
    wpr = wordsNeeded n
    dat = VU.concat [ padRow (bvWords rv) | rv <- rows ]
    padRow ws
      | VU.length ws == wpr = ws
      | otherwise           = ws VU.++ VU.replicate (wpr - VU.length ws) 0

-- | Build from nested list (list of rows, each a list of 'GF2').
bmFromList :: [[GF2]] -> BinMatrix
bmFromList [] = bmZero 0 0
bmFromList xss = bmFromRows (map bvFromList xss)

------------------------------------------------------------------------
-- Row access
------------------------------------------------------------------------

-- | Get row @i@ as a 'BinVec'.
bmGetRow :: BinMatrix -> Int -> BinVec
bmGetRow (BinMatrix _ n wpr dat) i =
  BinVec n (VU.slice (i * wpr) wpr dat)
{-# INLINE bmGetRow #-}

-- | Set row @i@ to a 'BinVec'.
bmSetRow :: Int -> BinVec -> BinMatrix -> BinMatrix
bmSetRow i v (BinMatrix m n wpr dat) = BinMatrix m n wpr dat'
  where
    off  = i * wpr
    ws   = bvWords v
    dat' = VU.create $ do
      mv <- VU.thaw dat
      let go j
            | j >= wpr  = return ()
            | otherwise = do
                VUM.write mv (off + j) (ws VU.! j)
                go (j + 1)
      go 0
      return mv

-- | Get entry at @(i, j)@.
bmGetEntry :: BinMatrix -> Int -> Int -> GF2
bmGetEntry (BinMatrix _ _ wpr dat) i j =
  let (wi, bi) = j `quotRem` 64
  in GF2 (testBit (dat VU.! (i * wpr + wi)) bi)
{-# INLINE bmGetEntry #-}

------------------------------------------------------------------------
-- Row operations (hot path for Gaussian elimination)
------------------------------------------------------------------------

-- | XOR row @src@ into row @dst@: @row[dst] ^= row[src]@.
bmAddRows :: Int -> Int -> BinMatrix -> BinMatrix
bmAddRows dst src (BinMatrix m n wpr dat) = BinMatrix m n wpr dat'
  where
    dOff = dst * wpr
    sOff = src * wpr
    dat' = VU.create $ do
      mv <- VU.thaw dat
      let go j
            | j >= wpr  = return ()
            | otherwise = do
                dv <- VUM.read mv (dOff + j)
                let sv = dat VU.! (sOff + j)
                VUM.write mv (dOff + j) (xor dv sv)
                go (j + 1)
      go 0
      return mv
{-# INLINE bmAddRows #-}

-- | Swap rows @i@ and @j@.
bmSwapRows :: Int -> Int -> BinMatrix -> BinMatrix
bmSwapRows i j mat
  | i == j    = mat
  | otherwise = BinMatrix m n wpr dat'
  where
    BinMatrix m n wpr dat = mat
    iOff = i * wpr
    jOff = j * wpr
    dat' = VU.create $ do
      mv <- VU.thaw dat
      let go k
            | k >= wpr  = return ()
            | otherwise = do
                iv <- VUM.read mv (iOff + k)
                jv <- VUM.read mv (jOff + k)
                VUM.write mv (iOff + k) jv
                VUM.write mv (jOff + k) iv
                go (k + 1)
      go 0
      return mv

------------------------------------------------------------------------
-- Transpose
------------------------------------------------------------------------

-- | Transpose a matrix.
bmTranspose :: BinMatrix -> BinMatrix
bmTranspose (BinMatrix m n wpr dat) = BinMatrix n m wpr' dat'
  where
    wpr' = wordsNeeded m
    dat' = VU.create $ do
      mv <- VUM.replicate (n * wpr') 0
      let go i
            | i >= m    = return ()
            | otherwise = do
                let goJ j
                      | j >= n    = return ()
                      | otherwise = do
                          let (swi, sbi) = j `quotRem` 64
                              srcBit = testBit (dat VU.! (i * wpr + swi)) sbi
                          if srcBit
                            then do
                              let (dwi, dbi) = i `quotRem` 64
                                  dstIdx = j * wpr' + dwi
                              old <- VUM.read mv dstIdx
                              VUM.write mv dstIdx (setBit old dbi)
                            else return ()
                          goJ (j + 1)
                goJ 0
                go (i + 1)
      go 0
      return mv

------------------------------------------------------------------------
-- Multiplication
------------------------------------------------------------------------

-- | Matrix-vector multiply: @A (m x n) * v (n) -> w (m)@.
bmMulVec :: BinMatrix -> BinVec -> BinVec
bmMulVec (BinMatrix m n wpr dat) v
  | bvLength v /= n = error "bmMulVec: dimension mismatch"
  | otherwise = BinVec m resultWords
  where
    vws = bvWords v
    rwpr = wordsNeeded m
    resultWords = VU.create $ do
      mv <- VUM.replicate rwpr 0
      let go i
            | i >= m    = return ()
            | otherwise = do
                -- Dot product of row i with v (over GF(2))
                let rowOff = i * wpr
                    dot = goW 0 0
                    goW k !acc
                      | k >= wpr  = acc
                      | otherwise = goW (k + 1)
                          (acc + popCount (dat VU.! (rowOff + k) .&. vws VU.! k))
                if odd dot
                  then do
                    let (wi, bi) = i `quotRem` 64
                    old <- VUM.read mv wi
                    VUM.write mv wi (setBit old bi)
                  else return ()
                go (i + 1)
      go 0
      return mv

-- | Matrix multiplication: @A (m x k) * B (k x n) -> C (m x n)@.
-- Transposes B first for cache-friendly row-dot-product.
bmMul :: BinMatrix -> BinMatrix -> BinMatrix
bmMul a b
  | bmCols a /= bmRows b = error "bmMul: dimension mismatch"
  | otherwise = BinMatrix m n wprC datC
  where
    m    = bmRows a
    n    = bmCols b
    wprA = bmWordsPerRow a
    wprC = wordsNeeded n
    -- Transpose B so rows of B^T are columns of B
    bt   = bmTranspose b
    wprBT = bmWordsPerRow bt
    datC = VU.create $ do
      mv <- VUM.replicate (m * wprC) 0
      let goI i
            | i >= m    = return ()
            | otherwise = do
                let goJ j
                      | j >= n    = return ()
                      | otherwise = do
                          -- Dot product of row i of A with row j of B^T
                          let aOff  = i * wprA
                              btOff = j * wprBT
                              minW  = min wprA wprBT
                              dot   = goW 0 0
                              goW w !acc
                                | w >= minW = acc
                                | otherwise = goW (w + 1)
                                    (acc + popCount (bmData a VU.! (aOff + w)
                                                     .&. bmData bt VU.! (btOff + w)))
                          if odd dot
                            then do
                              let (wi, bi) = j `quotRem` 64
                              old <- VUM.read mv (i * wprC + wi)
                              VUM.write mv (i * wprC + wi) (setBit old bi)
                            else return ()
                          goJ (j + 1)
                goJ 0
                goI (i + 1)
      goI 0
      return mv
