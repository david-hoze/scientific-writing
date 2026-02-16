module QEC.GF2.Gauss.Test (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck (Arbitrary(..), Gen, choose, vectorOf)

import QEC.GF2
import QEC.GF2.Matrix
import QEC.GF2.Gauss

------------------------------------------------------------------------
-- Generators
------------------------------------------------------------------------

instance Arbitrary GF2 where
  arbitrary = GF2 <$> arbitrary

genBinVec :: Int -> Gen BinVec
genBinVec n = bvFromList <$> vectorOf n arbitrary

genMatrix :: Int -> Int -> Gen BinMatrix
genMatrix m n = bmFromRows <$> vectorOf m (genBinVec n)

data SmallMatrix = SmallMatrix BinMatrix deriving (Show)

instance Arbitrary SmallMatrix where
  arbitrary = do
    m <- choose (1, 30)
    n <- choose (1, 30)
    SmallMatrix <$> genMatrix m n

newtype SmallSquare = SmallSquare BinMatrix deriving (Show)

instance Arbitrary SmallSquare where
  arbitrary = do
    n <- choose (1, 30)
    SmallSquare <$> genMatrix n n

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "QEC.GF2.Gauss"
  [ rrefTests
  , rankTests
  , kernelTests
  , solveTests
  ]

rrefTests :: TestTree
rrefTests = testGroup "RREF"
  [ testProperty "idempotence" $ \(SmallMatrix m) ->
      let (r1, _) = rref m
          (r2, _) = rref r1
      in r1 == r2

  , testCase "identity is already RREF" $ do
      let (r, pivs) = rref (bmIdentity 4)
      r @?= bmIdentity 4
      pivs @?= [0, 1, 2, 3]

  , testCase "zero matrix RREF" $ do
      let (r, pivs) = rref (bmZero 3 4)
      r @?= bmZero 3 4
      pivs @?= []

  , testCase "concrete 3x3" $ do
      -- [1 1 0]     [1 0 1]
      -- [0 1 1]  -> [0 1 1]  (already RREF-ish, but let's verify)
      -- [1 0 1]     [0 0 0]  (row3 = row1 XOR row2)
      let m = bmFromList [ [GF2 True, GF2 True, GF2 False]
                         , [GF2 False, GF2 True, GF2 True]
                         , [GF2 True, GF2 False, GF2 True] ]
          (r, pivs) = rref m
      pivs @?= [0, 1]
      rank m @?= 2
      -- Row 3 should be zero
      bmGetRow r 2 @?= bvZero 3

  , testProperty "pivot columns are sorted and within bounds" $ \(SmallMatrix m) ->
      let (_, pivs) = rref m
      in pivs == sort' pivs
         && all (\p -> p >= 0 && p < bmNumCols m) pivs
  ]
  where
    sort' [] = []
    sort' [x] = [x]
    sort' (x:y:rest) = if x <= y then x : sort' (y:rest) else []  -- just check sorted

rankTests :: TestTree
rankTests = testGroup "Rank"
  [ testCase "rank(I_n) = n" $ do
      rank (bmIdentity 1) @?= 1
      rank (bmIdentity 5) @?= 5
      rank (bmIdentity 10) @?= 10

  , testCase "rank(0) = 0" $ do
      rank (bmZero 3 4) @?= 0
      rank (bmZero 1 1) @?= 0

  , testProperty "rank <= min(m, n)" $ \(SmallMatrix m) ->
      rank m <= min (bmNumRows m) (bmNumCols m)

  , testProperty "rank(A) = rank(A^T)" $ \(SmallMatrix m) ->
      rank m == rank (bmTranspose m)
  ]

kernelTests :: TestTree
kernelTests = testGroup "Kernel"
  [ testProperty "rank-nullity theorem" $ \(SmallMatrix m) ->
      rank m + length (kernel m) == bmNumCols m

  , testProperty "kernel vectors are in null space" $ \(SmallMatrix m) ->
      let ks = kernel m
          rows = bmNumRows m
      in all (\v -> bmMulVec m v == bvZero rows) ks

  , testCase "kernel of identity is empty" $
      kernel (bmIdentity 5) @?= []

  , testCase "kernel of zero matrix has n vectors" $ do
      let ks = kernel (bmZero 3 4)
      length ks @?= 4
      -- Each kernel vector should be in the null space (trivially)
      all (\v -> bmMulVec (bmZero 3 4) v == bvZero 3) ks @?= True

  , testCase "kernel of rank-deficient matrix" $ do
      -- [1 1 0]
      -- [0 1 1]
      -- [1 0 1]  <- row1 XOR row2, so rank=2, nullity=1
      let m = bmFromList [ [GF2 True, GF2 True, GF2 False]
                         , [GF2 False, GF2 True, GF2 True]
                         , [GF2 True, GF2 False, GF2 True] ]
          ks = kernel m
      length ks @?= 1
      -- The kernel vector times A should be zero
      all (\v -> bmMulVec m v == bvZero 3) ks @?= True

  , testProperty "kernel vectors are linearly independent" $ \(SmallMatrix m) ->
      let ks = kernel m
      in case ks of
           [] -> True
           _  -> let km = bmFromRows ks
                 in rank km == length ks
  ]

solveTests :: TestTree
solveTests = testGroup "solveLinear"
  [ testCase "identity system" $ do
      let b = bvFromList [GF2 True, GF2 False, GF2 True]
      case solveLinear (bmIdentity 3) b of
        Nothing -> assertFailure "Should be solvable"
        Just x  -> x @?= b

  , testCase "inconsistent system" $ do
      -- [1 0]     [1]
      -- [1 0] x = [0]  -- inconsistent
      let m = bmFromList [ [GF2 True, GF2 False]
                         , [GF2 True, GF2 False] ]
          b = bvFromList [GF2 True, GF2 False]
      solveLinear m b @?= Nothing

  , testProperty "solution satisfies Ax = b" $ \(SmallMatrix m) ->
      forAll (genBinVec (bmNumCols m)) $ \x ->
        let b = bmMulVec m x
        in case solveLinear m b of
             Nothing -> False  -- Should always be solvable since b = Ax
             Just x' -> bmMulVec m x' == b

  , testCase "underdetermined system" $ do
      -- [1 1 0] x = [1]
      let m = bmFromList [ [GF2 True, GF2 True, GF2 False] ]
          b = bvFromList [GF2 True]
      case solveLinear m b of
        Nothing -> assertFailure "Should be solvable"
        Just x  -> bmMulVec m x @?= b
  ]
