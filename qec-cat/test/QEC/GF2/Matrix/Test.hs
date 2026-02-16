module QEC.GF2.Matrix.Test (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck (Arbitrary(..), Gen, choose, vectorOf)

import QEC.GF2
import QEC.GF2.Matrix

------------------------------------------------------------------------
-- Generators
------------------------------------------------------------------------

-- | Generate a BinVec of a given length.
genBinVec :: Int -> Gen BinVec
genBinVec n = bvFromList <$> vectorOf n arbitrary

instance Arbitrary GF2 where
  arbitrary = GF2 <$> arbitrary

-- | Generate a BinMatrix with given dimensions.
genMatrix :: Int -> Int -> Gen BinMatrix
genMatrix m n = bmFromRows <$> vectorOf m (genBinVec n)

-- | Small square matrix for property testing.
newtype SmallSquare = SmallSquare BinMatrix deriving (Show)

instance Arbitrary SmallSquare where
  arbitrary = do
    n <- choose (1, 32)
    SmallSquare <$> genMatrix n n

-- | Small rectangular matrix.
data SmallMatrix = SmallMatrix BinMatrix deriving (Show)

instance Arbitrary SmallMatrix where
  arbitrary = do
    m <- choose (1, 32)
    n <- choose (1, 32)
    SmallMatrix <$> genMatrix m n

-- | Matrix with a matching vector for multiplication.
data MatVecPair = MatVecPair BinMatrix BinVec deriving (Show)

instance Arbitrary MatVecPair where
  arbitrary = do
    m <- choose (1, 32)
    n <- choose (1, 32)
    MatVecPair <$> genMatrix m n <*> genBinVec n

-- | Compatible matrix pair for multiplication.
data MatMulPair = MatMulPair BinMatrix BinMatrix deriving (Show)

instance Arbitrary MatMulPair where
  arbitrary = do
    m <- choose (1, 20)
    k <- choose (1, 20)
    n <- choose (1, 20)
    MatMulPair <$> genMatrix m k <*> genMatrix k n

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "QEC.GF2.Matrix"
  [ constructionTests
  , rowOpTests
  , transposeTests
  , mulTests
  ]

constructionTests :: TestTree
constructionTests = testGroup "Construction"
  [ testCase "bmZero is all zeros" $ do
      let z = bmZero 3 4
      bmNumRows z @?= 3
      bmNumCols z @?= 4
      sequence_ [ bmGetEntry z i j @?= GF2 False
                | i <- [0..2], j <- [0..3] ]

  , testCase "bmIdentity diagonal" $ do
      let eye = bmIdentity 4
      bmNumRows eye @?= 4
      bmNumCols eye @?= 4
      sequence_ [ bmGetEntry eye i j @?= GF2 (i == j)
                | i <- [0..3], j <- [0..3] ]

  , testCase "bmIdentity size 1" $ do
      let eye = bmIdentity 1
      bmGetEntry eye 0 0 @?= GF2 True

  , testCase "bmIdentity size 65 (crosses word boundary)" $ do
      let eye = bmIdentity 65
      sequence_ [ bmGetEntry eye i j @?= GF2 (i == j)
                | i <- [0, 32, 63, 64], j <- [0, 32, 63, 64] ]

  , testCase "bmFromList roundtrip" $ do
      let rows = [[GF2 True, GF2 False], [GF2 False, GF2 True]]
          m = bmFromList rows
      bmGetEntry m 0 0 @?= GF2 True
      bmGetEntry m 0 1 @?= GF2 False
      bmGetEntry m 1 0 @?= GF2 False
      bmGetEntry m 1 1 @?= GF2 True

  , testProperty "bmGetRow roundtrip" $ \(SmallMatrix m) ->
      let rows = [ bmGetRow m i | i <- [0 .. bmNumRows m - 1] ]
      in bmFromRows rows == m
  ]

rowOpTests :: TestTree
rowOpTests = testGroup "Row operations"
  [ testCase "bmSetRow" $ do
      let m = bmZero 3 4
          v = bvFromList [GF2 True, GF2 True, GF2 False, GF2 True]
          m' = bmSetRow 1 v m
      bmGetEntry m' 1 0 @?= GF2 True
      bmGetEntry m' 1 1 @?= GF2 True
      bmGetEntry m' 1 2 @?= GF2 False
      bmGetEntry m' 1 3 @?= GF2 True
      -- Other rows unchanged
      bmGetRow m' 0 @?= bvZero 4
      bmGetRow m' 2 @?= bvZero 4

  , testCase "bmSwapRows" $ do
      let m = bmIdentity 3
          m' = bmSwapRows 0 2 m
      bmGetEntry m' 0 0 @?= GF2 False
      bmGetEntry m' 0 2 @?= GF2 True
      bmGetEntry m' 2 0 @?= GF2 True
      bmGetEntry m' 2 2 @?= GF2 False

  , testProperty "bmSwapRows is self-inverse" $ \(SmallSquare m) ->
      forAll (choose (0, bmNumRows m - 1)) $ \i ->
      forAll (choose (0, bmNumRows m - 1)) $ \j ->
        bmSwapRows i j (bmSwapRows i j m) == m

  , testCase "bmAddRows XOR" $ do
      let m = bmFromList [ [GF2 True, GF2 False, GF2 True]
                         , [GF2 True, GF2 True,  GF2 False] ]
          m' = bmAddRows 0 1 m  -- row0 ^= row1
      -- row 0 should be [0, 1, 1]
      bmGetEntry m' 0 0 @?= GF2 False
      bmGetEntry m' 0 1 @?= GF2 True
      bmGetEntry m' 0 2 @?= GF2 True
      -- row 1 unchanged
      bmGetEntry m' 1 0 @?= GF2 True
      bmGetEntry m' 1 1 @?= GF2 True
      bmGetEntry m' 1 2 @?= GF2 False

  , testProperty "bmAddRows row to itself = zero row" $ \(SmallSquare m) ->
      forAll (choose (0, bmNumRows m - 1)) $ \i ->
        let m' = bmAddRows i i m
        in bmGetRow m' i == bvZero (bmNumCols m)
  ]

transposeTests :: TestTree
transposeTests = testGroup "Transpose"
  [ testProperty "transpose involution" $ \(SmallMatrix m) ->
      bmTranspose (bmTranspose m) == m

  , testCase "transpose of identity = identity" $
      bmTranspose (bmIdentity 5) @?= bmIdentity 5

  , testCase "transpose dimensions" $ do
      let m = bmZero 3 7
          mt = bmTranspose m
      bmNumRows mt @?= 7
      bmNumCols mt @?= 3

  , testProperty "transpose preserves entries" $ \(SmallMatrix m) ->
      and [ bmGetEntry m i j == bmGetEntry (bmTranspose m) j i
          | i <- [0 .. bmNumRows m - 1]
          , j <- [0 .. bmNumCols m - 1] ]
  ]

mulTests :: TestTree
mulTests = testGroup "Multiplication"
  [ testProperty "A * I = A" $ \(SmallMatrix m) ->
      let n = bmNumCols m
      in bmMul m (bmIdentity n) == m

  , testProperty "I * A = A" $ \(SmallMatrix m) ->
      let r = bmNumRows m
      in bmMul (bmIdentity r) m == m

  , testProperty "bmMulVec (I n) v = v" $ \(MatVecPair _ v) ->
      let n = bvLength v
      in bmMulVec (bmIdentity n) v == v

  , testProperty "bmMulVec zero matrix = zero vector" $ \(MatVecPair m v) ->
      let z = bmZero (bmNumRows m) (bmNumCols m)
      in bmMulVec z v == bvZero (bmNumRows m)

  , testProperty "(AB)^T = B^T A^T" $ \(MatMulPair a b) ->
      bmTranspose (bmMul a b) == bmMul (bmTranspose b) (bmTranspose a)

  , testCase "concrete 2x2 multiplication" $ do
      -- [1 1] * [1 0] = [1 1]
      -- [0 1]   [0 1]   [0 1]
      let a = bmFromList [[GF2 True, GF2 True], [GF2 False, GF2 True]]
          b = bmFromList [[GF2 True, GF2 False], [GF2 False, GF2 True]]
          c = bmMul a b
      bmGetEntry c 0 0 @?= GF2 True
      bmGetEntry c 0 1 @?= GF2 True
      bmGetEntry c 1 0 @?= GF2 False
      bmGetEntry c 1 1 @?= GF2 True

  , testCase "concrete mulvec" $ do
      -- [1 0 1] * [1] = [1+1] = [0]
      -- [0 1 1]   [0]   [0+1]   [1]
      --           [1]
      let m = bmFromList [[GF2 True, GF2 False, GF2 True],
                          [GF2 False, GF2 True, GF2 True]]
          v = bvFromList [GF2 True, GF2 False, GF2 True]
          w = bmMulVec m v
      bvToList w @?= [GF2 False, GF2 True]
  ]
