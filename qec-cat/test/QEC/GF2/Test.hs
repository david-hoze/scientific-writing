module QEC.GF2.Test (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck (Arbitrary(..), Gen, choose, vectorOf)

import QEC.GF2

------------------------------------------------------------------------
-- Arbitrary instances
------------------------------------------------------------------------

instance Arbitrary GF2 where
  arbitrary = GF2 <$> arbitrary

-- | Generate a BinVec of a given length.
genBinVec :: Int -> Gen BinVec
genBinVec n = bvFromList <$> vectorOf n arbitrary

-- | Wrapper with a fixed reasonable size for property testing.
newtype SmallBinVec = SmallBinVec BinVec
  deriving (Show)

instance Arbitrary SmallBinVec where
  arbitrary = do
    n <- choose (1, 256)
    SmallBinVec <$> genBinVec n

-- | Pair of BinVecs with matching length.
data BinVecPair = BinVecPair BinVec BinVec
  deriving (Show)

instance Arbitrary BinVecPair where
  arbitrary = do
    n <- choose (1, 256)
    BinVecPair <$> genBinVec n <*> genBinVec n

-- | Triple of BinVecs with matching length.
data BinVecTriple = BinVecTriple BinVec BinVec BinVec
  deriving (Show)

instance Arbitrary BinVecTriple where
  arbitrary = do
    n <- choose (1, 256)
    BinVecTriple <$> genBinVec n <*> genBinVec n <*> genBinVec n

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "QEC.GF2"
  [ gf2Tests
  , binVecTests
  ]

gf2Tests :: TestTree
gf2Tests = testGroup "GF2 arithmetic"
  [ testCase "0 + 0 = 0" $ GF2 False + GF2 False @?= GF2 False
  , testCase "0 + 1 = 1" $ GF2 False + GF2 True  @?= GF2 True
  , testCase "1 + 0 = 1" $ GF2 True  + GF2 False @?= GF2 True
  , testCase "1 + 1 = 0" $ GF2 True  + GF2 True  @?= GF2 False
  , testCase "0 * 0 = 0" $ GF2 False * GF2 False @?= GF2 False
  , testCase "0 * 1 = 0" $ GF2 False * GF2 True  @?= GF2 False
  , testCase "1 * 0 = 0" $ GF2 True  * GF2 False @?= GF2 False
  , testCase "1 * 1 = 1" $ GF2 True  * GF2 True  @?= GF2 True
  , testCase "fromInteger 0" $ fromInteger 0 @?= GF2 False
  , testCase "fromInteger 1" $ fromInteger 1 @?= GF2 True
  , testCase "fromInteger 2" $ fromInteger 2 @?= GF2 False
  , testCase "negate x = x" $ negate (GF2 True) @?= GF2 True
  , testProperty "x + x = 0" $ \(x :: GF2) -> x + x == GF2 False
  ]

binVecTests :: TestTree
binVecTests = testGroup "BinVec"
  [ testGroup "Construction / roundtrip"
    [ testCase "bvZero has all zeros" $
        bvToList (bvZero 5) @?= replicate 5 (GF2 False)

    , testProperty "bvToList . bvFromList = id" $ \xs ->
        let v = bvFromList (xs :: [GF2])
        in bvToList v == xs

    , testProperty "bvLength . bvFromList = length" $ \xs ->
        bvLength (bvFromList (xs :: [GF2])) == length xs

    , testCase "empty vector" $
        bvToList (bvFromList []) @?= []
    ]

  , testGroup "bvXor"
    [ testProperty "a XOR a = zero" $ \(SmallBinVec a) ->
        bvXor a a == bvZero (bvLength a)

    , testProperty "a XOR zero = a" $ \(SmallBinVec a) ->
        bvXor a (bvZero (bvLength a)) == a

    , testProperty "commutativity" $ \(BinVecPair a b) ->
        bvXor a b == bvXor b a

    , testProperty "associativity" $ \(BinVecTriple a b c) ->
        bvXor (bvXor a b) c == bvXor a (bvXor b c)
    ]

  , testGroup "bvAnd"
    [ testProperty "a AND zero = zero" $ \(SmallBinVec a) ->
        let n = bvLength a in bvAnd a (bvZero n) == bvZero n

    , testProperty "commutativity" $ \(BinVecPair a b) ->
        bvAnd a b == bvAnd b a
    ]

  , testGroup "bvWeight"
    [ testProperty "weight of zero = 0" $ \n ->
        let n' = abs n `mod` 300 + 1
        in bvWeight (bvZero n') == 0

    , testProperty "weight >= 0" $ \(SmallBinVec a) ->
        bvWeight a >= 0

    , testCase "weight of all-ones length 3" $
        bvWeight (bvFromList [GF2 True, GF2 True, GF2 True]) @?= 3
    ]

  , testGroup "bvInnerGF2"
    [ testProperty "inner(a, a) = GF2 (odd (weight a))" $ \(SmallBinVec a) ->
        bvInnerGF2 a a == GF2 (odd (bvWeight a))

    , testProperty "commutativity" $ \(BinVecPair a b) ->
        bvInnerGF2 a b == bvInnerGF2 b a

    , testProperty "inner(a, zero) = 0" $ \(SmallBinVec a) ->
        bvInnerGF2 a (bvZero (bvLength a)) == GF2 False
    ]

  , testGroup "bit operations"
    [ testProperty "getBit after setBit" $ \(SmallBinVec v) ->
        let n = bvLength v
        in forAll (choose (0, n - 1)) $ \i ->
             bvGetBit (bvSetBit i v) i == GF2 True

    , testProperty "getBit after clearBit" $ \(SmallBinVec v) ->
        let n = bvLength v
        in forAll (choose (0, n - 1)) $ \i ->
             bvGetBit (bvClearBit i v) i == GF2 False

    , testProperty "flipBit twice = identity" $ \(SmallBinVec v) ->
        let n = bvLength v
        in forAll (choose (0, n - 1)) $ \i ->
             bvFlipBit i (bvFlipBit i v) == v
    ]

  , testGroup "cleanup invariant"
    [ testProperty "bvCleanup is idempotent" $ \(SmallBinVec v) ->
        bvCleanup (bvCleanup v) == bvCleanup v

    , testProperty "bvFromList produces clean vectors" $ \xs ->
        let v = bvFromList (xs :: [GF2])
        in bvCleanup v == v
    ]

  , testGroup "boundary cases"
    [ testCase "length 64 (exact word boundary)" $ do
        let xs = replicate 64 (GF2 True)
            v = bvFromList xs
        bvLength v @?= 64
        bvWeight v @?= 64
        bvToList v @?= xs

    , testCase "length 65 (one past word boundary)" $ do
        let xs = replicate 65 (GF2 True)
            v = bvFromList xs
        bvLength v @?= 65
        bvWeight v @?= 65
        bvToList v @?= xs

    , testCase "length 128 (two full words)" $ do
        let xs = replicate 128 (GF2 True)
            v = bvFromList xs
        bvWeight v @?= 128
    ]
  ]
