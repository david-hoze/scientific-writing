module QEC.Symplectic.Test (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck (Arbitrary(..), Gen, choose, vectorOf, elements)

import QEC.GF2
import QEC.Symplectic

------------------------------------------------------------------------
-- Generators
------------------------------------------------------------------------

instance Arbitrary PauliOp where
  arbitrary = elements [I, X, Y, Z]

newtype SmallPauli = SmallPauli PauliString deriving (Show)

instance Arbitrary SmallPauli where
  arbitrary = do
    n <- choose (1, 32)
    SmallPauli . pauliFromList <$> vectorOf n arbitrary

data PauliPair = PauliPair PauliString PauliString deriving (Show)

instance Arbitrary PauliPair where
  arbitrary = do
    n <- choose (1, 32)
    PauliPair <$> (pauliFromList <$> vectorOf n arbitrary)
              <*> (pauliFromList <$> vectorOf n arbitrary)

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "QEC.Symplectic"
  [ constructionTests
  , commutativityTests
  , multiplicationTests
  ]

constructionTests :: TestTree
constructionTests = testGroup "Construction"
  [ testProperty "pauliToList . pauliFromList = id" $ \ops ->
      pauliToList (pauliFromList (ops :: [PauliOp])) == ops

  , testCase "single X" $
      pauliToList (pauliFromList [X]) @?= [X]

  , testCase "single Z" $
      pauliToList (pauliFromList [Z]) @?= [Z]

  , testCase "IXYZ" $
      pauliToList (pauliFromList [I, X, Y, Z]) @?= [I, X, Y, Z]

  , testProperty "pauliNumQubits" $ \ops ->
      pauliNumQubits (pauliFromList (ops :: [PauliOp])) == length ops
  ]

commutativityTests :: TestTree
commutativityTests = testGroup "Commutativity"
  [ testCase "X,Z anticommute on same qubit" $
      pauliCommutes (pauliFromList [X]) (pauliFromList [Z]) @?= False

  , testCase "X,Y anticommute" $
      pauliCommutes (pauliFromList [X]) (pauliFromList [Y]) @?= False

  , testCase "Y,Z anticommute" $
      pauliCommutes (pauliFromList [Y]) (pauliFromList [Z]) @?= False

  , testCase "X,X commute" $
      pauliCommutes (pauliFromList [X]) (pauliFromList [X]) @?= True

  , testCase "Z,Z commute" $
      pauliCommutes (pauliFromList [Z]) (pauliFromList [Z]) @?= True

  , testCase "different-qubit Paulis commute" $
      pauliCommutes (pauliFromList [X, I]) (pauliFromList [I, Z]) @?= True

  , testCase "XI, IX commute" $
      pauliCommutes (pauliFromList [X, I]) (pauliFromList [I, X]) @?= True

  , testCase "XX, ZZ commute (both anticommute per qubit)" $
      pauliCommutes (pauliFromList [X, X]) (pauliFromList [Z, Z]) @?= True

  , testProperty "any Pauli commutes with itself" $ \(SmallPauli p) ->
      pauliCommutes p p

  , testProperty "commutativity is symmetric" $ \(PauliPair p1 p2) ->
      pauliCommutes p1 p2 == pauliCommutes p2 p1

  , testProperty "any Pauli commutes with identity" $ \(SmallPauli p) ->
      let n = pauliNumQubits p
          ident = pauliFromList (replicate n I)
      in pauliCommutes p ident
  ]

multiplicationTests :: TestTree
multiplicationTests = testGroup "Multiplication"
  [ testCase "X * Y = iZ" $ do
      let r = pauliMul (pauliFromList [X]) (pauliFromList [Y])
      pauliToList r @?= [Z]
      psPhase r @?= 1

  , testCase "Y * X = -iZ" $ do
      let r = pauliMul (pauliFromList [Y]) (pauliFromList [X])
      pauliToList r @?= [Z]
      psPhase r @?= 3

  , testCase "X * X = I" $ do
      let r = pauliMul (pauliFromList [X]) (pauliFromList [X])
      pauliToList r @?= [I]
      psPhase r @?= 0

  , testCase "Z * X = iY" $ do
      let r = pauliMul (pauliFromList [Z]) (pauliFromList [X])
      pauliToList r @?= [Y]
      psPhase r @?= 1

  , testCase "X * Z = -iY" $ do
      let r = pauliMul (pauliFromList [X]) (pauliFromList [Z])
      pauliToList r @?= [Y]
      psPhase r @?= 3

  , testProperty "P * I = P (same operators)" $ \(SmallPauli p) ->
      let n = pauliNumQubits p
          ident = pauliFromList (replicate n I)
          r = pauliMul p ident
      in pauliToList r == pauliToList p

  , testProperty "P * P has all I operators" $ \(SmallPauli p) ->
      let r = pauliMul p p
      in all (== I) (pauliToList r)
  ]
