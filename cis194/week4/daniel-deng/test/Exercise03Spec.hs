module CIS194.Homework04.Exercise03Spec where

import CIS194.Homework04.Exercise03

import Test.Tasty.Hspec

spec_xor :: Spec
spec_xor = do

  it "returns False for an empty list" $
    xor ([] :: [Bool]) `shouldBe` False

  it "returns True for a list with odd number of `True`s" $
    xor [True, False, True, False, True] `shouldBe` True

  it "returns False for a list with even number of `True`s" $
    xor [True, False, True, False] `shouldBe` False

  it "number of `False`s does not affect the returned result" $
    xor [True, False, False] `shouldBe` xor [True, False, False, False]

spec_map' :: Spec
spec_map' = do

  it "returns an empty list for an empty list" $
    map' (+1) ([] :: [Int]) `shouldBe` ([] :: [Int])

  it "returns the expected result for a list of `Int`s" $
    map' (*2) ([1, 3, 5] :: [Int]) `shouldBe` [2, 6, 10]

  it "returns the expected result for a list of `String`s" $
    map' ("Hello "++) ["Alice", "Bob", "World"] `shouldBe` ["Hello Alice", "Hello Bob", "Hello World"]

spec_myFoldL :: Spec
spec_myFoldL = do

  it "returns the init value for an empty list" $
    myFoldl (+) 100 ([] :: [Int]) `shouldBe` 100

  it "returns the expected result for a list of `Int`s" $
    myFoldl (+) 0 ([1..10] :: [Int]) `shouldBe` 55

  it "returns the expected result for a list of `String`s" $
    myFoldl (\x y -> x ++ " " ++ y) "Hello" ["Haskell", "Curry"] `shouldBe` "Hello Haskell Curry"
