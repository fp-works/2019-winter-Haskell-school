module CIS194.Homework01.Exercise01Spec where

import CIS194.Homework01.Exercise01

import Test.Tasty.Hspec

spec_toDigits :: Spec
spec_toDigits = do

  it "returns an expected list for a negative integer" $
    toDigits (-123) `shouldBe` [1..3]

  it "returns [0] for 0" $
    toDigits 0 `shouldBe` [0]

  it "returns an expected list for a positive integer" $
    toDigits 12345 `shouldBe` [1..5]

spec_toDigitsRev :: Spec
spec_toDigitsRev = do

  it "returns the expected list for a negative integer" $
    toDigitsRev (-123) `shouldBe` [3, 2, 1]

  it "returns [0] for 0" $
    toDigitsRev 0 `shouldBe` [0]

  it "returns the expected list for a positive integer" $
    toDigitsRev 378921 `shouldBe` [1, 2, 9, 8, 7, 3]
