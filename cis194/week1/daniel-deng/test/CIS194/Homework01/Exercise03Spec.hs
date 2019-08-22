module CIS194.Homework01.Exercise03Spec where

import CIS194.Homework01.Exercise03

import Test.Tasty.Hspec

spec_sumDigits :: Spec
spec_sumDigits = do

  it "returns 0 for an empty string" $
    sumDigits [] `shouldBe` 0

  it "returns the element value for a single element list" $
    sumDigits [1] `shouldBe` 1

  it "returns the expected result for a list of single digit numbers" $
    sumDigits [1, 2, 3, 4] `shouldBe` 10

  it "returns the expected result for a list of multi digit numbers" $
    sumDigits [16,7,12,5] `shouldBe` 22 --  1 + 6 + 7 + 1 + 2 + 5
