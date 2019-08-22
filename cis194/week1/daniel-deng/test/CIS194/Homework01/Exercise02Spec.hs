module CIS194.Homework01.Exercise02Spec where

import CIS194.Homework01.Exercise02

import Test.Tasty.Hspec

spec_doubleEveryOther :: Spec
spec_doubleEveryOther = do

  it "returns an empty list for an empty string" $
    doubleEveryOther [] `shouldBe` []

  it "returns the same list itself for a single element list" $
    doubleEveryOther [1] `shouldBe` [1]

  it "returns the expected list for a multiple elements list" $ do
    doubleEveryOther [1, 2, 3, 4]    `shouldBe` [2, 2, 6, 4]
    doubleEveryOther [1, 2, 3, 4, 5] `shouldBe` [1, 4, 3, 8, 5]
