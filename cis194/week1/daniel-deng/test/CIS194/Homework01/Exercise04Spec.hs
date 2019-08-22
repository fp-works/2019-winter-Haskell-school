module CIS194.Homework01.Exercise04Spec where

import CIS194.Homework01.Exercise04

import Test.Tasty.Hspec

spec_validate :: Spec
spec_validate = do

  it "returns False for a negative integer" $
    validate (-1) `shouldBe` False

  it "returns False for 0" $
    validate 0 `shouldBe` False

  it "returns False for an integer of less than 16 digits" $
    validate 42 `shouldBe` False

  it "returns False for an integer of more than 16 digits" $
    validate 42342948597593053 `shouldBe` False

  it "returns False for card number 4012888888881882" $
    validate 4012888888881882 `shouldBe` False

  it "returns True for card number 4012888888881881" $
    validate 4012888888881881 `shouldBe` True
