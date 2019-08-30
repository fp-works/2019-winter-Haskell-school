module CIS194.Homework04.Exercise01Spec where

import CIS194.Homework04.Exercise01

import Test.Tasty.Hspec

spec_fun1 :: Spec
spec_fun1 = do

  it "returns 1 for an empty list" $
    fun1 [] `shouldBe` 1

  it "returns 0 for a list containing 2" $
    fun1 [2, 3, 4, 5] `shouldBe` 0

  it "returns the product for all even numbers subtracting 2" $
    fun1 [4, 5, 6, 7, 8] `shouldBe` (4 - 2) * (6 - 2) * (8 - 2)

spec_fun2 :: Spec
spec_fun2 = do

  it "returns 0 for input: -1" $
    fun2 (-1) `shouldBe` 0

  it "returns 0 for input: 0" $
    fun2 0 `shouldBe` 0

  it "returns 0 for input: 1" $
    fun2 1 `shouldBe` 0

  it "returns 2 for input: 2" $
    fun2 2 `shouldBe` 2

  it "returns the expected result for input: 80" $
    fun2 80 `shouldBe` 80 + 40 + 20 + 10 + 16 + 8 + 4 + 2
