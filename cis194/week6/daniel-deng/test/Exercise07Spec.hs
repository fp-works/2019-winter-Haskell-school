module CIS194.Homework06.Exercise07Spec where

import CIS194.Homework06.Exercise07

import Test.Tasty.Hspec

spec_fib4 :: Spec
spec_fib4 =

  it "returns the correct fib series" $ do
    fib4 <$> [0..10] `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
    fib4 30          `shouldBe` 832040
