module CIS194.Homework06.Exercise01Spec where

import CIS194.Homework06.Exercise01

import Test.Tasty.Hspec

spec_fib :: Spec
spec_fib =

  it "returns the nth fib number" $ do
    fib 0 `shouldBe` 0
    fib 1 `shouldBe` 1
    fib 2 `shouldBe` 1
    fib 3 `shouldBe` 2
    fib 9 `shouldBe` 34

spec_fibs1 :: Spec
spec_fibs1 =

  it "returns the correct fib series" $
    take 10 fibs1 `shouldBe` [0,1,1,2,3,5,8,13,21,34]
