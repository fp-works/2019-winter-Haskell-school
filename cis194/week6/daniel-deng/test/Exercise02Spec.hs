module CIS194.Homework06.Exercise02Spec where

import CIS194.Homework06.Exercise02

import Test.Tasty.Hspec

spec_fibs2 :: Spec
spec_fibs2 =

  it "returns the correct fib series" $
    take 10 fibs2 `shouldBe` [0,1,1,2,3,5,8,13,21,34]
