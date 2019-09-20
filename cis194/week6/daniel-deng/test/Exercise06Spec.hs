module CIS194.Homework06.Exercise06Spec where

import CIS194.Homework06.Exercise06

import Test.Tasty.Hspec

spec_fibs3 :: Spec
spec_fibs3 =

  it "returns the correct fib series" $
    take 10 fibs3 `shouldBe` [0,1,1,2,3,5,8,13,21,34]
