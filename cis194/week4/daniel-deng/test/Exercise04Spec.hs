module CIS194.Homework04.Exercise04Spec where

import CIS194.Homework04.Exercise04

import Text.Printf

import Test.Tasty.Hspec

t :: Integer -> [Integer] -> Spec
t n expectedPrimeNumbers =
  it (printf "returns %s for the input %s" (show expectedPrimeNumbers) (show n)) $
    sieveSundaram n `shouldBe` expectedPrimeNumbers

spec_sieveSundaram :: Spec
spec_sieveSundaram = do

  t 0 []
  t 1 [3]
  t 2 [3, 5]
  t 3 [3,5,7]
  t 4 [3,5,7]
  t 5 [3,5,7,11]
  t 6 [3,5,7,11,13]
  t 7 [3,5,7,11,13]
  t 8 [3,5,7,11,13,17]
  t 9 [3,5,7,11,13,17,19]
  t 10 [3,5,7,11,13,17,19]
