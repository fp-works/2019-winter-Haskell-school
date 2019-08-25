module CIS194.Homework03.Exercise02Spec where

import CIS194.Homework03.Exercise02

import Text.Printf

import Test.Tasty.Hspec

t :: [Integer] -> [Integer] -> Spec
t input expectedOutput =
  it (printf "for %s should be %s" (show input) (show expectedOutput)) $
    localMaxima input `shouldBe` expectedOutput

spec_localMaxima :: Spec
spec_localMaxima = do

  t [2,9,5,6,1] [9,6]
  t [2,3,4,1,5] [4]
  t [1,2,3,4,5] []
  t [1,2]       []
  t [1]         []
  t []          []
