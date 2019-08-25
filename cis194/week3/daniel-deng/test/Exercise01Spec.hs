module CIS194.Homework03.Exercise01Spec where

import CIS194.Homework03.Exercise01

import Text.Printf

import Test.Tasty.Hspec

t :: (Show a, Eq a) => [a] -> [[a]] -> Spec
t input expectedOutput =
  it (printf "converts %s to %s" (show input) (show expectedOutput)) $
    skips input `shouldBe` expectedOutput

spec_skips :: Spec
spec_skips = do

  t "ABCD" ["ABCD", "BD", "C", "D"]
  t "hello!" ["hello!", "el!", "l!", "l", "o", "!"]
  t [True, False] [[True,False], [False]]
  t ([1] :: [Int]) ([[1]] :: [[Int]])
  t ([] :: [Int]) ([] :: [[Int]])
