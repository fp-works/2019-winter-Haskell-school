module Specs (specs) where

import           Test.Hspec

import           Exercise3

specs :: IO ()
specs = hspec $ do
  describe "Exercise 3" $
    it "xor - returns True if and only if there are an odd number of True" $ do
      xor [False, True, False] `shouldBe` True
      xor [False, True, False, False, True] `shouldBe` False
