module Specs (specs) where

import           Test.Hspec

import           Exercise4
import           Exercise5
import           StackVM

specs :: IO ()
specs = hspec $ do
  describe "Exercise 4" $ do
    it "performs integer arithmetic" $ do
      testInteger `shouldBe` Just (-7)
    it "performs boolean arithmetic" $ do
      testBool `shouldBe` Just True
    it "performs MinMax arithmetic" $ do
      testMM `shouldBe` Just (MinMax 5)
    it "performs Mod7 arithmetic" $ do
      testSat `shouldBe` Just (Mod7 0)

  describe "Exericse 5" $ do
    it "compiles and executes program" $ do
      let result = case compile "2 + 4 * 10" of
                    Just prog -> stackVM prog
          value  = case result of
                    Right (IVal n) -> n
      value `shouldBe` 42
