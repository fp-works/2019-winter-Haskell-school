module CIS194.Homework05.Exercise02Spec where

import CIS194.Homework05.Exercise02

import Test.Tasty.Hspec

spec_evalStr :: Spec
spec_evalStr =

  it "evaluates the value correctly" $ do
    evalStr "(2+3)*4" `shouldBe` Just 20
    evalStr "2+3*4" `shouldBe` Just 14
    evalStr "2+3*" `shouldBe` Nothing
    evalStr "" `shouldBe` Nothing
    evalStr "  " `shouldBe` Nothing
