module CIS194.Homework05.Exercise06Spec where

import CIS194.Homework05.Exercise03 (Expr(..))
import CIS194.Homework05.Exercise06 (HasVars(..), withVars)

import Test.Tasty.Hspec

spec_Integer :: Spec
spec_Integer =

  it "returns the correct result" $ do
    withVars [("x", 3)] (lit 5) `shouldBe` Just 5
    withVars [("x", 3)] (var "x") `shouldBe` Just 3
    withVars [("x", 3)] (var "y") `shouldBe` Nothing
    withVars [("x", 3)] (add (var "x") (lit 15)) `shouldBe` Just 18
    withVars [("x", 3), ("y", 5)] (add (var "x") (var "y")) `shouldBe` Just 8
    withVars [("x", 3), ("y", 5)] (mul (var "x") (var "y")) `shouldBe` Just 15
