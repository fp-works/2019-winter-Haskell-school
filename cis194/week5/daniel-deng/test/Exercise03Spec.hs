module CIS194.Homework05.Exercise03Spec where

import CIS194.Homework05.ExprT
import CIS194.Homework05.Exercise03

import Test.Tasty.Hspec

spec_ExprT :: Spec
spec_ExprT =

  it "is an instance of Expr" $
    reify (mul (add (lit 2) (lit 3)) (lit 4)) `shouldBe` Mul (Add (Lit 2) (Lit 3)) (Lit 4)
