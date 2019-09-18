module CIS194.Homework05.Exercise01Spec where

import CIS194.Homework05.ExprT
import CIS194.Homework05.Exercise01

import Test.Tasty.Hspec

spec_eval :: Spec
spec_eval =

  it "evaluates the value correctly" $
    eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20
