module CIS194.Homework05.Exercise04Spec where

import CIS194.Homework05.Exercise03 (Expr(..))
import CIS194.Homework05.Exercise04

import Test.Tasty.Hspec

spec_Integer :: Spec
spec_Integer =

  it "as an instance of Expr" $ do
    lit 2 `shouldBe` (2 :: Integer)
    add (lit 2) (lit 3) `shouldBe` (5 :: Integer)
    mul (lit 2) (lit 3) `shouldBe` (6 :: Integer)
    mul (add (lit 2) (lit 3)) (lit 4) `shouldBe` (20 :: Integer)

spec_Bool :: Spec
spec_Bool =

  it "as an instance of Expr" $ do
    lit (-1) `shouldBe` False
    lit 0 `shouldBe` False
    lit 1 `shouldBe` True
    add (lit 1) (lit 1) `shouldBe` True
    add (lit (-1)) (lit 1) `shouldBe` True
    add (lit (-1)) (lit (-1)) `shouldBe` False
    mul (lit 1) (lit 1) `shouldBe` True
    mul (lit (-1)) (lit 1) `shouldBe` False
    mul (lit (-1)) (lit (-1)) `shouldBe` False

spec_MinMax :: Spec
spec_MinMax =

  it "as an instance of Expr" $ do
    minMaxValue (lit 1 :: MinMax) `shouldBe` 1
    add (lit 1) (lit 2) `shouldBe` MinMax 2
    mul (lit 1) (lit 2) `shouldBe` MinMax 1

spec_Mod7 :: Spec
spec_Mod7 =

  it "as an instance of Expr" $ do
    mod7Value (lit 6 :: Mod7) `shouldBe` 6
    mod7Value (lit 14 :: Mod7)  `shouldBe` 0
    add (lit 6) (lit 7) `shouldBe` Mod7 6
    add (lit 6) (lit 8) `shouldBe` Mod7 0
    mul (lit 6) (lit 7) `shouldBe` Mod7 0
    mul (lit 6) (lit 8) `shouldBe` Mod7 6
