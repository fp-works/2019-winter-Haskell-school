module CalcSpec where

import qualified Data.Map                      as M
import           Calc
import           ExprT
import qualified StackVM                       as VM

import           Test.Hspec

deriving instance Eq VM.StackExp

main :: IO ()
main = hspec $ do
  describe "eval" $
    it "should work with nested expressions" $
      eval (Mul (Mul (Lit 2) (Lit 3)) (Add (Lit 1) (Lit 2))) `shouldBe` 18

  describe "evalStr" $ do
    it "should be Nothing for an invalid expression" $
      evalStr "2+" `shouldBe` Nothing

    it "should work for a valid expression" $
      evalStr "2+3*4" `shouldBe` Just 14

  describe "Expr ExtrT" $
    it "should have working ExprT instance" $
      (add (lit 1) (mul (lit 2) (lit 3)) :: ExprT) `shouldBe` Add (Lit 1) (Mul (Lit 2) (Lit 3))

  describe "Expr Integer" $
    it "should work" $
      (add (lit 1) (mul (lit 2) (lit 3)) :: Integer) `shouldBe` 7

  describe "Expr Bool" $ do
    it "lit should be False for non-positive" $
      (lit 0 :: Bool) `shouldBe` False

    it "lit should be True for positive" $
      (lit 1 :: Bool) `shouldBe` True

    it "add should be or" $
      (add True False, add False True, add False False, add True True)
        `shouldBe` (True, True, False, True)

    it "mul should be and" $
      (mul True False, mul False True, mul False False, mul True True)
        `shouldBe` (False, False, False, True)

  describe "Expr Mod7" $ do
    it "lit should mod 7 input" $
      (lit 10 :: Mod7) `shouldBe` Mod7 3

    it "add should mod 7 result" $
      add (Mod7 5) (Mod7 4) `shouldBe` Mod7 2

    it "mul should mod 7 result" $
      mul (Mod7 5) (Mod7 2) `shouldBe` Mod7 3

  describe "compile" $
    it "should convert string to StackVM program" $
      compile "(4+2)*(1+3)"
      `shouldBe` Just [VM.PushI 4, VM.PushI 2, VM.Add, VM.PushI 1, VM.PushI 3, VM.Add, VM.Mul]

  describe "(Expr, HasVars) (M.Map String Integer -> Maybe Integer)" $ do
    it "should get variable from map" $
      (var "x" :: M.Map String Integer -> Maybe Integer) (M.fromList [("x", 5)])
      `shouldBe` Just 5

    it "should be Nothing when variable is missing" $
      (var "x" :: M.Map String Integer -> Maybe Integer) (M.fromList [("y", 5)])
      `shouldBe` Nothing

    it "should support lit, add, mul" $
      (add (lit 5) (mul (lit 4) (var "x")) :: M.Map String Integer -> Maybe Integer) (M.fromList [("x", 3)])
      `shouldBe` Just 17
