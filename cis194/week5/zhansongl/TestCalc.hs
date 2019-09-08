
import Test.Hspec
import Calc
import ExprT
import qualified StackVM as S

main :: IO ()
main = hspec $ do
  describe "eval" $ do
    it "should return the correct result" $ do
      eval (Lit 12) `shouldBe` 12
      eval (Add (Lit 10) (Lit 12)) `shouldBe` 22
      eval (Mul (Lit 10) (Lit 12)) `shouldBe` 120
      eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20

  describe "evalStr" $ do
    it "should return Nothing for malformed input" $ do
      evalStr "(2+3)*4" `shouldBe` (Just 20)
      evalStr "2+3*4" `shouldBe` (Just 14)
      evalStr "(2+3)*" `shouldBe` Nothing

  describe "testExp" $ do
    it "should behave correctly for every instance" $ do
      (testExp "(3 * -4) + 5" :: Maybe Integer) `shouldBe` Just (-7)
      (testExp "(3 * -4) + 5" :: Maybe Bool) `shouldBe` Just True
      (testExp "3 * -4" :: Maybe Bool) `shouldBe` Just False
      (testExp "3 + -4" :: Maybe Bool) `shouldBe` Just True
      (testExp "(3 * -4) + 5" :: Maybe MinMax) `shouldBe` Just (MinMax 5)
      (testExp "(3 * -4) + 5" :: Maybe Mod7) `shouldBe` Just (Mod7 0)

  describe "compile" $ do
    it "should return the correct assembly" $ do
      (S.stackVM . maybe [] id $ compile "1+2") `shouldBe` (Right $ S.IVal 3)
      (S.stackVM . maybe [] id $ compile "1*2") `shouldBe` (Right $ S.IVal 2)

  describe "withVars" $ do
    it "should return Nothing when variable is not mapped" $ do
      (withVars [("x", 6)] $ add (lit 3) (var "y")) `shouldBe` Nothing
    it "should return the correct results when variable is mapped" $ do
      (withVars [("x", 6)] $ add (lit 3) (var "x")) `shouldBe` (Just 9)
      (withVars [("x", 6), ("y", 3)] $
        mul (var "x") (add (var "y") (var "x"))) `shouldBe` (Just 54)

