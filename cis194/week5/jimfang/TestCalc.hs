import Test.Hspec
import Calc
import ExprT

main :: IO ()
main = hspec $ do
  describe "eval" $ do
    it "should return the correct result" $ do
      eval (Lit 12) `shouldBe` 12
      eval (Add (Lit 10) (Lit 12)) `shouldBe` 22
      eval (Mul (Lit 10) (Lit 12)) `shouldBe` 120
      eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20