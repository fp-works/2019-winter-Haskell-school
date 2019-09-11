import           Test.Hspec

import           Calc
import qualified ExprT      as ET
import           StackVM    as SVM

main :: IO ()
main =
  hspec $ do
    describe "Calc" $
      -- Exercise 1 --
     do
      it "should eval given equations" $ do
        eval (ET.Mul (ET.Add (ET.Lit 2) (ET.Lit 3)) (ET.Lit 4)) `shouldBe` 20
      -- Exercise 2 --
      it "should eval given strings with Maybe wrapped" $ do
        evalStr "(2+3)*4" `shouldBe` Just 20
        evalStr "2+3*" `shouldBe` Nothing
      -- -- Exercise 3 --
      it "should have the same expression when using Expr typeclass" $ do
        reify (mul (add (lit 2) (lit 3)) (lit 4)) `shouldBe`
          (ET.Mul (ET.Add (ET.Lit 2) (ET.Lit 3)) (ET.Lit 4))
      -- Exercise 4 --
      it "should do specific calculations based on the types" $ do
        testInteger `shouldBe` Just (-7)
        testBool `shouldBe` Just True
        testMM `shouldBe` (Just $ MinMax 5)
        testSat `shouldBe` (Just $ Mod7 14)
      -- Exercise 5 --
      it "should compile the given arithmetic string to Program" $ do
        (show . compile $ "(2+3)*4") `shouldBe`
          "Just [PushI 2,PushI 3,Add,PushI 4,Mul]"
