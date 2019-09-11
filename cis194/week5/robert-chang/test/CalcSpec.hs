module CalcSpec where
import Calc
import Test.Hspec
import ExprT
import Parser

spec :: Spec
spec =
  describe "Week5" $ do
   evalSpec
   evalStrSpec
   exprSpec

evalSpec :: Spec
evalSpec = do
  it "return correct value" $
    eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20

evalStrSpec :: Spec
evalStrSpec = do 
  it "returns correct value" $
    evalStr "(2+3)*4" `shouldBe` Just 20 

  it "returns correct value" $
    evalStr "2+3*4" `shouldBe` Just 14 

  it "returns correct value" $
    evalStr "2+3*" `shouldBe` Nothing

exprSpec :: Spec
exprSpec = do 
  it "return correct value" $
    reify (add (lit 2) (lit 3)) `shouldBe` Add (Lit 2) (Lit 3)
  it "return correct value for lit" $
    (lit 1) `shouldBe` True
  it "return correct value for lit" $
    (lit (-1)) `shouldBe` False 
  it "return correct value for add" $
    (add (lit 1) (lit 1)) `shouldBe` True
  it "return correct value for multiplication" $
    (mul (lit 1) (lit 1)) `shouldBe` True
  it "return correct value for MinMax" $ do
    (lit 1) `shouldBe` (MinMax 1)
    (add (lit 1) (lit 2)) `shouldBe` (MinMax 2)
    (mul (lit 1) (lit 2)) `shouldBe` (MinMax 1)
    (lit 1) `shouldBe` (Mod7 1)
    (lit 8) `shouldBe` (Mod7 1)
    (add (lit 1) (lit 2)) `shouldBe` (Mod7 3)
    (mul (lit 10) (lit 2)) `shouldBe` (Mod7 6) 
