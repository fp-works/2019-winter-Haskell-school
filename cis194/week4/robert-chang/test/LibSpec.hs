module LibSpec where
import Lib
import Test.Hspec

spec :: Spec
spec =
  describe "Week4" $ do
   wholemealSpec
   moreFoldsSpec

wholemealSpec :: Spec
wholemealSpec = do
  it "returns 1 for an empty list" $
    fun1' [] `shouldBe` fun1 [] 

  it "returns the product even numbers subtracting 2" $
    fun1' [4, 5, 6, 7, 8, 9, 10] `shouldBe` fun1 [4, 5, 6, 7, 8, 9, 10]

  it "case 1" $
    fun2' 1 `shouldBe` fun2 1 

  it "case 2" $
    fun2' 2 `shouldBe` fun2 2 

  it "case 3" $
    fun2' 3 `shouldBe` fun2 3 

  it "case 100" $
    fun2' 100 `shouldBe` fun2 100

moreFoldsSpec :: Spec
moreFoldsSpec = do 
  it "should return correct result" $
    xor [False, True, False] `shouldBe` True

  it "should return correct result" $
    xor [False, True, False, False, True] `shouldBe` False
