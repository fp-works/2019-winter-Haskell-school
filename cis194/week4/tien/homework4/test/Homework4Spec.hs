import           Homework4
import           Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "Homework4" $
      -- Exercise 1.1 --
     do
      it "should return same results as fun1" $ do
        let a = [7, 3, 2, 5, 6, 2, 8]
        fun1' a `shouldBe` fun1 a
      -- Exercise 1.2 --
      it "should return same results as fun2" $
        -- cannot test 0 or minus numbers fun2 will not converge --
       do
        fun2' 1 `shouldBe` fun2 1
        fun2' 2 `shouldBe` fun2 2
        fun2' 3 `shouldBe` fun2 3
        fun2' 4 `shouldBe` fun2 4
        fun2' 5 `shouldBe` fun2 5
      -- Exercise 2 --
      it "should return a balanced Tree" $ do
        let (Node _ left _ right) = foldTree "ABCDEFGHIJ"
        checkDepth left - checkDepth right `shouldSatisfy` (< 2)
        let (Node _ left _ right) = foldTree "ABCDEFG"
        checkDepth left `shouldBe` checkDepth right
      -- Exercise 3.1 --
      it "should return True if the number of True is odd" $ do
        xor [False, True, False] `shouldBe` True
        xor [False, True, False, False, True] `shouldBe` False
      -- Exercise 3.2 --
      it "should return the same result as map" $ do
        let a = [2, 3, 4, 5]
        map' (* 2) a `shouldBe` map (* 2) a
      -- Exercise 3.3 --
      it "should return the same result as foldl" $ do
        let base = 0
        let xs = [1, 3, 5]
        foldl (-) base xs `shouldBe` myFoldl (-) base xs
      -- Exercise 4 --
      it "should return prime numbers up to 2n+2 given an integer n" $ do
        sieveSundaram 5 `shouldBe` [3, 5, 7, 11]
        sieveSundaram 30 `shouldBe`
          [3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61]
