import           Homework4
import           Test.Hspec

main::IO()
main =
  hspec $ do
    describe "Homework4" $ do
      -- Exercise 1.1 --
      it "should return same results as fun1" $ do
        let a = [7,3,2,5,6,2,8]
        fun1' a `shouldBe` fun1 a
      -- Exercise 1.2 --
      it "should return same results as fun2" $ do
        -- cannot test 0 or minus numbers fun2 will not converge --
        fun2' 1 `shouldBe` fun2 1
        fun2' 2 `shouldBe` fun2 2
        fun2' 3 `shouldBe` fun2 3
        fun2' 4 `shouldBe` fun2 4
        fun2' 5 `shouldBe` fun2 5
