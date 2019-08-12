import           Exercise1
import           Exercise2
import           Exercise3
import           Exercise4
import           Exercise5
import           Exercise6
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Exercise 1" $ do
    it "given -1, expect empty list" $
      toDigits (-1) `shouldBe` []
    it "given 0, expect empty list" $
      toDigits 0 `shouldBe` []
    it "given an integer, expect its digits decomposed into a list" $
      toDigits 123456789 `shouldBe` [1..9]
    it "expect digits decomposed in reversed order" $
      toDigitsRev 123456789 `shouldBe` reverse [1..9]

  describe "Exercise 2" $ do
    it "expect element-1 and element-3 get doubled" $
      doubleEveryOther [8, 7, 6, 5] `shouldBe` [8, 14, 6, 10]
    it "expect element-1 gets doubled" $
      doubleEveryOther [1, 2, 3] `shouldBe` [1, 4, 3]

  describe "Exercise 3" $ do
    it "given an empty list, expect sum is 0" $
      sumDigits [] `shouldBe` 0
    it "expect sum of digits" $
      sumDigits [16, 7, 12, 5] `shouldBe` 22

  describe "Exercise 4" $ do
    it "expect valid credit card num" $
      validateCreditcard 4012888888881881 `shouldBe` True
    it "expect invalid credit card num" $
      validateCreditcard 4012888888881882 `shouldBe` False

  describe "Exercise 5" $
    it "expect total num of moves" $
      length (hanoi 15 "a" "b" "c") `shouldBe` 2^15 - 1

  describe "Exercise 6" $
    it "expect total num of moves is optimal" $
      length (hanoi4 15 "a" "b" "c" "d") `shouldBe` 129
