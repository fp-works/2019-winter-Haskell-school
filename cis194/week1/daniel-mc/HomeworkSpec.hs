module HomeworkSpec where

import           Homework

import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "toDigits" $ do
    it "should return an empty list for a negative integer" $ toDigits (-1) `shouldBe` []

    it "should return an empty list for 0" $ toDigits 0 `shouldBe` []

    it "should return a list of digits for a positive integer" $ toDigits 123 `shouldBe` [1, 2, 3]

  describe "doubleEveryOther" $ do
    it "should leave empty list unchanged" $ doubleEveryOther [] `shouldBe` []

    it "should leave list with one integer unchanged" $ doubleEveryOther [2] `shouldBe` [2]

    it "should double first integer in list of two" $ doubleEveryOther [2, 3] `shouldBe` [4, 3]

    it "should double every second integer starting from the right"
      $          doubleEveryOther [2, 3, 5, 7, 11, 13, 17]
      `shouldBe` [2, 6, 5, 14, 11, 26, 17]

  describe "sumDigits" $ do
    it "should return 0 for an empty list" $ sumDigits [] `shouldBe` 0

    it "should sum digits of a single item" $ sumDigits [123] `shouldBe` 6

    it "should sum digits of multiple items" $ sumDigits [12, 34, 5] `shouldBe` 15

  describe "validate" $ do
    it "should return False for an invalid card number" $ validate 123 `shouldBe` False

    it "should return True for a valid card number" $ validate 4012888888881881 `shouldBe` True

  describe "hanoi" $ do
    it "should return no moves for 0 discs" $ abcHanoi 0 `shouldBe` []

    it "should return no moves for negative discs" $ abcHanoi (-2) `shouldBe` []

    it "should return correct move for 1 disc" $ abcHanoi 1 `shouldBe` [(a, b)]

    it "should return correct moves for 2 discs" $ abcHanoi 2 `shouldBe` [(a, c), (a, b), (c, b)]
 where
  a = "a"
  b = "b"
  c = "c"
  abcHanoi x = hanoi x a b c
