module HomeworkSpec where
import Homework
import Test.Hspec

spec :: Spec
spec = do
  describe "toDigits" $ do
   it "returns an empty list for a negative integer" $ do
      toDigits (-1) `shouldBe` []

   it "returns an empty list for 0" $ do
      toDigits 0 `shouldBe` []

   it "returns al ist for an integer" $ do
      toDigits 2341 `shouldBe` [2,3,4,1]

  describe "toDigitsRev" $ do
   it "returns reversed list" $ do
      toDigitsRev 1234 `shouldBe` [4,3,2,1]

  describe "doubleEveryOther" $ do
   it "returns an empty list for an empty string" $
    doubleEveryOther [] `shouldBe` []

   it "returns the same list itself for a single element list" $
    doubleEveryOther [1] `shouldBe` [1]

   it "returns the expected list for a multiple elements list" $ do
    doubleEveryOther [1, 2, 3, 4]    `shouldBe` [2, 2, 6, 4]
    doubleEveryOther [1, 2, 3, 4, 5] `shouldBe` [1, 4, 3, 8, 5] 

  describe "sumDigits" $ do
   it "returns 0 for an empty string" $
    sumDigits [] `shouldBe` 0

   it "returns the element value for a single element list" $
    sumDigits [1] `shouldBe` 1

   it "returns the expected result for a list of single digit numbers" $
    sumDigits [1, 2, 3, 4] `shouldBe` 10

   it "returns the expected result for a list of multi digit numbers" $
    sumDigits [16,7,12,5] `shouldBe` 22

  describe "validate" $ do
   it "returns true if is valid credit card number" $
    validate 4012888888881881 `shouldBe` True

   it "returns true if is valid credit card number" $
    validate 4012888888881882 `shouldBe` False
    
  describe "hanoi" $ do
    it "returns correct moves with 0 disc" $ do
      hanoi 0 "a" "b" "c" `shouldBe` []
    it "returns correct moves with 1 disc" $ do
      hanoi 1 "a" "b" "c" `shouldBe` [("a", "b")]
    it "returns correct moves with 2 disc" $ do
      hanoi 2 "a" "b" "c" `shouldBe` [("a","c"), ("a","b"), ("c","b")]
    it "returns correct moves with 3 disc" $ do
      hanoi 3 "a" "b" "c" `shouldBe` [("a", "b"), ("a", "c"), ("b", "c"),
                                        ("a", "b"), ("c", "a"), ("c", "b"),
                                        ("a", "b")]

  describe "hanoi4" $ do
    it "returns correct moves with 0 disc" $ do
      hanoi4 0 "a" "b" "c" "d" `shouldBe` []
    it "returns correct moves with 1 disc" $ do
      hanoi4 1 "a" "b" "c" "d" `shouldBe` [("a", "b")]
    it "returns correct number of moves with 2 disc" $ do
      length (hanoi4 2 "a" "b" "c" "d") `shouldBe` 3
    it "returns correct number of moves with 3 disc" $ do
      length (hanoi4 3 "a" "b" "c" "d") `shouldBe` 5
    it "returns correct number of moves with 15 disc" $ do
      length (hanoi4 15 "a" "b" "c" "d") `shouldBe` 129
      