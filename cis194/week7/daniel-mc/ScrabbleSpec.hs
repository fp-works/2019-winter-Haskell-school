module ScrabbleSpec where

import           Buffer
import           Sized
import           JoinList
import           Scrabble

import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "score" $ do
    it "scores lowercase correctly" $ score 'a' `shouldBe` 1
    it "scores uppercase correctly" $ score 'A' `shouldBe` 1
    it "scores non-letter" $ score '_' `shouldBe` 0

  describe "scoreString" $ it "scores correctly" $ scoreString "yay Haskell!" `shouldBe` 23

  describe "Buffer JoinList" $ do
    it "converts to string"
      $ toString (Append (Score 17, Size 2) (Single mempty "Hello") (Single mempty "World"))
      `shouldBe` "Hello\nWorld"

    it "converts from string" $ fromString "Hello\nWorld" `shouldBe` Append
      (Score 17, Size 2)
      (Single (Score 8, Size 1) "Hello")
      (Single (Score 9, Size 1) "World")

    it "replaces a line" $ replaceLine 0 "Hey" (Single (Score 8, Size 1) "Hello") `shouldBe` Single
      (Score 9, Size 1)
      "Hey"

    it "should gives score as value" $ value (Single (Score 8, Size 1) "Hello") `shouldBe` 8
