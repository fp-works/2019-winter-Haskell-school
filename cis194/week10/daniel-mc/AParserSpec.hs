module ScrabbleSpec where

import           AParser

import           Control.Applicative
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "instance Functor Parser" $ do
    it "should map posInt parser" $ runParser ((+ 2) <$> posInt) "10x" `shouldBe` Just (12, "x")

  describe "abParser" $ do
    it "should fail if first char is not a" $ runParser abParser "bcd" `shouldBe` Nothing
    it "should fail if second char is not b" $ runParser abParser "acde" `shouldBe` Nothing
    it "should succeed if string starts with ab" $ runParser abParser "abcd" `shouldBe` Just
      (('a', 'b'), "cd")

  describe "abParser_" $ do
    it "should return ()" $ runParser abParser_ "abcd" `shouldBe` Just ((), "cd")

  describe "intPair" $ do
    it "should parse two int" $ runParser intPair "12 34 56" `shouldBe` Just ([12, 34], " 56")

  describe "instance Alternative Parser" $ do
    it "should use first parser"
      $          runParser (char '1' *> char '2' <|> char '1') "123"
      `shouldBe` Just ('2', "3")

    it "should use second parser if first fails"
      $          runParser (char '1' *> char '2' <|> char '1') "134"
      `shouldBe` Just ('1', "34")

  describe "intOrUppercase" $ do
    it "should fail if lowercase" $ runParser intOrUppercase "abc" `shouldBe` Nothing
    it "should parse int" $ runParser intOrUppercase "123ABC" `shouldBe` Just ((), "ABC")
    it "should parse uppercase" $ runParser intOrUppercase "AB123" `shouldBe` Just ((), "B123")
