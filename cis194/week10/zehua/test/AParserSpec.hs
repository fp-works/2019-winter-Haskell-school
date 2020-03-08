module AParserSpec where

import           AParser
import           Control.Applicative (empty, liftA2, liftA3, (<|>))
import           Data.Char           (ord, toUpper)
import           Test.Hspec

join3Chars :: Char -> Char -> Char -> [Char]
join3Chars c1 c2 c3 = [c1, c2, c3]

spec :: Spec
spec = do
  describe "ex1" $ do
    it "works" $ do
      runParser (fmap toUpper (char 'a')) "abcd" `shouldBe` Just ('A', "bcd")
      runParser (fmap toUpper (char 'a')) "Abcd" `shouldBe` Nothing
      runParser (fmap ord (char 'a')) "abcd" `shouldBe` Just (97, "bcd")

  describe "ex2" $ do
    it "works with pure" $ do
      runParser (pure 'A') "abcd" `shouldBe` Just ('A', "abcd")
      runParser (pure 123) "abcd" `shouldBe` Just (123 :: Int, "abcd")

    it "works with <*>" $ do
      runParser ((,) <$> char 'a' <*> char 'b') "cd" `shouldBe` Nothing
      runParser ((,) <$> char 'a' <*> char 'b') "bcd" `shouldBe` Nothing
      runParser ((,) <$> char 'a' <*> char 'b') "acd" `shouldBe` Nothing
      runParser ((,) <$> char 'a' <*> char 'b') "abcd" `shouldBe` Just (('a', 'b'), "cd")
      runParser (liftA2 (,) (char 'a') (char 'b')) "abcd" `shouldBe` Just (('a', 'b'), "cd")

    it "works with multiple <*>" $ do
      runParser (join3Chars <$> char 'a' <*> char 'b' <*> char 'c') "abcd" `shouldBe` Just ("abc", "d")
      runParser (liftA3 join3Chars (char 'a') (char 'b') (char 'c')) "abcd" `shouldBe` Just ("abc", "d")

  describe "ex3" $ do
    it "works with abParser" $ do
      runParser abParser "cd" `shouldBe` Nothing
      runParser abParser "bcd" `shouldBe` Nothing
      runParser abParser "acd" `shouldBe` Nothing
      runParser abParser "abcd" `shouldBe` Just (('a', 'b'), "cd")

    it "works with abParser_" $ do
      runParser abParser_ "cd" `shouldBe` Nothing
      runParser abParser_ "bcd" `shouldBe` Nothing
      runParser abParser_ "acd" `shouldBe` Nothing
      runParser abParser_ "abcd" `shouldBe` Just ((), "cd")
      runParser abParser_ "abcdefghijklmn" `shouldBe` Just ((), "cdefghijklmn")

    it "works with intPair" $ do
      runParser intPair "a12 34" `shouldBe` Nothing
      runParser intPair "12a34" `shouldBe` Nothing
      runParser intPair "12 a34" `shouldBe` Nothing
      runParser intPair "12 34a" `shouldBe` Just ([12, 34], "a")
      runParser intPair "1234567 3456789 abcdefg" `shouldBe` Just ([1234567, 3456789], " abcdefg")

  describe "ex4" $ do
    it "works with empty" $ do
      runParser empty "abcde" `shouldBe` (Nothing :: Maybe (Char, String))

    it "works with <|>" $ do
      runParser (char 'a' <|> char 'b') "cdefg" `shouldBe` Nothing
      runParser (char 'a' <|> char 'b') "acdefg" `shouldBe` Just ('a', "cdefg")
      runParser (char 'a' <|> char 'b') "bcdefg" `shouldBe` Just ('b', "cdefg")
      runParser (char 'a' <|> char 'b') "abcdefg" `shouldBe` Just ('a', "bcdefg")

  describe "ex5" $ do
    it "works" $ do
      runParser intOrUppercase "342abcd" `shouldBe` Just ((), "abcd")
      runParser intOrUppercase "XYZ" `shouldBe` Just ((), "YZ")
      runParser intOrUppercase "abc" `shouldBe` Nothing
