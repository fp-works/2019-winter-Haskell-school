import Test.Hspec

import Data.Char (isUpper)

import AParser (Parser(..), satisfy)
import SExpr

main = hspec $ do
  describe "zeroOrMore" $ do
    it "should return empty list for incompatible input" $ do
      runParser (zeroOrMore (satisfy isUpper)) "123"
        `shouldBe` Just ("", "123")
    it "should return nonempty list for compatible input" $ do
      runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH"
        `shouldBe` Just ("ABC", "dEfgH")

  describe "oneOrMore" $ do
    it "should return Nothing for incompatible input" $ do
      runParser (oneOrMore (satisfy isUpper)) "123"
        `shouldBe` Nothing
    it "should return nonempty list for compatible input" $ do
      runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH"
        `shouldBe` Just ("ABC", "dEfgH")

  describe "spaces" $ do
    it "should return empty list for incompatible input" $ do
      runParser spaces "123" `shouldBe` Just ("", "123")
    it "should return nonempty list for compatible input" $ do
      runParser spaces "    dEfgH" `shouldBe` Just ("    ", "dEfgH")

  describe "ident" $ do
    it "should return empty list for incompatible input" $ do
      runParser ident "2bad" `shouldBe` Nothing
      runParser ident "" `shouldBe` Nothing
    it "should return nonempty list for compatible input" $ do
      runParser ident "foobar baz" `shouldBe` Just ("foobar", " baz")
      runParser ident "foo33" `shouldBe` Just ("foo33", "")

  describe "parseSExpr" $ do
    it "should return SExpr for valid input" $ do
      runParser parseSExpr "5" `shouldBe` Just (A (N 5),"")
      runParser parseSExpr "foo3" `shouldBe` Just (A (I "foo3"),"")
      runParser parseSExpr "(bar (foo) 3 5 874)"
        `shouldBe`
        Just (Comb [
                A (I "bar"),
                Comb [
                  A (I "foo")],
                A (N 3),
                A (N 5),
                A (N 874)], "")
      runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)"
        `shouldBe`
        Just (Comb [
                Comb [
                  Comb [
                    A (I "lambda"),
                    A (I "x"),
                    Comb [
                      A (I "lambda"),
                      A (I "y"),
                      Comb [
                        A (I "plus"),
                        A (I "x"),
                        A (I "y")]]],
                  A (N 3)],
                A (N 5)], "")
      runParser parseSExpr "(   lots  of   (  spaces   in  )  this ( one ) )"
        `shouldBe`
        Just (Comb [
                A (I "lots"),
                A (I "of"),
                Comb [
                  A (I "spaces"),
                  A (I "in")],
                A (I "this"),
                Comb [A (I "one")]], "")
