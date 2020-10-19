module SExprSpec where

import           AParser
import           SExpr
import           Test.Hspec

spec :: Spec
spec = do
  describe "ex1" $ do
    it "works with zeroOrMore" $ do
      runParser (zeroOrMore (char 'a')) "bcd" `shouldBe` Just ("", "bcd")
      runParser (zeroOrMore (char 'a')) "abcd" `shouldBe` Just ("a", "bcd")
      runParser (zeroOrMore (char 'a')) "aaaaabcd" `shouldBe` Just ("aaaaa", "bcd")
      runParser (zeroOrMore (char 'a')) "aaaaa" `shouldBe` Just ("aaaaa", "")
      runParser (zeroOrMore (char 'a')) "a" `shouldBe` Just ("a", "")
      runParser (zeroOrMore (char 'a')) "" `shouldBe` Just ("", "")

    it "works with oneOrMore" $ do
      runParser (oneOrMore (char 'a')) "bcd" `shouldBe` Nothing
      runParser (oneOrMore (char 'a')) "abcd" `shouldBe` Just ("a", "bcd")
      runParser (oneOrMore (char 'a')) "aaaaabcd" `shouldBe` Just ("aaaaa", "bcd")
      runParser (oneOrMore (char 'a')) "aaaaa" `shouldBe` Just ("aaaaa", "")
      runParser (oneOrMore (char 'a')) "a" `shouldBe` Just ("a", "")
      runParser (oneOrMore (char 'a')) "" `shouldBe` Nothing

  describe "ex2" $ do
    it "works with spaces" $ do
      runParser spaces "bcd" `shouldBe` Just ("", "bcd")
      runParser spaces " bcd" `shouldBe` Just (" ", "bcd")
      runParser spaces "     bcd" `shouldBe` Just ("     ", "bcd")
      runParser spaces "     " `shouldBe` Just ("     ", "")
      runParser spaces " " `shouldBe` Just (" ", "")
      runParser spaces "" `shouldBe` Just ("", "")

    it "works with ident" $ do
      runParser ident "Foobar baz" `shouldBe` Just ("Foobar", " baz")
      runParser ident "foobar baz" `shouldBe` Just ("foobar", " baz")
      runParser ident "foobar123 baz" `shouldBe` Just ("foobar123", " baz")
      runParser ident "foo33fA" `shouldBe` Just ("foo33fA", "")
      runParser ident "2bad" `shouldBe` Nothing
      runParser ident "_bad" `shouldBe` Nothing
      runParser ident "" `shouldBe` Nothing

  describe "ex3" $ do
    it "works with atom" $ do
      runParser parseAtom "bcd def" `shouldBe` Just (I "bcd", " def")
      runParser parseAtom "123 def" `shouldBe` Just (N 123, " def")
      runParser parseAtom "1bcd def" `shouldBe` Just (N 1, "bcd def")
      runParser parseAtom " bcd def" `shouldBe` Nothing
      runParser parseAtom " 123 def" `shouldBe` Nothing

    it "works with postIntAlone" $ do
      runParser posIntAlone "bcd def" `shouldBe` Nothing
      runParser posIntAlone "123 def" `shouldBe` Just (123, " def")
      runParser posIntAlone "123)def" `shouldBe` Just (123, ")def")
      runParser posIntAlone "123def" `shouldBe` Nothing

    it "works with atomIntAlone" $ do
      runParser parseAtomIntAlone "bcd def" `shouldBe` Just (I "bcd", " def")
      runParser parseAtomIntAlone "123 def" `shouldBe` Just (N 123, " def")
      runParser parseAtomIntAlone "1bcd def" `shouldBe` Nothing
      runParser parseAtomIntAlone " bcd def" `shouldBe` Nothing
      runParser parseAtomIntAlone " 123 def" `shouldBe` Nothing

    it "works with SExpr" $ do
      runParser parseSExpr "5" `shouldBe` Just (A (N 5), "")
      runParser parseSExpr "foo3" `shouldBe` Just (A (I "foo3"), "")
      runParser parseSExpr "3foo" `shouldBe` Just (A (N 3), "foo")
      runParser parseSExpr "(foo3)" `shouldBe` Just (Comb [A (I "foo3")], "")
      runParser parseSExpr " ((foo)(bar))  " `shouldBe`Just (
        Comb [Comb [A (I "foo")], Comb [A (I "bar")]], "")
      runParser parseSExpr "(bar (foo) 3 5 874)" `shouldBe` Just (
        Comb [A (I "bar"), Comb [A (I "foo")], A (N 3), A (N 5), A (N 874)], "")
      runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)" `shouldBe` Just (
        Comb [
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
      runParser parseSExpr "  ( lots of ( spaces in ) this ( one ) )    " `shouldBe` Just (
        Comb [A (I "lots"), A (I "of"),
              Comb [A (I "spaces"), A (I "in")], A (I "this"), Comb [A (I "one")]], "")

    it "works with SExpr for invalid s-expr" $ do
      runParser parseSExpr "(foo" `shouldBe` Nothing
      runParser parseSExpr "()" `shouldBe` Nothing
      runParser parseSExpr "(3foo)" `shouldBe` Nothing -- probably the most tricky case
