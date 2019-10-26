import AParser
import Data.Char
import SExpr
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "SExpr" $ do
      describe "exercise 1" $ do
        it "should return correct results of zeroOrMore" $ do
          runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe`
            Just ("ABC", "dEfgH")
          runParser (zeroOrMore (satisfy isUpper)) "aBCdEfgH" `shouldBe`
            Just ("", "aBCdEfgH")
        it "should return correct results of oneOrMore" $ do
          runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe`
            Just ("ABC", "dEfgH")
          runParser (oneOrMore (satisfy isUpper)) "aBCdEfgH" `shouldBe` Nothing
      describe "exercise 2" $ do
        it "should return correct results of spaces" $ do
          runParser spaces "  test123" `shouldBe` Just ("  ", "test123")
          runParser spaces "test123" `shouldBe` Just ("", "test123")
        it "should return correct results of ident" $ do
          runParser ident "foobar baz" `shouldBe` Just ("foobar", " baz")
          runParser ident "foo33fA" `shouldBe` Just ("foo33fA", "")
          runParser ident "2bad" `shouldBe` Nothing
          runParser ident "" `shouldBe` Nothing
      describe "exercise 3" $ do
        it "should return correct results of Parser SExpr" $ do
          (show $ runParser parseSExpr "#") `shouldBe` "Nothing"
          (show $ runParser parseSExpr "5") `shouldBe` "Just (A (N 5),\"\")"
          (show $ runParser parseSExpr "foo3") `shouldBe`
            "Just (A (I \"foo3\"),\"\")"
          (show $ runParser parseSExpr "(bar (foo) 3 5 874)") `shouldBe`
            "Just (Comb [A (I \"bar\"),Comb [A (I \"foo\")],A (N 3),A (N 5),A (N 874)],\"\")"
          (show $
           runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)") `shouldBe`
            "Just (Comb [Comb [Comb [A (I \"lambda\"),A (I \"x\"),Comb [A (I \"lambda\"),A (I \"y\"),Comb [A (I \"plus\"),A (I \"x\"),A (I \"y\")]]],A (N 3)],A (N 5)],\"\")"
          (show $ runParser parseSExpr "( lots of ( spaces in ) this ( one ) )") `shouldBe`
            "Just (Comb [A (I \"lots\"),A (I \"of\"),Comb [A (I \"spaces\"),A (I \"in\")],A (I \"this\"),Comb [A (I \"one\")]],\"\")"
