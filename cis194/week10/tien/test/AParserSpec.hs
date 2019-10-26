import AParser
import Data.Char
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "AParser" $ do
      let f = toInteger . digitToInt
      let newParserInt = fmap f (char '2')
      let testStr1 = "xyz234"
      let testStr2 = "2xyz"
      describe "exercise 1" $ do
        it "should fmap Parser Char to Parser Integer" $ do
          runParser newParserInt testStr1 `shouldBe` (runParser posInt testStr1)
          runParser newParserInt testStr2 `shouldBe` (runParser posInt testStr2)
      describe "exercise 3" $ do
        describe "3.1" $ do
          it "should return correct results of Parser ('a','b')" $ do
            runParser abParser "abcdef" `shouldBe` Just (('a', 'b'), "cdef")
            runParser abParser "aebcdf" `shouldBe` Nothing
        describe "3.2" $ do
          it "should return correct results of Parser ()" $ do
            runParser abParser_ "abcdef" `shouldBe` Just ((), "cdef")
            runParser abParser "aebcdf" `shouldBe` Nothing
        describe "3.3" $ do
          it "should return correct reults of intPair" $ do
            runParser intPair "12 34" `shouldBe` (Just ([12, 34], ""))
      describe "exercise 4 & 5" $ do
        it "should use choice to determine correct results" $ do
          runParser intOrUppercase "342abcd" `shouldBe` Just ((), "abcd")
          runParser intOrUppercase "XYZ" `shouldBe` Just ((), "YZ")
          runParser intOrUppercase "foo" `shouldBe` Nothing
