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
