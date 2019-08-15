import           Test.Hspec

import           Log
import           LogAnalysis

main :: IO ()
main =
  hspec $ do
    let infoLog = "I 29 la la la"
    let warningLog = "W 823 warning message"
    let errorLog = "E 2 562 help help"
    let unknownLog = "This is an unknown log"
    describe "LogAnalysis." $ do
      it "should parse correct log messages" $ do
        parseMessage infoLog `shouldBe` (LogMessage Info 29 "la la la")
        parseMessage warningLog `shouldBe` (LogMessage Warning 823 "warning message")
        parseMessage errorLog `shouldBe` (LogMessage ( Error 2 ) 562 "help help")
        parseMessage unknownLog `shouldBe` (Unknown "This is an unknown log")
