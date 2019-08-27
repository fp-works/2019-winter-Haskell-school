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
    let log1 = LogMessage Info 1 "log 1"
    let log2 = LogMessage Warning 2 "log 2"
    let log3 = LogMessage (Error 2) 3 "log 3"
    let log4 = LogMessage (Error 3) 4 "log 4"
    let log5 = LogMessage Warning 5 "log 5"
    let logErr51 = LogMessage (Error 51) 7 "log 51"
    let logErr53 = LogMessage (Error 53) 9 "log 53"
    let notlog = Unknown "this is not a log"
    let oneNodeMessageTree = Node Leaf log2 Leaf
    let twoNodesMessageTree1 = Node (Node Leaf log1 Leaf) log2 Leaf
    let twoNodesMessageTree2 = Node Leaf log2 (Node Leaf log3 Leaf)
    describe "LogAnalysis tests" $
      -- Exercise 1 --
     do
      it "should parse correct log messages" $ do
        parseMessage infoLog `shouldBe` (LogMessage Info 29 "la la la")
        parseMessage warningLog `shouldBe`
          (LogMessage Warning 823 "warning message")
        parseMessage errorLog `shouldBe` (LogMessage (Error 2) 562 "help help")
        parseMessage unknownLog `shouldBe` (Unknown "This is an unknown log")
