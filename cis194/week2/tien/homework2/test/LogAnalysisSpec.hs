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
      -- Exercise 2 --
      it "should return original message tree when entering a unknown log" $ do
        insert notlog oneNodeMessageTree `shouldBe` (oneNodeMessageTree)
      it "should return correct message tree with initial Leaf message tree" $ do
        insert log1 Leaf `shouldBe` (Node Leaf log1 Leaf)
      it
        "should correct message tree with a log with earlier timestamp given one node message tree" $ do
        insert log1 oneNodeMessageTree `shouldBe`
          (Node (Node Leaf log1 Leaf) log2 Leaf)
      it
        "should correct message tree with a log with later timestamp given one node message tree" $ do
        insert log3 oneNodeMessageTree `shouldBe`
          (Node Leaf log2 (Node Leaf log3 Leaf))
      it
        "should correct message tree with a log with later timestamp given two nodes message tree 1" $ do
        insert log4 twoNodesMessageTree1 `shouldBe`
          (Node (Node Leaf log1 Leaf) log2 (Node Leaf log4 Leaf))
      it
        "should correct message tree with a log with later timestamp given two nodes message tree 2" $ do
        insert log4 twoNodesMessageTree2 `shouldBe`
          (Node Leaf log2 (Node Leaf log3 (Node Leaf log4 Leaf)))
      -- Excercise 3 --
      it "should return a sorted message tree" $ do
        let testLogMessageList = [log2, log3, log1, log5, log4]
        build testLogMessageList `shouldBe`
          (Node
             (Node Leaf log1 (Node (Node Leaf log2 Leaf) log3 Leaf))
             log4
             (Node Leaf log5 Leaf))
      it "should return a sorted message tree with unknown log" $ do
        let testLogMessageListWithUnKnownLog =
              [log2, log3, log1, notlog, log5, log4]
        build testLogMessageListWithUnKnownLog `shouldBe`
          (Node
             (Node Leaf log1 (Node (Node Leaf log2 Leaf) log3 Leaf))
             log4
             (Node Leaf log5 Leaf))
      -- Exercise 4 --
      it "should return a sorted log message list" $ do
        let testMessageTree =
              Node
                (Node Leaf log1 (Node (Node Leaf log2 Leaf) log3 Leaf))
                log4
                (Node Leaf log5 Leaf)
        inOrder testMessageTree `shouldBe` ([log1, log2, log3, log4, log5])
      -- Exercise 5 --
      it
        "should return correct error message in a string list from a unsorted log message list" $ do
        let testLogMessageList = [log4, log3, logErr53, log5, logErr51, log2]
        whatWentWrong testLogMessageList `shouldBe` (["log 51", "log 53"])
