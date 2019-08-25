module LogAnalysisSpec where

import Log
import LogAnalysis

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "parseMessage" $ do
    it "should parse an error message" $
      parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"

    it "should parse an info message" $
      parseMessage "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"

    it "should be Unknown for an invalid message" $
      parseMessage "This is not in the right format"
      `shouldBe` Unknown "This is not in the right format"

    it "should be Unknown for an invalid error level" $
      parseMessage "E ab 29 la la la" `shouldBe` Unknown "E ab 29 la la la"

    it "should be Unknown for an invalid timestamp" $
      parseMessage "I ab la la" `shouldBe` Unknown "I ab la la"

  describe "parse" $
    it "should parse a file of log messages" $
      parse "I 1 ab\nE 2 5 error" `shouldBe` [
        LogMessage Info 1 "ab",
        LogMessage (Error 2) 5 "error"
      ]

  describe "insert" $ do
    it "should put message in root for Leaf" $
      insert testMsg1 Leaf `shouldBe` Node Leaf testMsg1 Leaf

    it "should insert earliest message correctly" $
      insert testMsg1 (Node (Node Leaf testMsg2 Leaf) testMsg3 Leaf)
      `shouldBe` Node (Node (Node Leaf testMsg1 Leaf) testMsg2 Leaf) testMsg3 Leaf

    it "should insert latest message correctly" $
      insert testMsg3 (Node (Node Leaf testMsg1 Leaf) testMsg2 Leaf)
      `shouldBe` Node (Node Leaf testMsg1 Leaf) testMsg2 (Node Leaf testMsg3 Leaf)

    it "should insert message inbetween others correctly" $
      insert testMsg2 (Node (Node Leaf testMsg1 Leaf) testMsg3 Leaf)
      `shouldBe` Node (Node Leaf testMsg1 (Node Leaf testMsg2 Leaf)) testMsg3 Leaf

    it "should not insert Unknown" $
      insert (Unknown "") (Node Leaf testMsg1 Leaf) `shouldBe` Node Leaf testMsg1 Leaf

  describe "build" $

    it "should build tree in order" $
      build [testMsg2, testMsg1, testMsg3]
      `shouldBe` Node (Node Leaf testMsg1 (Node Leaf testMsg2 Leaf)) testMsg3 Leaf

  describe "inOrder" $ do

    it "should be empty list for Leaf" $
      inOrder Leaf `shouldBe` []

    it "should work for a single message" $
      inOrder (Node Leaf testMsg1 Leaf) `shouldBe` [testMsg1]

    it "should work with nested nodes" $
      inOrder (Node (Node Leaf testMsg1 (Node Leaf testMsg2 Leaf)) testMsg3 Leaf)
      `shouldBe` [testMsg1, testMsg2, testMsg3]

  describe "whatWentWrong" $ do

    it "should discard Unknown, Info and Warning" $
      whatWentWrong [Unknown "", testMsg1, testMsg2, testMsg3] `shouldBe` []

    it "should filter irrelevant messages and order by timestamp" $
      whatWentWrong [testErr3, testMsg1, testMsg2, testErr1, testMsg3, testErr2]
      `shouldBe` ["e2", "e3"]

  where
    testMsg1 = LogMessage Info 1 ""
    testMsg2 = LogMessage Info 25 ""
    testMsg3 = LogMessage Warning 50 ""
    testErr1 = LogMessage (Error 10) 15 "e1"
    testErr2 = LogMessage (Error 50) 40 "e2"
    testErr3 = LogMessage (Error 80) 70 "e3"
