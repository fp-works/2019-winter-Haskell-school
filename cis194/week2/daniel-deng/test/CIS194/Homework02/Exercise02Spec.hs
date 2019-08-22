module CIS194.Homework02.Exercise02Spec where

import CIS194.Homework02.Log
import CIS194.Homework02.Exercise02
import CIS194.Homework02.Exercise04

import Test.Tasty.Hspec

spec_insert :: Spec
spec_insert = do

  describe "with a LogMessage of Unknown" $

    it "returns the same tree" $ do
      let unknown = Unknown "Some message"
      let info    = LogMessage Info 1 "Some message"
      let tree    = Node Leaf info Leaf
      insert unknown Leaf `shouldBe` Leaf
      insert unknown tree `shouldBe` tree

  describe "with a Leaf node" $

    it "returns a new tree with two Leaf nodes between the LogMessage" $ do
      let logMsg = LogMessage Info 1 "Some message"
      insert logMsg Leaf `shouldBe` Node Leaf logMsg Leaf

  describe "with a known LogMessage and a MessageTree with sub-nodes" $ do

    let msg_ts_2 = LogMessage Info      2 "Some message"
    let msg_ts_4 = LogMessage Warning   4 "Some message"
    let msg_ts_6 = LogMessage (Error 1) 6 "Some message"

    context "inserting timestamp 2 to a tree of timestamp 4 and 6" $ do

      let tree    = insert msg_ts_6 . insert msg_ts_4 $ Leaf
      let newTree = insert msg_ts_2 tree

      it "should return a a sorted MessageTree with all 3 LogMessages" $
        inOrder newTree `shouldBe` [msg_ts_2, msg_ts_4, msg_ts_6]

    context "inserting timestamp 4 to a tree of timestamp 2 and 6" $ do

      let tree    = insert msg_ts_6 . insert msg_ts_2 $ Leaf
      let newTree = insert msg_ts_4 tree

      it "should return a a sorted MessageTree with all 3 LogMessages" $
        inOrder newTree `shouldBe` [msg_ts_2, msg_ts_4, msg_ts_6]

    context "inserting timestamp 6 to a tree of timestamp 2 and 4" $ do

      let tree    = insert msg_ts_4 . insert msg_ts_2 $ Leaf
      let newTree = insert msg_ts_6 tree

      it "should return a a sorted MessageTree with all 3 LogMessages" $
        inOrder newTree `shouldBe` [msg_ts_2, msg_ts_4, msg_ts_6]
