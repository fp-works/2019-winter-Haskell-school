module CIS194.Homework02.Exercise04Spec where

import CIS194.Homework02.Log
import CIS194.Homework02.Exercise03
import CIS194.Homework02.Exercise04

import Test.Tasty.Hspec

spec_inOrder :: Spec
spec_inOrder = do

  let msg_ts_2 = LogMessage Info      2 "Some message"
  let msg_ts_4 = LogMessage Warning   4 "Some message"
  let msg_ts_6 = LogMessage (Error 1) 6 "Some message"

  context "when the input is Leaf" $

    it "returns an empty list" $
      inOrder Leaf `shouldBe` []

  context "when the input is an ordered MessageTree" $

    it "returns a chronological LogMessages" $ do
      let input          = build [msg_ts_2, msg_ts_6, msg_ts_4]
      let expectedOutput = [msg_ts_2, msg_ts_4, msg_ts_6]
      inOrder input `shouldBe` expectedOutput
