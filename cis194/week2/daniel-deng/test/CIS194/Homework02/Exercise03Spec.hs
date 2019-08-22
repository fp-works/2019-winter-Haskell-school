module CIS194.Homework02.Exercise03Spec where

import CIS194.Homework02.Log
import CIS194.Homework02.Exercise03
import CIS194.Homework02.Exercise04

import Test.Tasty.Hspec

spec_build :: Spec
spec_build = do

  let msg_ts_2 = LogMessage Info      2 "Some message"
  let msg_ts_4 = LogMessage Warning   4 "Some message"
  let msg_ts_6 = LogMessage (Error 1) 6 "Some message"

  context "when building with ordered LogMessages" $

    it "returns an ordered MessageTree" $ do
      let input          = [msg_ts_2, msg_ts_4, msg_ts_6]
      let expectedOutput = [msg_ts_2, msg_ts_4, msg_ts_6]
      (inOrder . build $ input) `shouldBe` expectedOutput

  context "when building with unordered LogMessages" $

    it "returns an ordered MessageTree" $ do
      let input          = [msg_ts_2, msg_ts_6, msg_ts_4]
      let expectedOutput = [msg_ts_2, msg_ts_4, msg_ts_6]
      (inOrder . build $ input) `shouldBe` expectedOutput
