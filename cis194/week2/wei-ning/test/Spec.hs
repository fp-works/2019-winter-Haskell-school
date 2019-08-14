
import           Exercise1
import           Exercise2
import           Exercise3
import           Exercise4
import           Exercise5
import qualified Log
import           Test.Hspec

import           Control.Monad

runSpec :: IO ()
runSpec = hspec $ do
  describe "Exercise 1" $ do
    it "individual message: expect error level-2" $
      parseMessage "E 2 562 help help\n" `shouldBe` Log.LogMessage (Log.Error 2) 562 "help help"
    it "individual message: expect info" $
      parseMessage "I 29 la la la\n" `shouldBe` Log.LogMessage Log.Info 29 "la la la"
    it "individual message: expect unknown (invalid format)" $
      parseMessage "This is not in the right format" `shouldBe` Log.Unknown "This is not in the right format"
    it "individual message: expect info without message body" $
      parseMessage "I 4993\n" `shouldBe` Log.LogMessage Log.Info 4993 ""
    it "expect 2 messages parsed" $
      parseMessages "I 4993\nI 4993\n" `shouldBe` [Log.LogMessage Log.Info 4993 "", Log.LogMessage Log.Info 4993 ""]
    it "parse whole file: expect 11 log messages (sample.log)" $ do
      logMessages <- Log.testParse parseMessages 9999 "testdata/sample.log"
      length logMessages `shouldBe` 11
    it "parse whole file: expect 5523 log messages (error.log)" $ do
      logMessages <- Log.testParse parseMessages 9999 "testdata/error.log"
      length logMessages `shouldBe` 5523

  describe "Exercise 2" $
    it "insert log messages from sample.log, expect root node" $ do
      logMessages <- Log.testParse parseMessages 9999 "testdata/sample.log"
      let (Log.Node _ m _) = foldr insert Log.Leaf logMessages
      m `shouldBe` Log.LogMessage (Log.Error 99) 10 "Flange failed!"

  describe "Exercise 3" $
    it "insert log messages from error.log, expect root node" $ do
      logMessages <- Log.testParse parseMessages 9999 "testdata/error.log"
      let (Log.Node _ m _) = build logMessages
      m `shouldBe` Log.LogMessage Log.Info 3815 "about."

  describe "Exercise 4" $
    it "construct sorted log messages, expect the first and last message" $ do
      logMessages <- Log.testParse parseMessages 9999 "testdata/error.log"
      let lms = inOrder (build logMessages)
      head lms `shouldBe` Log.LogMessage Log.Info 0 "CHAPTER I. Down the Rabbit-Hole"
      last lms `shouldBe` Log.LogMessage Log.Info 5522 "pci 0000016facpu0Tste"

  describe "Exercise 5" $
    it "expect 9 critical error messages (level >= 50)" $ do
      messages <- Log.testWhatWentWrong (inOrder . build . parseMessages) whatWentWrong "testdata/error.log"
      length messages `shouldBe` 9

main :: IO ()
main = runSpec
