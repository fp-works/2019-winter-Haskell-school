module LogAnalysisSpec where

import Test.Hspec
import LogAnalysis
import Log

spec :: Spec
spec = do
  describe "parseMessage" $ do
    it "constructs LogMessage from log" $ do
      parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"
      parseMessage "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"
      parseMessage "This is not in the right format" `shouldBe` Unknown "This is not in the right format"
