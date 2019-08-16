module LibSpec where
import LogAnalysis 
import Log

import Test.Hspec

spec :: Spec
spec = do
  describe "parseMessage" $ do
   it "returns correct Error Level " $ do
      parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"
   it "return correct Info Level " $ do
      parseMessage "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"
   it "return correct Warning Level " $ do
      parseMessage "W 29 la la la" `shouldBe` LogMessage Warning 29 "la la la"
   it "return correct unKnown" $ do
      parseMessage "haha Steve Mao" `shouldBe` Unknown "haha Steve Mao"

  -- I don't know how to test this guy
  -- describe "parse" $ do
  --  it "returns correct result" $ do   
     -- testParse parse 1 "test/sample.log" `shouldBe` LogMessage Info 6 "Completed armadillo processing"
    
