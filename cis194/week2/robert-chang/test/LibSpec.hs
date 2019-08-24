module LibSpec where
import LogAnalysis 
import Log

import Test.Hspec

spec :: Spec
spec =
  describe "LogAnalysis" $ do
   parseMessageSpec
   insertSpec
   buildSpec
   inOrderSpec
   whatWentWrongSpec

parseMessageSpec :: Spec
parseMessageSpec = do
   it "returns correct Error Level " $ do
      parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"
   it "return correct Info Level " $ do
      parseMessage "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"
   it "return correct Warning Level " $ do
      parseMessage "W 29 la la la" `shouldBe` LogMessage Warning 29 "la la la"
   it "return correct unKnown" $ do
      parseMessage "haha Steve Mao" `shouldBe` Unknown "haha Steve Mao"

insertSpec :: Spec
insertSpec = do
  let tree = Node (Node Leaf (LogMessage Info 29 "la la la") Leaf) (LogMessage Warning 29 "la la la") Leaf 
  it "should insert LogMessage into the tree" $
    insert (LogMessage (Error 2) 562 "help help") Leaf `shouldBe` Node Leaf (LogMessage (Error 2) 562 "help help") Leaf

  it "should insert LogMessage into the left tree when timestamp is smaller" $
    insert (LogMessage (Error 2) 10 "help help") tree `shouldBe` Node (Node (Node Leaf (LogMessage (Error 2) 10 "help help") Leaf) (LogMessage Info 29 "la la la") Leaf) (LogMessage Warning 29 "la la la") Leaf 

  it "should insert LogMessage into the right tree when timestamp is bigger" $
    insert (LogMessage (Error 2) 590 "help help") tree `shouldBe` Node (Node Leaf (LogMessage Info 29 "la la la") Leaf) (LogMessage Warning 29 "la la la") (Node Leaf (LogMessage (Error 2) 590 "help help") Leaf)


buildSpec :: Spec
buildSpec = do
  let list = [ (LogMessage (Error 2) 590 "help help"), (LogMessage Info 29 "la la la") ];
  it "should build the logMessage" $
      build list `shouldBe` Node Leaf (LogMessage Info 29 "la la la") (Node Leaf (LogMessage (Error 2) 590 "help help") Leaf)

inOrderSpec :: Spec
inOrderSpec = do 
  let tree = Node (Node Leaf (LogMessage Info 29 "la la la") Leaf) (LogMessage Warning 29 "la la la") Leaf 
  it "should build the message list" $
    inOrder tree `shouldBe` [LogMessage Info 29 "la la la",  LogMessage Warning 29 "la la la"]

whatWentWrongSpec :: Spec
whatWentWrongSpec = do
  let list = [ (LogMessage (Error 82) 590 "help help"), (LogMessage Info 29 "la la la"), (LogMessage (Error 83) 591 "help help2"), (LogMessage (Error 10) 590 "help help3") ];
  it "should return correct message" $
    whatWentWrong list `shouldBe` ["help help", "help help2"] 
