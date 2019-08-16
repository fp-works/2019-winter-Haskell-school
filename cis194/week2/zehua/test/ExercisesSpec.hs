module ExercisesSpec where

import Test.Hspec
import Exercises
import Log

spec :: Spec
spec = do
  describe "ex1.parseMessage" $ do
    it "works with error" $ do
      parseMessage "E 2 562 help help" `shouldBe`
        LogMessage (Error 2) 562 "help help"

    it "parses error with out-of-range severity as unknown" $ do
      parseMessage "E 0 562 help help" `shouldBe`
        Unknown "E 0 562 help help"
      parseMessage "E 101 562 help help" `shouldBe`
        Unknown "E 101 562 help help"

    it "works with warn" $ do
      parseMessage "W 567 lu la la" `shouldBe` LogMessage Warning 567 "lu la la"

    it "works with info" $ do
      parseMessage "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"

    it "works with invalid timestamp" $ do
      parseMessage "W abc This is not in the right format" `shouldBe`
        Unknown "W abc This is not in the right format"

    it "works with unknown" $ do
      parseMessage "This is not in the right format" `shouldBe`
        Unknown "This is not in the right format"

  describe "ex1.parse" $ do
    it "works with sample" $ do
      actual <- testParse parse 11 "testdata/sample.log"
      actual `shouldBe` [
        LogMessage Info 6 "Completed armadillo processing",
        LogMessage Info 1 "Nothing to report",
        LogMessage Info 4 "Everything normal",
        LogMessage Info 11 "Initiating self-destruct sequence",
        LogMessage (Error 70) 3 "Way too many pickles",
        LogMessage (Error 65) 8 "Bad pickle-flange interaction detected",
        LogMessage Warning 5 "Flange is due for a check-up",
        LogMessage Info 7 "Out for lunch, back in two time steps",
        LogMessage (Error 20) 2 "Too many pickles",
        LogMessage Info 9 "Back from lunch",
        LogMessage (Error 99) 10 "Flange failed!"
        ] 

    it "works with first 5 lines of error.log" $ do
      actual <- testParse parse 5 "testdata/error.log"
      actual `shouldBe` [
        LogMessage Info 5053 "pci_id: con ing!",
        LogMessage Info 4681 "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)",
        LogMessage Warning 3654 "e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled",
        LogMessage Info 4076 "verse.'",
        LogMessage Info 4764 "He trusts to you to set them free,"
        ]
