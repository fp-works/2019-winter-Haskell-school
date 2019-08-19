module ExercisesSpec where

import           Data.List  (sort)
import           Exercises
import           Log
import           Test.Hspec

-- util to make LogMessage sortable.
-- Using `newtype` to avoid having to touch the original def in Log.hs
newtype SortableLogMessage = SortableLogMessage { getLogMessage :: LogMessage } deriving (Eq, Show)
instance Ord SortableLogMessage where
  a1 <= a2 = let b1 = getLogMessage a1
                 b2 = getLogMessage a2
             in b1 `lteq` b2
               where
                 Unknown _ `lteq` _         = True
                 _ `lteq` Unknown _         = False
                 LogMessage _ ts1 _ `lteq` LogMessage _ ts2 _ = ts1 <= ts2

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

  describe "ex2" $ do
    it "returns original when given unknown" $ do
      insert (Unknown "unknown msg") Leaf `shouldBe` Leaf
      insert (Unknown "unknown msg") (Node Leaf (LogMessage Info 123 "info") Leaf)
        `shouldBe` (Node Leaf (LogMessage Info 123 "info") Leaf)

    it "handles leaf node" $ do
      insert (LogMessage Warning 123 "warn") Leaf `shouldBe`
        (Node Leaf (LogMessage Warning 123 "warn") Leaf)

    it "inserts to left when smaller" $ do
      insert (LogMessage Warning 123 "warn") (Node Leaf (LogMessage Info 124 "info") Leaf)`shouldBe`
        (Node (Node Leaf (LogMessage Warning 123 "warn") Leaf) (LogMessage Info 124 "info") Leaf)

    it "inserts to right when equal or greater" $ do
      insert (LogMessage Warning 123 "warn") (Node Leaf (LogMessage Info 122 "info") Leaf)`shouldBe`
        (Node Leaf (LogMessage Info 122 "info") (Node Leaf (LogMessage Warning 123 "warn") Leaf))

      insert (LogMessage Warning 123 "warn") (Node Leaf (LogMessage Info 123 "info") Leaf)`shouldBe`
        (Node Leaf (LogMessage Info 123 "info") (Node Leaf (LogMessage Warning 123 "warn") Leaf))

  describe "ex3" $ do
    it "returns leaf for empty list" $ do
      build [] `shouldBe` Leaf

    it "handles list" $ do
      build
          [ (LogMessage Warning 124 "third")
          , (LogMessage Info 122 "first")
          , (LogMessage (Error 1) 123 "second")
          , (LogMessage (Error 2) 125 "fourth") ]
        `shouldBe`
          Node
            (Node
              (Node Leaf (LogMessage Info 122 "first") Leaf)
              (LogMessage (Error 1) 123 "second")
              (Node Leaf (LogMessage Warning 124 "third") Leaf))
            (LogMessage (Error 2) 125 "fourth")
            Leaf

  describe "ex4" $ do
    it "returns empty for a leaf" $ do
      inOrder Leaf `shouldBe` []

    it "handles node" $ do
      inOrder (Node
                Leaf
                (LogMessage Info 122 "first")
                (Node
                  (Node Leaf (LogMessage (Error 1) 123 "second") Leaf)
                  (LogMessage Warning 124 "third")
                  (Node Leaf (LogMessage (Error 2) 125 "fourth") Leaf)))
        `shouldBe` [ (LogMessage Info 122 "first")
                   , (LogMessage (Error 1) 123 "second")
                   , (LogMessage Warning 124 "third")
                   , (LogMessage (Error 2) 125 "fourth") ]

    it "combines with build should be equivalent to sort without Unknown" $ do
      let input  =
              [ (LogMessage Warning 124 "third")
              , (LogMessage Info 122 "first")
              , (LogMessage (Error 1) 123 "second")
              , (LogMessage (Error 2) 125 "fourth") ]
          sortLogMessages :: [LogMessage] -> [LogMessage]
          sortLogMessages = fmap getLogMessage . sort . fmap SortableLogMessage
      (inOrder . build) input `shouldBe` sortLogMessages input

  describe "ex5" $ do
    it "handles empty list" $ do
      whatWentWrong [] `shouldBe` []

    it "filters not qualified logs" $ do
      whatWentWrong [ (LogMessage Warning 122 "warn") ] `shouldBe` []
      whatWentWrong [ (LogMessage Info 122 "info") ] `shouldBe` []
      whatWentWrong [ (LogMessage (Error 49) 122 "error 49") ] `shouldBe` []

    it "keeps logs when error >= 50" $ do
      whatWentWrong
          [ (LogMessage Warning 122 "warn")
          , (LogMessage (Error 50) 122 "error 50")
          , (LogMessage Info 122 "info")
          , (LogMessage (Error 49) 122 "error 49") ]
        `shouldBe` [ "error 50" ]
      whatWentWrong
          [ (LogMessage Warning 121 "warn")
          , (LogMessage (Error 51) 122 "area 51")
          , (LogMessage Info 120 "info")
          , (LogMessage (Error 49) 123 "error 49") ]
        `shouldBe` [ "area 51" ]

    it "filters and sorts logs when error >= 50" $ do
      whatWentWrong
          [ (LogMessage (Error 100) 123 "error 100")
          , (LogMessage Warning 122 "warn")
          , (LogMessage (Error 51) 122 "area 51")
          , (LogMessage Info 122 "info")
          , (LogMessage (Error 99) 125 "error 99")
          , (LogMessage (Error 49) 122 "error 49")
          , (LogMessage (Error 51) 124 "error 75") ]
        `shouldBe`
          [ "area 51"
          , "error 100"
          , "error 75"
          , "error 99" ]

    it "parses from sample.log" $ do
      s <- testWhatWentWrong parse whatWentWrong "testdata/sample.log"
      s `shouldBe`
        [ "Way too many pickles"
        , "Bad pickle-flange interaction detected"
        , "Flange failed!" ]
    it "parses from error.log" $ do
      s <- testWhatWentWrong parse whatWentWrong "testdata/error.log"
      take 5 s `shouldBe`
        [ "Mustardwatch opened, please close for proper functioning!"
        , "All backup mustardwatches are busy"
        , "Depletion of mustard stores detected!"
        , "Hard drive failure: insufficient mustard"
        , "All backup mustardwatches are busy" ]

  describe "ex6" $ do
    it "returns empty when event is the first one" $ do
      findEventsBefore "warn" [ (LogMessage Warning 122 "warn") ] `shouldBe` []

    it "returns when event is the last" $ do
      findEventsBefore "error 49"
          [ (LogMessage Warning 123 "warn")
          , (LogMessage (Error 50) 124 "error 50")
          , (LogMessage (Error 49) 125 "error 49")
          , (LogMessage Info 122 "info") ]
        `shouldBe`
          [ (LogMessage Info 122 "info")
          , (LogMessage Warning 123 "warn")
          , (LogMessage (Error 50) 124 "error 50") ]
