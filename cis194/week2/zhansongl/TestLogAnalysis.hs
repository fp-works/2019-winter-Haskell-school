import LogAnalysis
import Log

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "parseMessage" $ do
    it "can parse entries with empty messages" $ do
      parseMessage "I 99" `shouldBe` (LogMessage Info 99 "")
      parseMessage "W 99" `shouldBe` (LogMessage Warning 99 "")
      parseMessage "E 12 99" `shouldBe` (LogMessage (Error 12) 99 "")
    it "can parse entries with nonempty messages" $ do
      parseMessage "I 99 timer restarted" `shouldBe` (LogMessage Info 99 "timer restarted")
      parseMessage "W 99 it dont matter" `shouldBe` (LogMessage Warning 99 "it dont matter")
      parseMessage "E 12 99 this is not a drill  " `shouldBe` (LogMessage (Error 12) 99 "this is not a drill  ")
    it "can parse random strings" $ do
      let s = "The quick brown fox jumps over the lazy dog"
      parseMessage s `shouldBe` (Unknown s)

  describe "parse" $ do
    it "can parse multiple lines correctly" $ do
      let s = "I 5053 pci_id: con ing!\n\
              \E 47 1034 'What a pity it wouldn't stay!' sighed the Lory, as soon as it was quite\n\
              \W 3654 e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled"
      parse s `shouldBe` [LogMessage Info 5053 "pci_id: con ing!",
                          LogMessage (Error 47) 1034 "'What a pity it wouldn't stay!'\
                                                      \ sighed the Lory, as soon as it was quite",
                          LogMessage Warning 3654 "e8] PGTT ASF! 00f00000003.2: 0x000 - 0000:\
                                                  \ 00009dbfffec00000: Pround/f1743colled"]

  describe "insert" $ do
    it "creates a single node tree with first new message" $ do
      insert (LogMessage Info 10 "") Leaf `shouldBe` (Node Leaf (LogMessage Info 10 "") Leaf)
    it "chooses the left tree when new message comes earlier" $ do
      insert (LogMessage Info 10 "") (Node Leaf (LogMessage Info 11 "b") Leaf) `shouldBe`
        (Node (Node Leaf (LogMessage Info 10 "") Leaf) (LogMessage Info 11 "b") Leaf)
    it "chooses the right tree when new message comes later" $ do
      insert (LogMessage Info 12 "") (Node Leaf (LogMessage Info 11 "b") Leaf) `shouldBe`
        (Node Leaf (LogMessage Info 11 "b") (Node Leaf (LogMessage Info 12 "") Leaf))

  describe "build" $ do
    it "builds an empty tree with no log messages" $ do
      build [] `shouldBe` Leaf
    it "builds a correct search tree" $ property $
      propCorrectTree

  describe "inOrder" $ do
    it "returns a sorted list of message logs" $ property $
      propCorrectInOrder

  describe "whatWentWrong" $ do
    it "returns relevant errors" $ do
      whatWentWrong (parse $ unlines sampleLogs) `shouldBe` relevantSampleLogs

sampleLogs :: [String]
sampleLogs = ["I 6 Completed armadillo processing",
              "I 1 Nothing to report",
              "E 99 10 Flange failed!",
              "I 4 Everything normal",
              "I 11 Initiating self-destruct sequence",
              "E 70 3 Way too many pickles",
              "E 65 8 Bad pickle-flange interaction detected",
              "W 5 Flange is due for a check-up",
              "I 7 Out for lunch, back in two time steps",
              "E 20 2 Too many pickles",
              "I 9 Back from lunch"]

relevantSampleLogs :: [String]
relevantSampleLogs = [ "Way too many pickles", "Bad pickle-flange interaction detected", "Flange failed!"]

propCorrectInOrder :: Property
propCorrectInOrder = forAll genLogs (\xs -> sortedLogs (inOrder $ build xs))

sortedLogs :: [LogMessage] -> Bool
sortedLogs [] = True
sortedLogs [_] = True
sortedLogs (LogMessage _ t1 _ : LogMessage _ t2 _ : ls) =
  (t1 <= t2) && (sortedLogs ls)

propCorrectTree :: Property
propCorrectTree = forAll genLogs (\xs -> validSearchTree $ build xs)

genLogs :: Gen [LogMessage]
genLogs = do
  positives <- listOf (arbitrary :: Gen (Positive Int))
  let timeStamps = map getPositive positives
  return $ map (\t -> LogMessage Info t "") timeStamps

validSearchTree :: MessageTree -> Bool
validSearchTree Leaf = True
validSearchTree (Node left (LogMessage _ tRoot _) right) =
  case (left, right) of
       (Leaf, Leaf) -> True
       (Leaf, Node _ (LogMessage _ tRight _) _) ->
         tRoot <= tRight && validSearchTree right
       (Node _ (LogMessage _ tLeft _) _, Leaf) ->
         tLeft <= tRoot && validSearchTree left
       (Node _ (LogMessage _ tLeft _) _, Node _ (LogMessage _ tRight _) _) ->
         tLeft <= tRoot && tRoot <= tRight && validSearchTree left && validSearchTree right

