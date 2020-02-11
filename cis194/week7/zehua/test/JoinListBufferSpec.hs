module JoinListBufferSpec where

import           Buffer
import           Control.Monad   (mapM_)
import           Data.List.Index (setAt)
import           JoinList
import           JoinListBuffer
import           Scrabble
import           Sized
import           StringBuffer
import           Test.Hspec

-- create a Single score size annotated JoinList
sss :: Int -> Int -> String -> JoinList (Score, Size) String
sss s1 s2 = Single ((Score s1), (Size s2))

-- create an Append score size annotated JoinList
ass :: Int -> Int
           -> JoinList (Score, Size) String
           -> JoinList (Score, Size) String
           -> JoinList (Score, Size) String
ass s1 s2 = Append ((Score s1), (Size s2))

dataEmptyJL :: JoinList (Score, Size) String
dataEmptyJL = Empty

dataA2QList :: [String]
dataA2QList = ["abc", "def", "hij", "klm", "opq"]

dataA2QString :: String
dataA2QString = unlines dataA2QList

dataA2QJL :: JoinList (Score, Size) String
dataA2QJL = ass 50 5
            (ass 27 3
              (ass 14 2
                (sss 7 1 "abc")
                (sss 7 1 "def"))
              (sss 13 1 "hij"))
            (ass 23 2
              (sss 9 1 "klm")
              (sss 14 1 "opq"))

infix 1 `shouldBeJLEqualTo`
shouldBeJLEqualTo :: JoinList (Score, Size) String -> JoinList (Score, Size) String -> Expectation
actual `shouldBeJLEqualTo` expected = do
  toLines actual `shouldBe` toLines expected
  getJoinListScore actual `shouldBe` getJoinListScore expected
  getJoinListSize actual `shouldBe` getJoinListSize expected

assertReplaceLineJL :: Int -> String -> Expectation
assertReplaceLineJL n l = replaceLineJL n l dataA2QJL `shouldBeJLEqualTo` (fromLines . setAt n l $ dataA2QList)

infix 1 `shouldReturnTheSameBufferAs`
shouldReturnTheSameBufferAs :: (BufferJoinList -> BufferJoinList) -> (BufferString ->  BufferString) -> Expectation
jlFunc `shouldReturnTheSameBufferAs` strFunc = do
      (toString . jlFunc . fromString $ dataA2QString) `shouldBe`
        (toString . strFunc . fromString $ dataA2QString)

infix 1 `shouldReturnTheSameAs`
shouldReturnTheSameAs :: (Show a, Eq a)  => (BufferJoinList -> a) -> (BufferString ->  a) -> Expectation
jlFunc `shouldReturnTheSameAs` strFunc = do
      (jlFunc . fromString $ dataA2QString) `shouldBe`
        (strFunc . fromString $ dataA2QString)


spec :: Spec
spec = do
  describe "ex4" $ do
    it "works for fromLines" $ do
      fromLines ["abc"] `shouldBe` sss 7 1 "abc"
      fromLines dataA2QList `shouldBe` dataA2QJL

    it "works for toLines" $ do
      toLines dataA2QJL `shouldBe` dataA2QList

    it "works for stringToJL" $ do
      stringToJL "" `shouldBe` dataEmptyJL
      stringToJL dataA2QString `shouldBe` dataA2QJL

    it "works for jlToString" $ do
      jlToString dataEmptyJL `shouldBe` ""
      jlToString dataA2QJL `shouldBe` dataA2QString

    it "works for replaceLineJL" $ do
      replaceLineJL 0 "xyz" dataEmptyJL `shouldBeJLEqualTo` dataEmptyJL
      assertReplaceLineJL 1 ""
      mapM_ (\n -> assertReplaceLineJL n "xyz") [-1..6]

    it "returns the same as StringBuffer" $ do
      toString `shouldReturnTheSameAs` toString
      mapM_ (\n -> line n `shouldReturnTheSameAs` line n) [-1..6]
      mapM_ (\n -> replaceLine n "xyz" `shouldReturnTheSameBufferAs` replaceLine n "xyz") [-1..6]
      numLines `shouldReturnTheSameAs` numLines

