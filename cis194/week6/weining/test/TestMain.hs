
import           Test.Hspec

import           Exercise1
import           Exercise2
import           Exercise3
import           Exercise4
import           Exercise5
import           Stream


main :: IO ()
main = hspec $ do
  describe "Exercise 1" $ do
    it "compute the first 31 fibonacci numbers" $ do
      -- first 31 fib numbers downloaded from
      -- http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Fibonacci/fibtable.html
      let expected = [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89,
                      144, 233, 377, 610, 987, 1597, 2584, 4181,
                      6765, 10946, 17711, 28657, 46368, 75025,
                      121393, 196418, 317811, 514229, 832040]
      map fibs1 [0..30] `shouldBe` expected

  describe "Exercise 2" $ do
    it "compute the first 31 fibonacci numbers in O(n)" $ do
      let expected = [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89,
                      144, 233, 377, 610, 987, 1597, 2584, 4181,
                      6765, 10946, 17711, 28657, 46368, 75025,
                      121393, 196418, 317811, 514229, 832040]
      take 31 fibs2 `shouldBe` expected

  describe "Exercise 3" $ do
    it "create the string representation for a stream" $ do
      show (singletonStream 3) `shouldBe` "[3..]"

  describe "Exercise 4" $ do
    it "implement streamRepeat()" $ do
      (take 3 . streamToList . streamRepeat $ 1) `shouldBe` [1, 1, 1]

    it "implement streamMap()" $ do
      (take 3 . streamToList . (streamMap (> 0)) . streamRepeat $ 1) `shouldBe` [True, True, True]

    it "implement streamFromSeed()" $ do
      (take 3 . streamToList . streamFromSeed (+ 1) $ 1) `shouldBe` [1, 2, 3]

  describe "Exercise 5" $ do
    it "implement nats: all natural numbers starting from 0" $ do
      (take 10 . streamToList $ nats) `shouldBe` [0..9]

    it "implement ruler: the n-th element in the stream is the largest power of 2 which evenly divides n" $ do
      (take 16 . streamToList $ ruler) `shouldBe` [0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4]


