module FibonacciSpec where

  import qualified Data.Map                      as M
  import           Fibonacci

  import           Test.Hspec

  main :: IO ()
  main = hspec $ do
    describe "fibs2" $
      it "is correct for the first ten numbers" $
        take 10 fibs2 `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]

    describe "ruler" $
      it "is correct for the first ten numbers" $
        (take 16 . streamToList $ ruler)
        `shouldBe` [0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4]

    describe "fibs3" $
      it "is correct for the first ten numbers" $
        (take 10 . streamToList $ fibs3) `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]

    describe "fibs4" $
      it "is correct for the first ten numbers" $
        (take 10 . streamToList $ fibs4) `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
