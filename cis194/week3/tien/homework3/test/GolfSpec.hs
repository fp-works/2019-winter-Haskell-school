import           Test.Hspec

import           Golf

main :: IO ()
main =
  hspec $ do
    describe "Golf" $
  -- Exercise 1 --
     do
      it "should return correct result for skip" $ do
        skips ([] :: [Int]) `shouldBe` []
        skips [1] `shouldBe` [[1]]
        skips [True, False] `shouldBe` [[True, False], [False]]
        skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]
        skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
  -- Exercise 2 --
      it "should return local maximum(s)" $ do
        localMaxima [2, 9, 5, 6, 1] `shouldBe` [9, 6]
        localMaxima [2, 3, 4, 1, 5] `shouldBe` [4]
        localMaxima [1, 2, 3, 4, 5] `shouldBe` []
-- Exercise 3 --
      it "should return correct histogram" $ do
        histogram [3, 5] `shouldBe` "   * *    \n==========\n0123456789\n"
        histogram [1, 1, 1, 5] `shouldBe`
          " *        \n *        \n *   *    \n==========\n0123456789\n"
