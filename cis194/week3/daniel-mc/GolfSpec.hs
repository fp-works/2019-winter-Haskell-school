module HomeworkSpec where

import           Golf

import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "skips" $ do
    it "should be empty for empty list" $ skips ([] :: [Int]) `shouldBe` []

    it "should work with nonempty list" $ skips "abcdef"
      `shouldBe` ["abcdef", "bdf", "cf", "d", "e", "f"]

  describe "localMaxima" $ do
    it "should be empty for empty list" $ localMaxima [] `shouldBe` []

    it "should be empty for list length 2" $ localMaxima [3, 5] `shouldBe` []

    it "should return single local maxima" $ localMaxima [1, 3, 2] `shouldBe` [3]

    it "should return multiple local maxima" $ localMaxima [1, 3, 2, 4, 1] `shouldBe` [3, 4]

    it "should not include maxima at boundaries" $ localMaxima [6, 2, 5, 1, 9] `shouldBe` [5]

    it "should require maxima to be strictly greater" $ localMaxima [1, 5, 5, 2] `shouldBe` []

  describe "histogram" $ do
    it "should work" $ histogram [0, 5, 5, 8, 9]
      `shouldBe` "     *    \n*    *  **\n==========\n0123456789\n"
