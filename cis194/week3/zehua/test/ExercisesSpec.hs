module ExercisesSpec where

import           Exercises
import           Test.Hspec


spec :: Spec
spec = do
  describe "ex1" $ do
    it "works with empty" $ do
      skips ([] :: [Int]) `shouldBe` []

    it "works with non empty" $ do
      skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]
      skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
      skips ([1] :: [Int])`shouldBe` [[1]]
      skips ([1,2,3,4,5,6] :: [Int]) `shouldBe` [[1,2,3,4,5,6],[2,4,6],[3,6],[4],[5],[6]]
      skips [True,False] `shouldBe` [[True,False], [False]]

  describe "ex2" $ do
    it "returns empty given empty" $ do
      localMaxima ([] :: [Integer]) `shouldBe` []

    it "returns empty given 1 item" $ do
      localMaxima ([1] :: [Integer]) `shouldBe` []

    it "returns empty given 2 items" $ do
      localMaxima ([1, 2] :: [Integer]) `shouldBe` []

    it "does not return items on either end" $ do
      localMaxima ([1, 2, 3, 4] :: [Integer]) `shouldBe` []

    it "works with 1 maxima" $ do
      localMaxima [1, 2, 3, 4, 3, 2] `shouldBe` [4]

    it "works with multiple maximas" $ do
      localMaxima ([1, 2, 3, 2, 3, 2] :: [Integer]) `shouldBe` [3, 3]
      localMaxima ([1, 2, 4, 2, 3, 2, 4, 5, 1] :: [Integer]) `shouldBe` [4, 3, 5]

  describe "ex3" $ do
      it "returns empty given empty" $ do
        histogram [] `shouldBe` expected []

      it "handles 1 count" $ do
        histogram [3,5] `shouldBe` expected [ "   * *    " ]

      it "handles 3 count" $ do
        histogram [1,1,1,5] `shouldBe`
          expected [ " *        "
                   , " *        "
                   , " *   *    " ]

      it "handles counts" $ do
        histogram  [1,4,5,4,6,6,3,4,2,4,9] `shouldBe`
          expected [ "    *     "
                   , "    *     "
                   , "    * *   "
                   , " ******  *" ]
    where
      expected l = unlines l ++ "==========\n0123456789\n"
