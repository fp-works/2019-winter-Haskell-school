import Golf

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "skips" $ do
    it "should return the the correct lists" $ do
      (skips "ABCD") `shouldBe` ["ABCD", "BD", "C", "D"]
      (skips "hello!") `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
      (skips [1]) `shouldBe` [[1]]
      (skips [True, False]) `shouldBe` [[True,False], [False]]
      (skips ([] :: [Int])) `shouldBe` []

  describe "localMaxima" $ do
    it "should return the nothing for empty list" $ do
      (localMaxima []) `shouldBe` []
    it "should return the correct local maxima for nonempty lists" $ do
      (localMaxima [2,9,5,6,1]) `shouldBe` [9,6]
      (localMaxima [2,3,4,1,5]) `shouldBe` [4]
      (localMaxima [1,2,3,4,5]) `shouldBe` []

  describe "histogram" $ do
    it "should return the correct histogram for empty list" $ do
      lines (histogram []) `shouldBe` ["==========",
                                       "0123456789"]
    it "should return the correct histogram for nonempty lists" $ do
      lines (histogram [1,1,1,5]) `shouldBe` [" *        ",
                                              " *        ",
                                              " *   *    ",
                                              "==========",
                                              "0123456789"]
      lines (histogram [1,4,5,4,6,0,6,3,4,2,4,9]) `shouldBe` ["    *     ",
                                                              "    *     ",
                                                              "    * *   ",
                                                              "*******  *",
                                                              "==========",
                                                              "0123456789"]

