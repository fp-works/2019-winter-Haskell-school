import           Test.Hspec

import           Golf

runSpec :: IO ()
runSpec = hspec $ do
  describe "Exercise 1" $
    it "skips" $ do
      skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]
      skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
      skips [1] `shouldBe` [[1]]
      skips [True, False] `shouldBe` [[True, False], [False]]
      skips "" `shouldBe` ([] :: [String])

  describe "Exercise 2" $
    it "local-maxima" $ do
      localMaxima [2,9,5,6,1] `shouldBe` [9,6]
      localMaxima [2,3,4,1,5] `shouldBe` [4]
      localMaxima [1,2,3,4,5] `shouldBe` []

  describe "Exercise 3" $
    it "histogram" $ do
      let numbers = [1, 1, 1, 5]
      histogram numbers `shouldBe` unlines [ " *        ",
                                             " *        ",
                                             " *   *    ",
                                             "==========",
                                             "0123456789" ]
      let numbers' = [1,4,5,4,6,6,3,4,2,4,9]
      histogram numbers' `shouldBe` unlines [ "    *     ",
                                              "    *     ",
                                              "    * *   ",
                                              " ******  *",
                                              "==========",
                                              "0123456789" ]

main :: IO ()
main = do
  runSpec
