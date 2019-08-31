module GolfSpec where
import Golf
import Test.Hspec
import Data.List.HT

spec :: Spec
spec =
  describe "Golf" $ do
   skipsSpec
   localMaximaSpec

skipsSpec :: Spec
skipsSpec = do
   it "returns correct" $ do
     skips "ABCD"  `shouldBe`  ["ABCD", "BD", "C", "D"]
     skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
     skips [1] `shouldBe` [[1]]
     skips [True,False] `shouldBe` [[True, False], [False]]


localMaximaSpec:: Spec
localMaximaSpec = do
   it "returns correct" $ do
     localMaxima [2,9,5,6,1] `shouldBe` [9,6]
     localMaxima [2,3,4,1,5] `shouldBe` [4]
     localMaxima [1,2,3,4,5] `shouldBe` []

