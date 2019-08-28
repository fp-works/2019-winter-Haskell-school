module GolfSpec where
import Golf
import Test.Hspec
import Data.List.HT

spec :: Spec
spec =
  describe "Golf" $ do
   skipsSpec

skipsSpec :: Spec
skipsSpec = do
   it "returns correct" $ do
     skips "ABCD"  `shouldBe`  ["ABCD", "BD", "C", "D"]
     skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
     skips [1] `shouldBe` [[1]]
     skips [True,False] `shouldBe` [[True, False], [False]]

