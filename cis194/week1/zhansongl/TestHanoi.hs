
import Hanoi
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "hanoi-3" $ do
    it "returns correct moves with 0 disc" $ do
      (hanoi 0 "a" "b" "c") `shouldBe` []
    it "returns correct moves with 1 disc" $ do
      (hanoi 1 "a" "b" "c") `shouldBe` [("a", "b")]
    it "returns correct moves with 2 disc" $ do
      (hanoi 2 "a" "b" "c") `shouldBe` [("a","c"), ("a","b"), ("c","b")]
    it "returns correct moves with 3 disc" $ do
      (hanoi 3 "a" "b" "c") `shouldBe` [("a", "b"), ("a", "c"), ("b", "c"),
                                        ("a", "b"), ("c", "a"), ("c", "b"),
                                        ("a", "b")]

  describe "hanoi-4" $ do
    it "returns correct moves with 0 disc" $ do
      (hanoi4 0 "a" "b" "c" "d") `shouldBe` []
    it "returns correct moves with 1 disc" $ do
      (hanoi4 1 "a" "b" "c" "d") `shouldBe` [("a", "b")]
    it "returns correct number of moves with 2 disc" $ do
      (length (hanoi4 2 "a" "b" "c" "d")) `shouldBe` 3
    it "returns correct number of moves with 3 disc" $ do
      (length (hanoi4 3 "a" "b" "c" "d")) `shouldBe` 5
    it "returns correct number of moves with 15 disc" $ do
      (length (hanoi4 15 "a" "b" "c" "d")) `shouldBe` 129

