module Exercise5Spec where

import Test.Hspec
import Exercise5

spec :: Spec
spec = do
  describe "hanoi" $ do
    it "works with 1 disk" $ do
      hanoi 1 "a" "b" "c" `shouldBe` [("a","b")]

    it "works with 2 disks" $ do
      hanoi 2 "a" "b" "c" `shouldBe` [("a","c"), ("a","b"), ("c","b")]

    it "works with 3 disks" $ do
      hanoi 3 "a" "b" "c" `shouldBe`
        [("a","b"),("a","c"),("b","c"),("a","b"),("c","a"),("c","b"),("a","b")]
