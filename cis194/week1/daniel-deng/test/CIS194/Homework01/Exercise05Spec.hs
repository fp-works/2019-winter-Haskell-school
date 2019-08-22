module CIS194.Homework01.Exercise05Spec where

import CIS194.Homework01.Exercise05

import Test.Tasty.Hspec

spec_hanoi :: Spec
spec_hanoi = do

  it "hanoi with negative disks should take no move" $
    hanoi (-1) "a" "b" "c" `shouldBe` []

  it "hanoi with 0 disks should take no move" $
    hanoi 0 "a" "b" "c" `shouldBe` []

  it "hanoi with 1 disks should take 1 move" $
    hanoi 1 "a" "b" "c" `shouldBe` [("a", "b")]

  it "hanoi with 2 disks should take 3 moves" $
    hanoi 2 "a" "b" "c" `shouldBe` [("a","c"), ("a","b"), ("c","b")]

  it "hanoi with 3 disks should take 7 moves" $
    hanoi 3 "a" "b" "c" `shouldBe` [("a","b"),("a","c"),("b","c"),("a","b"),("c","a"),("c","b"),("a","b")]
