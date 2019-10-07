module JoinListSpec where

import           JoinList
import           Scrabble
import           Sized
import           Data.Monoid                    ( Sum(..) )

import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "+++" $ do
    it "appends empty" $ (Empty +++ Empty :: JoinList (Sum Integer) Integer) `shouldBe` Empty

    it "appends single" $ Single (Sum 10) 1 +++ Single (Sum 20) 2 `shouldBe` Append
      (Sum 30)
      (Single (Sum 10) 1)
      (Single (Sum 20) 2)

    it "appends tree"
      $          Single (Sum 10) 1
      +++        Append (Sum 30) (Single (Sum 10) 1) (Single (Sum 20) 2)
      `shouldBe` Append (Sum 40)
                        (Single (Sum 10) 1)
                        (Append (Sum 30) (Single (Sum 10) 1) (Single (Sum 20) 2))

  describe "indexJ" $ do
    it "retrieves item correctly"
      $          indexJ 2 (Single (Size 1) "a" +++ Single (Size 1) "b" +++ Single (Size 1) "c")
      `shouldBe` Just "c"

  describe "dropJ" $ do
    it "drops items correctly"
      $          dropJ 2 (Single (Size 1) "a" +++ Single (Size 1) "b" +++ Single (Size 1) "c")
      `shouldBe` Single 1 "c"

  describe "takeJ" $ do
    it "takes items correctly"
      $          takeJ 2 (Single (Size 1) "a" +++ Single (Size 1) "b" +++ Single (Size 1) "c")
      `shouldBe` Single (Size 1) "a"
      +++        Single (Size 1) "b"

  describe "scoreLine" $ do
    it "creates tree with score correctly"
      $          scoreLine "yay "
      +++        scoreLine "haskell!"
      `shouldBe` Append (Score 23) (Single (Score 9) "yay ") (Single (Score 14) "haskell!")
