module Week7Spec where
import Data.Monoid
import Test.Hspec
import Sized
import JoinList
import Scrabble

spec :: Spec
spec = do
      describe "exercise 1" $ do
        let l1 = Single (Product 5) 'y'
        let l2 = Append (Product 6) (Single (Product 2) 'e') (Single (Product 3) 'a')
        it "should return correct JoinList" $ do
          l1 +++ l2 `shouldBe` (Append (Product 30) l1 l2)

      describe "exercise 2" $ do
        let l3 = Append (Size 2) (Single (Size 1) 1) (Single (Size 1) 2)
        it "should return correct index" $ do
          indexJ' 0 l3 `shouldBe` jlToList l3 !!? 0
          indexJ' 1 l3 `shouldBe` jlToList l3 !!? 1
          indexJ' 2 l3 `shouldBe` jlToList l3 !!? 2

        it "should drop n" $ do
          jlToList (dropJ 1 l3) `shouldBe` drop 1 (jlToList l3) 

        it "should take n" $ do
          jlToList (takeJ 1 l3) `shouldBe` take 1 (jlToList l3) 

      describe "exercise 3" $ do
        let l3 = Append (Score 23) (Single (Score 9) "yay ") (Single (Score 14) "haskell!")
        it "should scoreLine" $ do  
          scoreLine "yay " +++ scoreLine "haskell!" `shouldBe` l3

