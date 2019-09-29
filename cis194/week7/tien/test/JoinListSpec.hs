import Data.Monoid
import Test.Hspec

import JoinList
import Scrabble
import Sized

main :: IO ()
main =
  hspec $ do
    describe "JoinList" $ do
      describe "exercise 1" $ do
        it "should return correct JoinList" $ do
          let jl1 = Single (Product 5) 'e'
          let jl2 =
                Append
                  (Product 30)
                  (Single (Product 10) 'g')
                  (Single (Product 3) 'h')
          jl1 +++ jl2 `shouldBe` (Append (Product 150) jl1 jl2)
      describe "exercise 2" $ do
        let jl1 =
              Append
                (Size 4)
                (Append
                   (Size 3)
                   (Single (Size 1) 'a')
                   (Append (Size 2) (Single (Size 1) 'b') (Single (Size 1) 'c')))
                (Single (Size 1) 'd')
        let jl2 = Append (Size 2) (Single (Size 1) 'e') (Single (Size 1) 'f')
        let jl3 = Append (Size 6) jl1 jl2
        describe "2.1" $ do
          it "should return same result as via jlToList and !!?" $ do
            indexJ 1 jl3 `shouldBe` (jlToList jl3 !!? 1)
            indexJ 3 jl3 `shouldBe` (jlToList jl3 !!? 3)
            indexJ 10 jl3 `shouldBe` (jlToList jl3 !!? 10)
        describe "2.2" $ do
          it "should return the same result via normal drop" $ do
            jlToList (dropJ 0 jl3) `shouldBe` drop 0 (jlToList jl3)
            jlToList (dropJ 1 jl3) `shouldBe` drop 1 (jlToList jl3)
            jlToList (dropJ 2 jl3) `shouldBe` drop 2 (jlToList jl3)
            jlToList (dropJ 10 jl3) `shouldBe` drop 10 (jlToList jl3)
        describe "2.3" $ do
          it "should return the same result via normal take" $ do
            jlToList (takeJ 0 jl3) `shouldBe` take 0 (jlToList jl3)
            jlToList (takeJ 1 jl3) `shouldBe` take 1 (jlToList jl3)
            jlToList (takeJ 3 jl3) `shouldBe` take 3 (jlToList jl3)
            jlToList (takeJ 8 jl3) `shouldBe` take 8 (jlToList jl3)
      describe "exercise 3" $ do
        it "should return a correct score joinlist" $ do
          let result =
                Append
                  (Score 23)
                  (Single (Score 9) "yay ")
                  (Single (Score 14) "haskell!")
          scoreLine "yay " +++ scoreLine "haskell!" `shouldBe` result
