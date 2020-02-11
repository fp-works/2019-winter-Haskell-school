module JoinListSpec where

import           Control.Monad (mapM_)
import           Data.Monoid
import           JoinList
import           Scrabble
import           Sized
import           Test.Hspec

singleProd :: Int -> String -> JoinList (Product Int) String
singleProd p = Single (Product p)

appendProd :: Int
           -> JoinList (Product Int) String
           -> JoinList (Product Int) String
           -> JoinList (Product Int) String
appendProd p = Append (Product p)

singleSize :: Int -> String -> JoinList Size String
singleSize p = Single (Size p)

appendSize :: Int
           -> JoinList Size String
           -> JoinList Size String
           -> JoinList Size String
appendSize p = Append (Size p)

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:_)  !!? 0         = Just x
(_:xs) !!? i         = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

dataEmptyJL :: JoinList Size String
dataEmptyJL = Empty

dataSize1 :: JoinList Size String
dataSize1 = singleSize 1 "abc"

dataSize2 :: JoinList Size String
dataSize2 = appendSize 2 (singleSize 1 "abc") (singleSize 1 "def")

dataSize4 :: JoinList Size String
dataSize4 = appendSize 4
              (singleSize 1 "abc")
              (appendSize 3
                (appendSize 2
                  (singleSize 1 "def")
                  (singleSize 1 "ghi"))
                (singleSize 1 "jkl"))

spec :: Spec
spec = do
  describe "ex1" $ do
    it "works" $ do
      Empty +++ singleProd 1 "a" `shouldBe` singleProd 1 "a"
      singleProd 2 "b" +++ Empty `shouldBe` singleProd 2 "b"
      singleProd 2 "b" +++ singleProd 3 "cde" `shouldBe` appendProd 6 (singleProd 2 "b") (singleProd 3 "cde")

  describe "ex2" $ do
    it "works for indexJ that returns Nothing" $ do
      indexJ 0 (Empty :: JoinList Size String) `shouldBe` Nothing
      indexJ (-1) dataSize1 `shouldBe` Nothing
      indexJ 1 dataSize1 `shouldBe` Nothing
      indexJ 2 dataSize2 `shouldBe` Nothing

    it "works for indexJ" $ do
      mapM_ (\(i, jl) -> indexJ i jl `shouldBe` jlToList jl !!? i)
        [ (0, dataSize1)
        , (0, dataSize2)
        , (1, dataSize2) ]

    it "works for dropJ with empty" $ do
      dropJ (-1) dataEmptyJL `shouldBe` Empty
      dropJ 0 dataEmptyJL `shouldBe` Empty
      dropJ 1 dataEmptyJL `shouldBe` Empty

    it "works for dropJ with size 1" $ do
      dropJ (-1) dataSize1 `shouldBe` dataSize1
      dropJ 0 dataSize1 `shouldBe` dataSize1
      dropJ 1 dataSize1 `shouldBe` Empty
      dropJ 2 dataSize1 `shouldBe` Empty

    it "works for dropJ with size 2" $ do
      dropJ (-2) dataSize2 `shouldBe` dataSize2
      dropJ 0 dataSize2 `shouldBe` dataSize2
      dropJ 1 dataSize2 `shouldBe` singleSize 1 "def"
      dropJ 2 dataSize2 `shouldBe` Empty
      dropJ 3 dataSize2 `shouldBe` Empty

    it "works for dropJ with size 4" $ do
      dropJ (-1) dataSize4 `shouldBe` dataSize4
      dropJ 0 dataSize4 `shouldBe` dataSize4
      dropJ 1 dataSize4 `shouldBe` (appendSize 3
                                     (appendSize 2
                                       (singleSize 1 "def")
                                       (singleSize 1 "ghi"))
                                     (singleSize 1 "jkl"))
      dropJ 2 dataSize4 `shouldBe` (appendSize 2 (singleSize 1 "ghi") (singleSize 1 "jkl"))
      dropJ 3 dataSize4 `shouldBe` (singleSize 1 "jkl")
      dropJ 4 dataSize4 `shouldBe` Empty
      dropJ 5 dataSize4 `shouldBe` Empty

    it "works for takeJ with empty" $ do
      takeJ (-1) dataEmptyJL `shouldBe` Empty
      takeJ 0 dataEmptyJL `shouldBe` Empty
      takeJ 1 dataEmptyJL `shouldBe` Empty

    it "works for takeJ with size 1" $ do
      takeJ (-1) dataSize1 `shouldBe` Empty
      takeJ 0 dataSize1 `shouldBe` Empty
      takeJ 1 dataSize1 `shouldBe` dataSize1
      takeJ 2 dataSize1 `shouldBe` dataSize1

    it "works for takeJ with size 2" $ do
      takeJ (-1) dataSize2 `shouldBe` Empty
      takeJ 0 dataSize2 `shouldBe` Empty
      takeJ 1 dataSize2 `shouldBe` dataSize1
      takeJ 2 dataSize2 `shouldBe` dataSize2
      takeJ 3 dataSize2 `shouldBe` dataSize2

    it "works for takeJ with size 4" $ do
      takeJ (-1) dataSize4 `shouldBe` Empty
      takeJ 0 dataSize4 `shouldBe` Empty
      takeJ 1 dataSize4 `shouldBe` dataSize1
      takeJ 2 dataSize4 `shouldBe` dataSize2
      takeJ 3 dataSize4 `shouldBe`
        (appendSize 3
          (singleSize 1 "abc")
          (appendSize 2
            (singleSize 1 "def")
            (singleSize 1 "ghi")))
      takeJ 4 dataSize4 `shouldBe` dataSize4
      takeJ 5 dataSize4 `shouldBe` dataSize4

  describe "ex3" $ do
    it "works for scoreLine" $ do
      scoreLine "yay " +++ scoreLine "haskell!" `shouldBe`
        Append (Score 23)
          (Single (Score 9) "yay ")
          (Single (Score 14) "haskell!")
