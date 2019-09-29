{-# LANGUAGE BlockArguments #-}
module JoinListSpec where
import JoinList
import Test.Hspec
import Sized

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

type JL = JoinList Size Int

spec :: Spec
spec = do
  describe "(+++)" do
    it "Empty +++ a" do
      Single [1] 1 +++ Empty `shouldBe` Single [1] 1
    it "a +++ Empty" do
      Empty +++ Single [1] 1 `shouldBe` Single [1] 1
    it "a +++ b" do
      Single [2] 4 +++ Single [1] 1 `shouldBe` Append [2, 1] (Single [2] 4) (Single [1] 1)
      Append [2, 1] (Single [2] 4) (Single [1] 1) +++ Single [3] 1 `shouldBe` Append [2, 1, 3] (Append [2, 1] (Single [2] 4) (Single [1] 1)) (Single [3] 1)
      
  describe "tag" do
    it "tag Empty" do
      tag (Empty :: JoinList [Int] Int) `shouldBe` []
    it "tag Single" do
      tag (Single [0] 3) `shouldBe` [0]
    it "tag Append" do
      tag (Append [1] Empty Empty) `shouldBe` [1]

  describe "indexJ" do
    it "indexJ Empty" do
      let jl = (Empty :: JL)
      indexJ 0 jl `shouldBe` jlToList jl !!? 0
    it "indexJ Single" do
      let jl = Single (Size 1) 3
      indexJ 0 jl `shouldBe` jlToList jl !!? 0
    it "indexJ Append" do
      let jl = Append (Size 2) (Single (Size 1) 3) (Single (Size 1) 4)
      indexJ 0 jl `shouldBe` jlToList jl !!? 0
      indexJ 1 jl `shouldBe` jlToList jl !!? 1
      indexJ 2 jl `shouldBe` jlToList jl !!? 2
  
  describe "dropJ" do
    it "dropJ Empty" do
      let jl = (Empty :: JL)
      jlToList (dropJ 0 jl) `shouldBe` drop 0 (jlToList jl)
    it "dropJ Single" do
      let jl = Single (Size 1) 3
      jlToList (dropJ 0 jl) `shouldBe` drop 0 (jlToList jl)
      jlToList (dropJ 1 jl) `shouldBe` drop 1 (jlToList jl)
    it "dropJ Append" do
      let jl = Append (Size 2) (Single (Size 1) 3) (Single (Size 1) 4)
      jlToList (dropJ 0 jl) `shouldBe` drop 0 (jlToList jl)
      jlToList (dropJ 1 jl) `shouldBe` drop 1 (jlToList jl)
      jlToList (dropJ 2 jl) `shouldBe` drop 2 (jlToList jl)
      jlToList (dropJ 3 jl) `shouldBe` drop 3 (jlToList jl)
      
  describe "takeJ" do
    it "takeJ Empty" do
      let jl = (Empty :: JL)
      jlToList (takeJ 0 jl) `shouldBe` take 0 (jlToList jl)
    it "takeJ Single" do
      let jl = Single (Size 1) 3
      jlToList (takeJ 0 jl) `shouldBe` take 0 (jlToList jl)
    it "takeJ Append" do
      let jl = Append (Size 2) (Single (Size 1) 3) (Single (Size 1) 4)
      jlToList (takeJ 0 jl) `shouldBe` take 0 (jlToList jl)
      jlToList (takeJ 1 jl) `shouldBe` take 1 (jlToList jl)      
      jlToList (takeJ 2 jl) `shouldBe` take 2 (jlToList jl)      
      jlToList (takeJ 3 jl) `shouldBe` take 3 (jlToList jl)      
      