{-# LANGUAGE BlockArguments #-}
module PartyTest where
import Test.Tasty.Hspec
import Party
import Employee
import Data.Tree

getFun :: GuestList -> Fun
getFun (GL _ f) = f

spec_party :: Spec
spec_party = do
  describe "glCons" do
    it "glCons mempty" do
      glCons (Emp "Steve" 10) (GL [] 0) `shouldBe` (GL [Emp "Steve" 10] 10)
    it "glCons a" do
      glCons (Emp "Steve" 10) (GL [Emp "Anna" 1] 1) `shouldBe` (GL [Emp "Anna" 1, Emp "Steve" 10] 11)
      
  describe "instance Semigroup GuestList" do
    it "(<>)" do
      GL [Emp "Anna" 1] 1 <> GL [Emp "Steve" 10] 10 `shouldBe` (GL [Emp "Anna" 1, Emp "Steve" 10] 11)
    
  describe "instance Monoid GuestList" do
    it "mempty" do
      GL [Emp "Steve" 10] 10 <> mempty `shouldBe` GL [Emp "Steve" 10] 10
      mempty <> GL [Emp "Steve" 10] 10 `shouldBe` GL [Emp "Steve" 10] 10

  describe "moreFun" do
    it "f1 > f2" do
      moreFun (GL [Emp "Steve" 10] 10) (GL [Emp "Anna" 1] 1) `shouldBe` (GL [Emp "Steve" 10] 10)
    it "f1 < f2" do
      moreFun (GL [Emp "Anna" 1] 1) (GL [Emp "Steve" 10] 10) `shouldBe` (GL [Emp "Steve" 10] 10)
      
  describe "treeFold" do
    it "sums the values in a tree" do
      foldTree (\x xs -> sum (x:xs)) (Node 1 [Node 2 [], Node 3 []]) `shouldBe` 6
      foldTree (\x xs -> sum (x:xs)) (Node 1 [Node 2 [Node 7 [Node 2 [], Node 4 []]], Node 2 []]) `shouldBe` 18
    it "finds the maximum value in the tree" do
      foldTree (\x xs -> maximum (x:xs)) (Node 1 [Node 2 [], Node 3 []]) `shouldBe` 3
    it "counts the number of leaves in the tree" do
      foldTree (\_ xs -> if null xs then 1 else sum xs) (Node 1 [Node 2 [], Node 3 []]) `shouldBe` 2
    it "finds depth of the tree; i.e. the number of branches from the root of the tree to the furthest leaf" do
      foldTree (\_ xs -> if null xs then 0 else 1 + maximum xs) (Node 1 [Node 2[], Node 3 []]) `shouldBe` 1

  describe "maxFun" do
    it "testCompany" do
      (getFun . maxFun $ testCompany) `shouldBe` 26
      (getFun . maxFun $ testCompany2) `shouldBe` 26
