{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module CIS194.Homework07.Exercise02Spec where

import CIS194.Homework07.JoinList ( JoinList(..) )
import CIS194.Homework07.Sized ( Size(..) )
import CIS194.Homework07.Exercise02

import Test.Tasty.Hspec

type TestType = JoinList Size Char

jl0 :: TestType
jl0 = Empty

jl1 :: TestType
jl1 = Single (Size 1) 'a'

jl2 :: TestType
jl2 = Single (Size 1) 'b'

jl3 :: TestType
jl3 = Single (Size 1) 'c'

jl4 :: TestType
jl4 = Single (Size 1) 'd'

jl12 :: TestType
jl12 = jl1 <> jl2

jl34 :: TestType
jl34 = jl3 <> jl4

jl1234 :: TestType
jl1234 = jl12 <> jl34

spec_indexJ :: Spec
spec_indexJ = do

  context "with a negative index" $
    it "returns `Nothing`" $ do
      indexJ (-1) jl0  `shouldBe` Nothing
      indexJ (-1) jl1  `shouldBe` Nothing
      indexJ (-1) jl2  `shouldBe` Nothing
      indexJ (-1) jl12 `shouldBe` Nothing

  context "with a 0 index" $ do
    it "returns `Nothing` for an `Empty` JoinList" $
      indexJ 0 jl0 `shouldBe` Nothing
    it "returns the only element for a `Single` JoinList" $
      indexJ 0 jl1 `shouldBe` Just 'a'
    it "returns the first element for an `Append` JoinList" $ do
      indexJ 0 jl1234 `shouldBe` Just 'a'
      indexJ 1 jl1234 `shouldBe` Just 'b'
      indexJ 2 jl1234 `shouldBe` Just 'c'
      indexJ 3 jl1234 `shouldBe` Just 'd'

  context "with a positive index n" $ do
    it "returns `Nothing` for an `Empty` JoinList" $
      indexJ 1 jl0 `shouldBe` Nothing
    it "returns `Nothing` for a `Single` JoinList" $
      indexJ 1 jl1 `shouldBe` Nothing
    it "returns `Nothing` for an `Append` JoinList if out of bounds" $
      indexJ 2 jl12 `shouldBe` Nothing
    it "returns the nth value in an `Append` JoinList if exists" $
      indexJ 1 jl12 `shouldBe` Just 'b'

spec_dropJ :: Spec
spec_dropJ = do

  context "with a non positive n" $
    it "returns the original JoinList" $ do
      dropJ (-1) jl0 `shouldBe` jl0
      dropJ (-1) jl1 `shouldBe` jl1
      dropJ (-1) jl2 `shouldBe` jl2
      dropJ (-1) jl12 `shouldBe` jl12
      dropJ (-1) jl1234 `shouldBe` jl1234
      dropJ 0 jl0 `shouldBe` jl0
      dropJ 0 jl1 `shouldBe` jl1
      dropJ 0 jl2 `shouldBe` jl2
      dropJ 0 jl12 `shouldBe` jl12
      dropJ 0 jl1234 `shouldBe` jl1234

  context "with a number n that is greater than or equal to the size of the JoinList" $
    it "returns the an `Empty` JoinList" $ do
      dropJ 0 jl0 `shouldBe` Empty
      dropJ 1 jl1 `shouldBe` Empty
      dropJ 1 jl2 `shouldBe` Empty
      dropJ 2 jl12 `shouldBe` Empty

  context "with n >= 1 on a `Single` JoinList" $
    it "returns the an `Empty` JoinList" $ do
      dropJ 1 jl1 `shouldBe` Empty
      dropJ 2 jl1 `shouldBe` Empty

  context "with n <= 0 on a `Single` JoinList" $
    it "returns the original `Single` JoinList" $ do
      dropJ (-1) jl1 `shouldBe` jl1
      dropJ 0 jl2 `shouldBe` jl2

  it "with n on an `Append` JoinList" $ do
    dropJ 0 jl1234 `shouldBe` jl1234
    dropJ 1 jl1234 `shouldBe` jl2 <> jl34
    dropJ 2 jl1234 `shouldBe` jl34
    dropJ 3 jl1234 `shouldBe` jl4
    dropJ 4 jl1234 `shouldBe` Empty
    dropJ 5 jl1234 `shouldBe` Empty

spec_takeJ :: Spec
spec_takeJ = do

  context "with a non positive n" $
    it "returns an `Empty` JoinList" $ do
      takeJ (-1) jl0 `shouldBe` Empty
      takeJ (-1) jl1 `shouldBe` Empty
      takeJ (-1) jl2 `shouldBe` Empty
      takeJ (-1) jl12 `shouldBe` Empty
      takeJ (-1) jl1234 `shouldBe` Empty
      takeJ 0 jl0 `shouldBe` Empty
      takeJ 0 jl1 `shouldBe` Empty
      takeJ 0 jl2 `shouldBe` Empty
      takeJ 0 jl12 `shouldBe` Empty
      takeJ 0 jl1234 `shouldBe` Empty

  context "with a number n that is greater than or equal to the size of the JoinList" $
    it "returns the original JoinList" $ do
      takeJ 1 jl0 `shouldBe` Empty
      takeJ 1 jl1 `shouldBe` jl1
      takeJ 1 jl2 `shouldBe` jl2
      takeJ 2 jl12 `shouldBe` jl12
      takeJ 4 jl1234 `shouldBe` jl1234

  context "with n <= 0 on a `Single` JoinList" $
    it "returns the an `Empty` JoinList" $ do
      takeJ (-1) jl1 `shouldBe` Empty
      takeJ 0 jl1 `shouldBe` Empty
      takeJ (-1) jl12 `shouldBe` Empty
      takeJ 0 jl1234 `shouldBe` Empty

  context "with n > 0 on a `Single` JoinList" $
    it "returns the original `Single` JoinList" $ do
      takeJ 1 jl1 `shouldBe` jl1
      takeJ 8 jl2 `shouldBe` jl2

  it "with n on an `Append` JoinList" $ do
    takeJ 0 jl1234 `shouldBe` Empty
    takeJ 1 jl1234 `shouldBe` jl1
    takeJ 2 jl1234 `shouldBe` jl12
    takeJ 3 jl1234 `shouldBe` jl12 <> jl3
    takeJ 4 jl1234 `shouldBe` jl1234
    takeJ 5 jl1234 `shouldBe` jl1234
