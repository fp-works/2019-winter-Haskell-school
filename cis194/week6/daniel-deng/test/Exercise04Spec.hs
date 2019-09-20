module CIS194.Homework06.Exercise04Spec where

import CIS194.Homework06.TestHelpers (streamTake)
import CIS194.Homework06.Exercise04

import Data.Char

import Test.Tasty.Hspec

spec_streamRepeat :: Spec
spec_streamRepeat =

  it "repeats the element" $
    streamTake 10 (streamRepeat 'a') `shouldBe` "aaaaaaaaaa"

spec_streamMap :: Spec
spec_streamMap =

  it "transform the elements of a stream" $
    (streamTake 10 . streamMap toUpper $ streamRepeat 'a') `shouldBe` "AAAAAAAAAA"

spec_streamFromSeed :: Spec
spec_streamFromSeed =

  it "transform the elements of a stream" $
    (streamTake 10 . streamFromSeed (+1) $ (1 :: Int)) `shouldBe` [1..10]
