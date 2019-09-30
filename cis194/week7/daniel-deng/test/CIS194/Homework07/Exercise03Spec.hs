module CIS194.Homework07.Exercise03Spec where

import CIS194.Homework07.Scrabble ( Score(..) )
import CIS194.Homework07.Exercise03

import Test.Tasty.Hspec

spec_scoreString :: Spec
spec_scoreString =

  it "produces the expected outputs" $ do
    scoreString "yay " `shouldBe` Score 9
    scoreString "haskell!" `shouldBe` Score 14
    (scoreString "yay " <> scoreString "haskell!") `shouldBe` Score 23
    scoreString "The quick brown fox jumps over the lazy dog" `shouldBe` Score 99
