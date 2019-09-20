module CIS194.Homework06.Exercise05Spec where

import CIS194.Homework06.TestHelpers (streamTake)
import CIS194.Homework06.Exercise05

import Test.Tasty.Hspec

spec_nats :: Spec
spec_nats =

  it "represents natrual numbers" $
    streamTake 1000 nats `shouldBe` [0..999]

spec_ruler :: Spec
spec_ruler =

  it "returns the correct stream" $
    streamTake 16 ruler `shouldBe` [0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4]
