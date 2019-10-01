module CIS194.Homework07.Exercise04Spec where

import CIS194.Homework07.Buffer
import CIS194.Homework07.Exercise04

import Test.Tasty.Hspec

t1 :: String
t1 = "yay "

t2 :: String
t2 = "haskell!"

t3 :: String
t3 = "The quick brown fox jumps over the lazy dog"

text :: [String]
text = [t1, t2, t3]

mkBuffer :: [String] -> JoinListBuffer
mkBuffer = fromString . unlines

buffer :: JoinListBuffer
buffer = mkBuffer text

spec_toString :: Spec
spec_toString =

  it "returns the correct result" $
    toString buffer `shouldBe` unlines text

spec_line :: Spec
spec_line =

  it "returns the correct result" $ do
    line (-1) buffer `shouldBe` Nothing
    line 0 buffer `shouldBe` Just "yay "
    line 1 buffer `shouldBe` Just "haskell!"
    line 2 buffer `shouldBe` Just "The quick brown fox jumps over the lazy dog"
    line 3 buffer `shouldBe` Nothing

spec_replaceLine :: Spec
spec_replaceLine = do

  context "when n is out of bounds" $
    it "returns the original JoinListBuffer" $ do
      replaceLine (-1) "foo" buffer `shouldBe` buffer
      replaceLine 1000 "foo" buffer `shouldBe` buffer

  context "when n is valid for the given JoinListBuffer" $
    it "returns the correct JoinListBuffer" $ do
      (toString . replaceLine 0 "foo" $ buffer) `shouldBe` unlines ["foo", t2, t3]
      (toString . replaceLine 1 "foo" $ buffer) `shouldBe` unlines [t1, "foo", t3]
      (toString . replaceLine 2 "foo" $ buffer) `shouldBe` unlines [t1, t2, "foo"]

spec_numLines :: Spec
spec_numLines =

  it "returns the correct result" $ do
    numLines buffer `shouldBe` 3
    (numLines . mkBuffer $ []) `shouldBe` 0
    (numLines . mkBuffer $ ["a", "b", "c", "d", "e"]) `shouldBe` 5

spec_value :: Spec
spec_value =

  it "returns the correct result" $ do
    value buffer `shouldBe` 122
    (value . mkBuffer $ []) `shouldBe` 0
    (value . mkBuffer $ ["a", "b", "c", "d", "e"]) `shouldBe` 10
