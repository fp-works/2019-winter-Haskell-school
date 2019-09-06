module Week4Spec where

  import           Week4

  import           Test.Hspec

  main :: IO ()
  main = hspec $ do
    describe "fun1" $ do
      it "should return 1 for empty list" $ fun1 [] `shouldBe` 1

      it "should return 1 for list of odd numbers" $ fun1 [1,3,5,7,9] `shouldBe` 1

      it "should return 0 for list with 2" $ fun1 [1,4,7,6,2,4,6,8,9] `shouldBe` 0

      it "should return product of even numbers minus 2" $ fun1 [1,4,7,6,4,6,8,9] `shouldBe` 384

    describe "fun2" $ do
      it "should return 0 for 1" $ fun2 1 `shouldBe` 0
      it "should return 2 for 2" $ fun2 2 `shouldBe` 2
      it "should return 40 for 3" $ fun2 3 `shouldBe` 40
      it "should return 46 for 6" $ fun2 6 `shouldBe` 46
      it "should return 234 for 7" $ fun2 7 `shouldBe` 234

    describe "foldTree" $ do
      it "should return Leaf for an empty list" $ foldTree "" `shouldBe` Leaf

      it "should return single Node for single item" $ foldTree "A" `shouldBe` Node 0 Leaf 'A' Leaf

    describe "xor" $ do
      it "should return False for an empty list" $ xor [] `shouldBe` False
      it "should return True for a single True" $ xor [True] `shouldBe` True
      it "should return False for two True" $ xor [True, True] `shouldBe` False
      it "should return True for three True" $ xor [True, True, True] `shouldBe` True
      it "should ignore False values" $ xor [False, True, False, True, True, False] `shouldBe` True

    describe "myFoldl" $
      it "should be same as foldl" $
        myFoldl (flip (:)) [] "abc" `shouldBe` "cba"

    describe "sieveSundaram" $
      it "should be correct for n=50" $
        sieveSundaram 50 `shouldBe` [3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101]
