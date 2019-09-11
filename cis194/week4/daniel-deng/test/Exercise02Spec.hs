module CIS194.Homework04.Exercise02Spec where

import CIS194.Homework04.Exercise02

import Text.Printf

import Test.Tasty.Hspec

balanced :: Tree a -> Bool
balanced Leaf = True
balanced (Node _ l _ r)
  | abs (ld - rd) <= 1 = balanced l && balanced r
  | otherwise          = False
  where
    ld = depth l
    rd = depth r

t :: String -> Integer -> Spec
t input expectedDepth = do

  it (printf "returns a Tree with a depth of %s for the input %s" (show expectedDepth) (show input)) $
    depth (foldTree input) `shouldBe` expectedDepth

  it (printf "returns a balanced Tree for the input %s" $ show input) $
    balanced (foldTree input) `shouldBe` True

t' :: String -> Tree Char -> Spec
t' input expectedTree =

  it (printf "returns %s for the input %s" (show expectedTree) (show input)) $
    foldTree input `shouldBe` expectedTree

spec_foldTree :: Spec
spec_foldTree = do

  t' ""      Leaf
  t' "a"   $ Node 0 Leaf 'a' Leaf
  t' "ab"  $ Node 1 (Node 0 Leaf 'a' Leaf) 'b' Leaf
  t' "abc" $ Node 1 (Node 0 Leaf 'b' Leaf) 'c' (Node 0 Leaf 'a' Leaf)

  t ""         (-1)
  t "a"        0
  t ['a'..'b'] 1
  t ['a'..'c'] 1
  t ['a'..'j'] 3
  t ['a'..'z'] 5
