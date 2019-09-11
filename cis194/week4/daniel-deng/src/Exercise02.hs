module CIS194.Homework04.Exercise02 (Tree(..), depth, foldTree) where

import Data.Bool

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

depth :: Tree a -> Integer
depth Leaf           = -1
depth (Node h _ _ _) = h

addElement :: a -> Tree a -> Tree a
addElement newElement Leaf = Node 0 Leaf newElement Leaf
addElement newElement (Node d l e r)
  | lDeeper   = Node d l e newR
  | otherwise = Node newD newL e r
  where
    lDeeper   = depth l > depth r
    newL      = addElement newElement l
    newR      = addElement newElement r
    newD      = bool d (d + 1) goDeeper
    goDeeper  = depth l < depth newL

foldTree :: Foldable f => f a -> Tree a
foldTree = foldr addElement Leaf
