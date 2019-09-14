{-# OPTIONS_GHC -Wall #-}

module Wholemeal where

import Data.Bool (bool)
import qualified Data.Array as A

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (*) 1 . map (flip (-) 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate next
  where next n = bool (3 * n + 1) (n `div` 2) $ even n


data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

getHeight :: Tree a -> Integer
getHeight Leaf = (-1)
getHeight (Node h _ _ _) = h

insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node _ left y right)
  | getHeight left < getHeight right =
      let h' = 1 + max (getHeight left') (getHeight right)
          left' = insert x left
      in  Node h' left' y right
  | otherwise =
      let h' = 1 + max (getHeight left) (getHeight right')
          right' = insert x right
      in  Node h' left y right'

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

xor :: [Bool] -> Bool
xor = odd . length . filter id

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x ys -> (f x):ys) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x g z -> g $ f z x) id xs $ base

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
  map (\x -> 2 * x + 1)
  . map fst
  . filter snd
  . A.assocs
  . (A.//) (A.listArray (1,n) (repeat True))
  . flip zip (repeat False)
  . filter (<= n) -- indices to delete
  . map (\(i, j) -> i + j + 2 * i * j)
  $ cartProd [1..(n `div` 2)] [1..(n `div` 2)]

