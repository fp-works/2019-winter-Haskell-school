{-# OPTIONS_GHC -Wall #-}
module Homework where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fn :: Integer -> Integer -> Integer
fn a b 
  | even a = b * (a - 2)
  | otherwise = b

fun1' :: [Integer] -> Integer
fun1' xs = foldr fn 1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fn2 :: Integer -> Integer
fn2 a 
  | even a = a `div` 2
  | otherwise = 3 * a + 1

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (> 1) . iterate fn2

data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)
  
getHeight :: Tree a -> Integer
getHeight Leaf = -1
getHeight (Node height _ _ _) = height

insertTree :: a -> Tree a -> Tree a
insertTree a Leaf = Node 0 Leaf a Leaf
insertTree a (Node height left b right) 
  | getHeight left < getHeight right = Node height left' b right 
  | otherwise = Node height' left b right'
      where height' = getHeight right' + 1
            left' = insertTree a left
            right' = insertTree a right

foldTree :: [a] -> Tree a
foldTree = foldr insertTree Leaf

xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\curr acc -> [f curr] ++ acc) [] xs
