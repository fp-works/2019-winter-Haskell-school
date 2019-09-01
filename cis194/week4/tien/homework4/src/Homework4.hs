module Homework4 where

import           Data.Bool

-- Exercise 1 --
-- 1.1 --
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . (subtract 2 <$>) . filter even

-- 1.2 --
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (> 1) . iterate configInputs
  where
    configInputs = bool <$> (+ 1) . (* 3) <*> flip div 2 <*> even

-- Exercise 2 --
data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)
