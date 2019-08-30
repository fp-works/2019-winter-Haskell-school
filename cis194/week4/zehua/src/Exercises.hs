module Exercises where

import Data.Bool (bool)

-- ex1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . fmap (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = foldr (+) 0
      . filter even
      . takeWhile (>1) -- >1 instead of /=1 to defend against negative numbers
      . iterate (bool <$> oddv <*> evenv <*> even)
    where
      oddv = (+1) . (*3)
      evenv = (`div` 2)
