module CIS194.Homework06.Exercise01 (fib, fibs1) where

fib :: Integer -> Integer
fib n
  | n <= 0    = 0
  | n == 1    = 1
  | otherwise = fib (n - 2) + fib (n - 1)

fibs1 :: [Integer]
fibs1 = fib <$> [0..]
