module Exercise1
    ( fibs1
    ) where

fibs1 :: Integer -> Integer
fibs1 0 = 0
fibs1 1 = 1
fibs1 n = fibs1 (n - 1) + fibs1 (n - 2)
