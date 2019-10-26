
module Fibonacci where

import Control.Applicative ((<*>))

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = fmap fib [0..]

fibs2 :: [Integer]
fibs2 = next 0 1
    where next m n = m : next n (m + n)