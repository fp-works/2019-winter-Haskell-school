{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Fibonacci
  ( fib
  , fibs1
  , fibs2
  , streamToList
  , streamRepeat
  , streamMap
  , streamFromSeed
  , nats
  , ruler
  , fibs3
  , fibs4
  )
where

-- Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]


-- Exercise 2
iterate2 :: (a -> a -> a) -> a -> a -> [a]
iterate2 f x y = x : y : iterate2 f x' (f y x') where x' = f x y

fibs2 :: [Integer]
fibs2 = iterate2 (+) 0 1

-- Exercise 3
data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Stream x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x xs) = Stream (f x) $ streamMap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream x . streamFromSeed f . f $ x

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

-- Alternates items from two streams
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream x xs) y = Stream x $ interleaveStreams y xs

-- Recursively interleave streams so that each number occurs half as often as the last
-- Requires interleaveStreams to be lazy in the second argument
rulerGen :: Integer -> Stream Integer
rulerGen x = interleaveStreams (streamRepeat x) (rulerGen $ x + 1)

ruler :: Stream Integer
ruler = rulerGen 0

-- Exercise 6

x :: Stream Integer
x = Stream 0 $ Stream 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger n = Stream n $ streamRepeat 0
  negate = streamMap negate
  (Stream a as) + (Stream b bs) = Stream (a + b) (as + bs)
  (Stream a as) * bigB@(Stream b bs) = Stream (a * b) (streamMap (* a) bs + as * bigB)

instance Fractional (Stream Integer) where
  bigA@(Stream a as) / bigB@(Stream b bs) =
    Stream (div a b) (streamMap (`div` b) (as - (bigA / bigB) * bs))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x ^ 2)

-- Exercise 7
data Matrix = Matrix Integer Integer Integer Integer deriving (Show)

instance Num Matrix where
  (Matrix x0 x1 x2 x3) * (Matrix y0 y1 y2 y3) =
    Matrix (x0 * y0 + x1 * y2) (x0 * y1 + x1 * y3) (x2 * y0 + x3 * y2) (x2 * y1 + x3 * y3)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = case Matrix 1 1 1 0 ^ n of
  (Matrix _ _ u _) -> u

fibs4 :: Stream Integer
fibs4 = streamMap fib4 nats
