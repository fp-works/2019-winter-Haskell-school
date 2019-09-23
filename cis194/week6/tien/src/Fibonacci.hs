{-# LANGUAGE FlexibleInstances #-}

module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- exercise 1 --
fibs1 :: [Integer]
fibs1 = fib <$> [0 ..]

-- exercise 2 --
fibs2 :: [Integer]
fibs2 = 0 : 1 : next fibs2
  where
    next (x:y:xs) = (x + y) : next (y : xs)

-- exercise 3 --
data Stream a =
  Con a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Con x xs) = x : (streamToList xs)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

-- exercise 4 --
streamRepeat :: a -> Stream a
streamRepeat = Con <*> streamRepeat

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Con x xs) = Con (f x) . streamMap f $ xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Con x . streamFromSeed f . f $ x

-- exercise 5 --
nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Con x xs) ys = Con x . interleaveStreams ys $ xs

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) . streamMap (+ 1) $ ruler
