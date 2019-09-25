module Lib where

-- ex1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- https://stackoverflow.com/questions/1105765/generating-fibonacci-numbers-in-haskell
-- ex2
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- ex3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs 

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

-- ex4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- ex5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStream :: Stream a -> Stream a -> Stream a
interleaveStream (Cons x xs) ys = Cons x (interleaveStream ys xs)

startRuler n = interleaveStream (streamRepeat n) (startRuler (n+1))

ruler :: Stream Integer
ruler = startRuler 0

