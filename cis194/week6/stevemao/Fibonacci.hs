{-# OPTIONS_GHC -Wall #-}
module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = (fib $ n - 1) + (fib $ n - 2)

fibs1 :: [Integer]
fibs1 = fmap fib [0..]

start :: Integer -> Integer -> [Integer]
start a b = a : (start b $ a + b)

fibs2 :: [Integer]
fibs2 = start 0 1

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a s) = (a : streamToList s)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat a = Cons a . streamRepeat $ a

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a s) = Cons (f a) . streamMap f $ s

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons val . streamFromSeed f $ val
  where val = f a

nats :: Stream Integer
nats = streamFromSeed (+ 1) (-1)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a as) bs = Cons a . interleaveStreams bs $ as

generateRuler :: Integer -> Stream Integer
generateRuler a = interleaveStreams (streamRepeat a) . generateRuler $ a + 1

ruler :: Stream Integer
ruler = generateRuler 0

data Matrix = Matrix ((Integer, Integer), (Integer, Integer))

instance Num Matrix where
  Matrix ((a, b), (c, d)) * Matrix ((a', b'), (c', d'))
    = Matrix ((a * a' + c * b', b * a' + d * b'), (a * c' + c * d', b * c' + d * d'))

getBottomLeft :: Matrix -> Integer
getBottomLeft (Matrix ((_, b), (_, _))) = b

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = getBottomLeft $ Matrix ((1, 1), (1, 0)) ^ n
