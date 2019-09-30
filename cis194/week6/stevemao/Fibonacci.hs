{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
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
streamFromSeed f a = Cons a . streamFromSeed f . f $ a

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a as) bs = Cons a . interleaveStreams bs $ as

generateRuler :: Integer -> Stream Integer
generateRuler a = interleaveStreams (streamRepeat a) . generateRuler $ a + 1

ruler :: Stream Integer
ruler = generateRuler 0

x :: Stream Integer
x = Cons 0 . Cons 1 . streamRepeat $ 0

-- using fromInteger and then (*) is too slow
intMulStream :: Integer -> Stream Integer -> Stream Integer
intMulStream i = streamMap (* i)

instance Num (Stream Integer) where
  fromInteger n = Cons n . streamRepeat $ 0
  negate = streamMap negate
  (Cons n1 s1) + (Cons n2 s2) = Cons (n1 + n2) $ s1 + s2
  (Cons n1 s1) * s@(Cons n2 s2) = Cons (n1 * n2) $ intMulStream n1 s2 + s1 * s
  
instance Fractional (Stream Integer) where
  si1@(Cons n1 s1) / si2@(Cons n2 s2) = Cons (div n1 n2) . intMulStream (div 1 n2) $ s1 - si1 / si2 * s2

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x ^ 2)

data Matrix = Matrix ((Integer, Integer), (Integer, Integer))

instance Num Matrix where
  Matrix ((a, b), (c, d)) * Matrix ((a', b'), (c', d'))
    = Matrix ((a * a' + c * b', b * a' + d * b'), (a * c' + c * d', b * c' + d * d'))

getBottomLeft :: Matrix -> Integer
getBottomLeft (Matrix ((_, b), (_, _))) = b

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = getBottomLeft $ Matrix ((1, 1), (1, 0)) ^ n
