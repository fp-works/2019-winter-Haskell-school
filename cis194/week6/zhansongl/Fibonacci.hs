{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

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

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons h t) = h : streamToList t

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat = Cons <*> streamRepeat

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons h t) = Cons (f h) (streamMap f t)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f = Cons <*> streamFromSeed f . f

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a as) bs = Cons a . interleaveStreams bs $ as

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap (+1) ruler)

x :: Stream Integer
x = Cons 0 . Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger n = Cons n . streamRepeat $ 0
  negate = streamMap Prelude.negate
  (+) (Cons a0 a') (Cons b0 b') = Cons (a0 + b0) (a' + b')
  (*) (Cons a0 a') b@(Cons b0 b') = Cons (a0 * b0) (fromIntegral a0 * b' + a' * b)

instance Fractional (Stream Integer) where
  (/) (Cons a0 a') (Cons b0 b') = q
    where q = Cons (a0 `div` b0) (fromIntegral (1 `div` b0) * (a' - q * b'))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x * x)

data Matrix = Matrix Integer Integer Integer Integer deriving (Show, Eq)

instance Num Matrix where
  (*) (Matrix a11 a12 a21 a22) (Matrix b11 b12 b21 b22) =
      (Matrix (a11 * b11 + a12 * b21) (a11 * b12 + a12 * b22)
            (a21 * b11 + a22 * b21) (a21 * b12 + a22 * b22))

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = get12 $ (Matrix 1 1 1 0)^n
  where get12 (Matrix _ a _ _) = a

