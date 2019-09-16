{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Exercises where


-- ex1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
-- fib = (+) <$> fib . subtract 1 <*> fib . subtract 2

fibs1 :: [Integer]
fibs1 = map fib [0..]


-- ex2
fibs2 :: [Integer]
fibs2 = map fst $ iterate fibnext (0,1)
  where
    fibnext (n1, n) = (n, n1 + n)


-- ex3
data Stream a = Cons a (Stream a) deriving Eq

streamToList :: Stream a -> [a]
streamToList (Cons a b) = a : streamToList b

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList


-- ex4
streamRepeat :: a -> Stream a
streamRepeat a = Cons a $ streamRepeat a

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a b) = Cons (f a) $ streamMap f b

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a $ streamFromSeed f (f a)


-- ex5
nats :: Stream Integer
nats = streamFromSeed (+1) 1

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a b) s = (Cons a (interleaveStreams s b))
-- this version is not as lazy and actually does not work for ruler
-- interleaveStreams (Cons x1 y1) (Cons x2 y2) = (Cons x1 (Cons x2 (interleaveStreams y1 y2)))

ruler :: Stream Integer
-- interleaving with all 0 and itself but plus 1 to all elements
ruler = interleaveStreams (streamRepeat 0) (streamMap (+1) ruler)

-- ex6
-- x = 0 + 1x + 0x^2 + 0x^3 + ...
x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

streamConstNum :: Num a => a -> Stream a
streamConstNum n = Cons n (streamRepeat 0)

instance (Num a) => Num (Stream a) where
  fromInteger                   = streamConstNum . fromInteger
  negate (Cons n xs)            = Cons (-n) xs
  (Cons a0 a') + (Cons b0 b')   = Cons (a0 + b0) $ a' + b'
  (Cons a0 a') * b@(Cons b0 b') = Cons (a0 * b0) $ (streamConstNum a0) * b' + a' * b

--  (a0/b0) + x((1/b0)(A' − Q*B')).
--divStream :: Stream Integer -> Stream Integer -> Stream Integer
divStream :: Integral a => Stream a -> Stream a -> Stream a
divStream (Cons a0 a') (Cons b0 b') = q
    where
      q = Cons (div a0 b0) $
          (divStream (fromInteger 1) (streamConstNum b0)) * (a' - q * b')

fibs3 :: [Integer]
fibs3 = streamToList (divStream x ((fromInteger 1) - x - x * x))

{-
instance (Enum a) => Enum (Stream a) where

instance (Ord a) => Ord (Stream a) where

instance (Real a) => Real (Stream a) where

instance (Integral a) => Integral (Stream a) where
  div (Cons a0 a') (Cons b0 b') = q
    where
      q = Cons (div a0 b0) $
          (div (fromInteger 1) (streamConstNum b0)) * (a' - q * b')

fibs3 = streamToList (div x ((fromInteger 1) - x - x * x))
-}

-- ex7
-- a 2x2 matrix
-- data Matrix = Matrix Integer Integer Integer Integer deriving (Eq, Show)
data Matrix a = Matrix a a a a deriving (Eq, Show)

instance Num a => Num (Matrix a) where
  Matrix a11 a12 a21 a22 * Matrix b11 b12 b21 b22
    = Matrix
        (a11 * b11 + a12 * b21)
        (a11 * b12 + a12 * b22)
        (a21 * b11 + a22 * b21)
        (a21 * b12 + a22 * b22)

f1 :: Matrix Integer
f1 = Matrix 1 1 1 0

fib4 :: Integer -> Integer
-- taking power of (n+1) to handle fib4 0 == 0
fib4 n = case f1 ^ (n + 1) of
           Matrix _ _ _ fn -> fn
