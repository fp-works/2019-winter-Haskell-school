{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module CIS194.Homework06.Exercise07 (fib4) where

data Mat2x2 a = Mat2x2 a a a a

instance Num a => Num (Mat2x2 a) where
  Mat2x2 a11 a12 a21 a22 * Mat2x2 b11 b12 b21 b22
    = Mat2x2
        (a11 * b11 + a12 * b21)
        (a11 * b12 + a12 * b22)
        (a21 * b11 + a22 * b21)
        (a21 * b12 + a22 * b22)

fib4 :: Integer -> Integer
fib4 n
  | n <= 0    = 0
  | otherwise = r1c2 $ Mat2x2 1 1 1 0 ^ n
              where
                r1c2 (Mat2x2 _ a _ _) = a
