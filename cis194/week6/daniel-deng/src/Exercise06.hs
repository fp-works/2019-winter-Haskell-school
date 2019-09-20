{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module CIS194.Homework06.Exercise06 (fibs3) where

import CIS194.Homework06.Exercise03 (Stream(..), streamToList)
import CIS194.Homework06.Exercise04 (streamRepeat, streamMap)

import Prelude as P

x :: Stream Integer
x = Stream 0 . Stream 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger                         = Stream <*> streamRepeat . pure 0
  negate                              = streamMap P.negate
  (+) (Stream x0 xs) (Stream y0 ys)   = Stream (x0 + y0) (xs + ys)
  (*) (Stream x0 xs) s@(Stream y0 ys) = Stream (x0 * y0) $ fromInteger x0 * ys + xs * s

instance Fractional (Stream Integer) where
  (/) (Stream a0 a') (Stream b0 b') = q
                                    where
                                      q  = Stream c cs
                                      c  = a0 `div` b0
                                      cs = fromIntegral (1 `div` b0) * (a' - q * b')

fibs3 :: [Integer]
fibs3 = streamToList $ x / divider
      where
        divider = 1 - x - x * x
