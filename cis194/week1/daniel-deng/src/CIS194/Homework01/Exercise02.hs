module CIS194.Homework01.Exercise02 (doubleEveryOther) where

import Data.Bool

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = snd . foldr f (0 :: Integer, [] :: [Integer])
                 where
                   f x (cnt, xs) = (cnt + 1, newVal x cnt : xs)
                   newVal x      = bool x (x * 2) . odd
