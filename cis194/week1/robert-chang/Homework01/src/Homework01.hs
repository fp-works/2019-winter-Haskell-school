module Homework01
    ( toDigits
    , toDigitsRev
    , doubleEveryOther
    , sumDigits
    ) where

toDigits :: Integer -> [Integer]
toDigits num
  | 0 >= num  = []
  | otherwise = toDigits (num `div` 10) ++ [num `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (zipWith ($) (cycle [id, (*2)]) (reverse xs))

sumDigits :: [Integer] -> Integer 
sumDigits =  sum . concatMap (toDigits) 

