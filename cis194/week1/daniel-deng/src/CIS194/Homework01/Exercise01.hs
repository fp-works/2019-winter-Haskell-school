module CIS194.Homework01.Exercise01 (toDigits, toDigitsRev) where

toDigits :: Integer -> [Integer]
toDigits num
  | num <= 0  = []
  | otherwise = toDigits (num `div` 10) ++ [num `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits
