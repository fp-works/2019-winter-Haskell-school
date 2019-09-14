{-# OPTIONS_GHC -Wall #-}

module CreditCards where

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n >= 0 = digit n
  | otherwise = []
  where digit 0 = []
        digit n' = n' `rem` 10 : digit (n' `div` 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev [x] = [x]
doubleEveryOtherRev (x : y : xs) = (x : 2 * y : doubleEveryOtherRev xs)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherRev . reverse

sumDigits :: [Integer] -> Integer
sumDigits xs = sum . flatten $ map toDigits xs
  where flatten = foldr (++) []

validate :: Integer -> Bool
validate n = ((sumDigits . doubleEveryOther $ toDigits n) `rem` 10) == 0

