{-# OPTIONS_GHC -Wall #-}

module CreditCards where

toDigits :: Integer -> [Integer]
toDigits = fmap (`rem` 10) . takeWhile (>0) . iterate (`div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x : y : xs) = (x : 2 * y : doubleEveryOther xs)

sumDigits :: [Integer] -> Integer
sumDigits = sum . flatten . fmap toDigits
  where flatten = foldr (++) []

validate :: Integer -> Bool
validate n = ((sumDigits . doubleEveryOther . toDigits $ n) `rem` 10) == 0

