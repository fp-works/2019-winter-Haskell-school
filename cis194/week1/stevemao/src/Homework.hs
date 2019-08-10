module Homework ( toDigits
                , toDigitsRev
                , doubleEveryOther
                , sumDigits
                , validate) where

toDigits :: Integer -> [Integer]
toDigits a
  | a <= 0  = []
  | otherwise = toDigits (a `div` 10) ++ [a `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]

doubleOrNot :: Integer -> Int -> Integer
doubleOrNot a b
  | b `mod` 2 == 0 = a * 2
  | otherwise = a

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = mapInd doubleOrNot

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

divisible :: Integer -> Bool
divisible a = a `mod` 10 == 0

validate :: Integer -> Bool
validate = divisible . sumDigits . doubleEveryOther . toDigits
