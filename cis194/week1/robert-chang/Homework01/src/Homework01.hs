module Homework01
    ( toDigits
    , toDigitsRev
    , doubleEveryOther
    , sumDigits
    , validate
    ) where

-- toDigits
toDigits :: Integer -> [Integer]
toDigits num
  | 0 >= num  = []
  | otherwise = toDigits (num `div` 10) ++ [num `mod` 10]

-- toDigitsRev
toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- doubleEveryOther
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (zipWith ($) (cycle [id, (*2)]) (reverse xs))

-- sumDigits
sumDigits :: [Integer] -> Integer 
sumDigits =  sum . concatMap (toDigits) 

-- validate 
getLastDigit :: Integer -> Integer
getLastDigit =  last . toDigits 

eQZero :: Integer -> Bool
eQZero = (== 0)

validate :: Integer -> Bool
validate =  eQZero . getLastDigit . sumDigits . doubleEveryOther . toDigits   
