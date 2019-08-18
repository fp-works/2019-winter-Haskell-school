module Homework ( toDigits
                , toDigitsRev
                , doubleEveryOther
                , sumDigits
                , validate
                , hanoi ) where

-- 1
toDigits :: Integer -> [Integer]
toDigits num
  | num > 0  = toDigits (num `div` 10) ++ [num `rem` 10]
  | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x1 : x2 : xs) = x1 : (x2 * 2) : doubleEveryOther xs

-- 3
sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

-- 4
divisible :: Integer -> Bool
divisible num = num `mod` 10 == 0

validate :: Integer -> Bool
validate = divisible. sumDigits . doubleEveryOther . toDigitsRev

--5
type Disks = Integer
type Peg   = String
type Move  = (Peg, Peg)

hanoi :: Disks -> Peg -> Peg -> Peg -> [Move]

hanoi n a b c
  | n > 0 = hanoi (n-1) a c b 
          ++ [(a,b)]
          ++ hanoi (n-1) c b a
  | otherwise = []