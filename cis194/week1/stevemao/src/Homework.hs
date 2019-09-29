module Homework where

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
  | b `mod` 2 == 0 = a
  | otherwise = a * 2

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . mapInd doubleOrNot . reverse

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

divisible :: Integer -> Bool
divisible a = a `mod` 10 == 0

validate :: Integer -> Bool
validate = divisible . sumDigits . doubleEveryOther . toDigits

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ 
  = []
hanoi n a b c 
  = hanoi (n - 1) a c b 
  ++ [(a, b)]
  ++ hanoi (n - 1) c b a
  
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _
  = []
hanoi4 1 a b _ _
  = [(a, b)]
hanoi4 n a b c d
  = hanoi4 (n - 2) a c d b  
  ++ [(a, d), (a, b), (d, b)]
  ++ hanoi4 (n - 2) c b a d 
