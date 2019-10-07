{-# OPTIONS_GHC -Wall #-}
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
hanoi4 0 _ _ _ _ = []
-- https://en.wikipedia.org/wiki/Tower_of_Hanoi#With_four_pegs_and_beyond
-- Frame–Stewart algorithm
hanoi4 n a b c d = (hanoi4 k a c b d) ++ (hanoi (n - k) a b d) ++ (hanoi4 k c b a d)
  where k = n - round (sqrt double) + 1
        double = (fromInteger $ 2 * n + 1) :: Double
