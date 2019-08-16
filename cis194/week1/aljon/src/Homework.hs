module Homework where

-- Week 1 Exercise: https://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf

-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits x
  | x > 0 = toDigits (div x 10) ++ [mod x 10]
  | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

-- Exercise 2
doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft = zipWith ($) (cycle [id, (*2)])

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = (reverse . doubleEveryOtherFromLeft . reverse)

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits xs = sum (map (sum . toDigits) xs)

-- Exercise 4
validate :: Integer -> Bool
validate creditCard = (sumDigits (doubleEveryOther (toDigits creditCard))) `mod` 10 == 0

-- Exercise 5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a
