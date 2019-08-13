module Homework
  ( toDigits
  , doubleEveryOther
  , sumDigits
  , validate
  , hanoi
  , hanoi4
  )
where

-- Exercise 1
toDigitsRev :: Integer -> [Integer]
toDigitsRev x | x <= 0    = []
              | otherwise = (x `mod` 10) : toDigitsRev (x `div` 10)
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- Exercise 2

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev []             = []
doubleEveryOtherRev [x           ] = [x]
doubleEveryOtherRev (x1 : x2 : xs) = x1 : (x2 * 2) : doubleEveryOtherRev xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherRev . reverse

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigitsRev

-- Exercise 4
validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigits) x `rem` 10 == 0

-- Exercise 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c | n < 0     = []
              | otherwise = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a

-- Exercise 6
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 a b _ _ = [(a, b)]
hanoi4 n a b c d
  | n < 0     = []
  | otherwise = hanoi4 (n - 2) a c b d ++ [(a, d), (a, b), (d, b)] ++ hanoi4 (n - 2) c b a d
