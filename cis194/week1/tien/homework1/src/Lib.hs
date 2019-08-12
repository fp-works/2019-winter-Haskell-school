module Lib
  ( toDigits
  , toDigitsRev
  , buildWeights
  , doubleEveryOther
  , sumDigits
  , validate
  , hanoi
  ) where

-- Exercise One: --
charToString c = c : []

toDigits :: Integer -> [Integer]
toDigits num
  | num > 0 = map (read . charToString) $ show num :: [Integer]
  | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev num = reverse . toDigits $ num

-- Exercise Two: --
buildWeights :: Integer -> [Integer]
buildWeights num = (take . fromInteger) num (cycle [1, 2])

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther arr =
  zipWith (*) arr . reverse . buildWeights . toInteger . length $ arr

-- Exercise Three: --
sumDigits :: [Integer] -> Integer
sumDigits arr = sum . concatMap (toDigits) $ arr

-- Exercise Four: --
validate :: Integer -> Bool
validate ccNum
  | ((flip mod 10) . sumDigits . doubleEveryOther . toDigits $ ccNum) == 0 =
    True
  | otherwise = False

-- Exercise Five --
type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi num a b c
  | num > 1 = hanoi (num - 1) a c b ++ [(a, b)] ++ hanoi (num - 1) c b a
  | otherwise = [(a, b)]
