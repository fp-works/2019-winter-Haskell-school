module Exercise1
  ( toDigits
  , toDigitsRev
  ) where

toDigitsRev :: Integer -> [Integer]
toDigitsRev x | x <= 0    = []
              | x < 10    = [x]
              | otherwise = (x `mod` 10) : toDigitsRev (x `div` 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev
