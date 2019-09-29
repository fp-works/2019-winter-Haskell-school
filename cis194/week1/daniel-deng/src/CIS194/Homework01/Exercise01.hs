{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module CIS194.Homework01.Exercise01 (toDigits, toDigitsRev) where

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = getDigit <$> [0 .. digitCount - 1]
              where
                num        = abs n
                digitCount = max 1 . ceiling . logBase 10 . fromIntegral $ num
                getDigit   = (`mod` 10) . (num `div`) . (10 ^)
