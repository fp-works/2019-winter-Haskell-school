module CIS194.Homework01.Exercise03 (sumDigits) where

import CIS194.Homework01.Exercise01 (toDigits)

sumDigits :: [Integer] -> Integer
sumDigits = sum . (>>= toDigits)
