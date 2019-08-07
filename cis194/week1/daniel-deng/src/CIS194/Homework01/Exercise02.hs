module CIS194.Homework01.Exercise02 (doubleEveryOther) where

doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft []                      = []
doubleEveryOtherFromLeft [element]               = [element]
doubleEveryOtherFromLeft (first : second : rest) = [first, second * 2] ++ doubleEveryOtherFromLeft rest

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherFromLeft . reverse
