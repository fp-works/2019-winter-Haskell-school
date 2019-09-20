module CIS194.Homework06.Exercise02 (fibs2) where

foldPlusAppend :: Integer -> Integer -> [Integer]
foldPlusAppend a b = a : foldPlusAppend b (a + b)

fibs2 :: [Integer]
fibs2 = foldPlusAppend 0 1
