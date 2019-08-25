module CIS194.Homework03.Exercise03 (histogram) where

import Data.List

stars :: Int -> String
stars = flip replicate '*'

spaces :: Int -> String
spaces = flip replicate ' '

starLine :: Int -> String
starLine nTimes = spaces (10 - nTimes) ++ stars nTimes

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

starLines :: [Integer] -> [String]
starLines xs = filter (/= spaces 10)
             . transpose
             . fmap (starLine . flip count xs)
             $ [0..9]

histogram :: [Integer] -> String
histogram = unlines . (++ ["==========", "0123456789"]) . starLines
