{-# OPTIONS_GHC -Wall #-}
module Golf
  ( skips
  , localMaxima
  , histogram
  )
where

-- Exercise 1

-- Filter a list to every nth element
skipN :: [a] -> Int -> [a]
skipN x n = case drop (n - 1) x of
  (y : ys) -> y : skipN ys n
  _        -> []

skips :: [a] -> [[a]]
skips x = map (skipN x) [1 .. (length x)]

-- Exercise 2

localMaxima :: [Integer] -> [Integer]
localMaxima (x : xs@(y : z : _)) | y > x && y > z = y : localMaxima xs
                                 | otherwise      = localMaxima xs
localMaxima _ = []

-- Exercise 3

-- Count how many times a value appears in a list
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

-- Get histogram as a list
hList :: [Integer] -> [Int]
hList x = map (`count` x) [0 .. 9]

hChar :: Bool -> Char
hChar True = '*'
hChar _    = ' '

-- Get nth line for histogram string with counts h
hLine :: [Int] -> Int -> String
hLine h n = map (hChar . (>= n)) h

histogramLines :: [Int] -> [String]
histogramLines x = map (hLine x) [1 .. (foldr max 0 x)]

histogram :: [Integer] -> String
histogram x = unlines . reverse $ ['0' .. '9'] : replicate 10 '=' : (histogramLines . hList $ x)
