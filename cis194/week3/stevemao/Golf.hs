{-# OPTIONS_GHC -Wall #-}
module Golf where
import Data.List.HT
import Data.Foldable (fold)

skips :: [a] -> [[a]]
skips [] = []
skips list@(x : _) = fmap (\(_, index) -> drop 1 . sieve index $ (x : list)) . zip list $ [1..]

getSecondIfMax :: (Integer, Integer, Integer) -> [Integer] -> [Integer]
getSecondIfMax (a, b, c) list
  | b > a && b > c = [b] ++ list
  | otherwise = list

localMaxima :: [Integer] -> [Integer]
localMaxima list = foldr getSecondIfMax [] $ zip3 list (drop 1 list) (drop 2 list)

numPerLine :: [Integer] -> [Int]
numPerLine list = fmap (\n -> length . filter (== n) $ list) [0..9]

printLine :: [Int] -> Int  -> String
printLine list n = fmap (\a -> if a > n then '*' else ' ') list ++ "\n"

histogram :: [Integer] -> String
histogram list = (fold . fmap (printLine npl) $ [maxNpl, maxNpl - 1..0]) ++ "==========\n0123456789\n"
  where npl = numPerLine list
        maxNpl = maximum npl - 1
