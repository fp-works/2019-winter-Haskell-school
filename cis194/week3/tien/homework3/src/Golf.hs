module Golf where

import           Data.Array
import           Data.Bool
import           Data.List

-- Exercise 1 --
everyNth :: Integral b => b -> [(a, b)] -> [a]
everyNth n = fst . unzip . filter ((== 0) . (`mod` n) . snd)

skips :: [a] -> [[a]]
skips [] = []
skips arr = (`everyNth` pairs) <$> indexArr
  where
    indexArr = [1 .. length arr]
    pairs = zip arr indexArr

-- Exercise 2 --
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs)
  | y > x && y > z = y : localMaxima (y : z : xs)
  | otherwise = localMaxima (y : z : xs)
localMaxima _ = []

-- Exercise 3 --
formPairs :: [a] -> [(a, Integer)]
formPairs = flip zip . repeat $ 1

formGroups :: [Integer] -> [Integer]
formGroups =
  concat .
  drop 1 .
  transpose .
  map (\(x, y) -> [x, y]) . assocs . accumArray (+) 0 (0, 9) . formPairs

mapToHistogram :: [Integer] -> [String]
mapToHistogram = map mapToString
  where
    mapToString = bool <$> const " " <*> const "*" <*> (> 0)

formHistogram :: [Integer] -> [[String]]
formHistogram arr
  | any (> 1) arr =
    (formHistogram . map remainingPoints $ arr) ++ [mapToHistogram arr]
  | otherwise = [mapToHistogram arr]
  where
    remainingPoints = bool <$> id <*> subtract 1 <*> (> 0)

histogram :: [Integer] -> String
histogram =
  unlines .
  (++ ["==========", "0123456789"]) . map concat . formHistogram . formGroups
