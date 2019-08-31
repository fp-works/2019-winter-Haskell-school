module Golf where
import Data.List.HT

-- attempt 1 use sieve from utility-ht

-- sieve :: Int -> [a] -> [a] Source#
-- keep every k-th value from the list

-- skips :: [a] -> [[a]]
-- skips l = (flip sieve) l <$> [1..length l]
-- however the output is:
--  skips 'ABCD'
--  expected: ["ABCD","BD","C","D"]
--  but got: ["ABCD","AC","AD","A"]

-- attempt 2 
{-
skips :: [a] -> [[a]]
skips l = (flip $ (.) <$> sieve <*> drop . (-1 +)) l <$> [1..length l]
-}

-- attempt 3 (inspired and copied from Zehua's solution)
skips :: [a] -> [[a]]
-- fmap: 
-- applied n => sieve n (drop (n-1) l) to each element
-- drop (n-1) l will remove the first n-1 elements
-- then sieve n newElement will move the index to the current n
-- so that attempt 1 issue will be resolved 
skips l = (\n -> sieve n (drop (n-1) l)) <$> [1..length l]


-- using recurison 
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:as)
  | x < y && y > z = y : localMaxima (y:z:as)
  | otherwise      = localMaxima (y:z:as)
localMaxima _ = []
