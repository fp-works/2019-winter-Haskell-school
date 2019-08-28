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
skips :: [a] -> [[a]]
skips l = (flip $ (.) <$> sieve <*> drop . (-1 +)) l <$> [1..length l]