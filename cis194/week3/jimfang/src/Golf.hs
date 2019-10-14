module Golf where

-- Refer to the homework of zhansongl
-- need utility-ht
import Data.List.HT (sieve, tails)
-- need split
import Data.List.Split (divvy)
import Data.Map (empty, insert, adjust, (!?))
import Data.Bool (bool)
-- need safe
import Safe (initSafe, atDef)

{-
- tails "ABCD" --> ["ABCD", "BCD", "CD", "D", ""]
-
- sieve :: Int -> [a] -> [a] 
-   keep every k-th value from the list
- sieve 1 "ABCD" --> "ABCD"
- sieve 2 "BCD" --> "BD"
- sieve 3 "CD" --> "C"
- sieve 4 "D" --> "D"
-}

skips :: [a] -> [[a]]
skips = zipWith sieve [1..] . initSafe . tails


{-
- `divvy n m xs` take n elements from xs, shift by m, until the end,
- e.g.
-     divvy 3 2 [1,2,3,4,5] -> [[1,2,3],[3,4,5]]
-     divvy 3 1 [1,2,3,4,5] -> [[1,2,3],[2,3,4],[3,4,5]]
-
- `atDef def xs n` returns the nth element in the list, zero-indexed,
- If n is out of bound it returns def, so this function is safe.
- e.g.
-     atDef 0 [1,2,3,4,5] 1 = 2
-     atDef 0 [1,2,3,4,5] 100 = 0
-}

localMaxima :: [Integer] -> [Integer]
localMaxima = map (\x -> atDef 0 x 1) . filter (\[a, b, c] -> a <= b && b >= c) . divvy 3 1

{-
- Create a map that counts the number of occurrences of each digit,
- Create `h` number of rows where `h` is the maximum number of occurrences,
- Draw `h` lines and put star or space in based on the occurrences of the digit
-   and the line number, i.e. for the 2nd line if there are more than 2 2's in the
-   input a star should be drawn, otherwise it should be a space.
- Put the lines together to get the histogram.
-}

histogram :: [Int] -> String
histogram xs = (++ "==========\n0123456789\n")
             . unlines
             $ reverse (map (\k -> map (\n -> bool ' ' '*' (maybe (0::Int) id (p!?n) >= k)) l) [1..(maximum p)])
  where p = foldr (adjust (+1)) (foldr (flip insert 0) empty l) xs
        l = [0..9]

