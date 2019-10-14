{-# OPTIONS_GHC -Wall #-}

module Golf where

import Data.Bool (bool)
import Data.List (transpose, sort, group)
-- base-unicode-symbols
import Data.List.Unicode ((∈))

-- need utility-ht
import Data.List.HT (sieve, tails)
-- need split
import Data.List.Split (divvy)
-- need safe
import Safe (initSafe, atDef)

{-
- tails "ABCD" --> ["ABCD", "BCD", "CD", "D", ""]
-
- sieve 1 "ABCD" --> "ABCD"
- sieve 2 "BCD" --> "BD"
- sieve 3 "CD" --> "C"
- sieve 4 "D" --> "D"
-}

skips :: [a] -> [[a]]
-- 38 chars
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
-- 66 chars
localMaxima = map (\x -> atDef 0 x 1) . filter (\[a, b, c] -> a <= b && b >= c) . divvy 3 1

{-
 - *) group . sort $ [1,2,3,4,4,4,5,9] => [[1],[2],[3],[4,4,4],[5],[9]]
 -
 - *) and then transform this into rows of the diagram:
 -
 -    transpose [[1],[2],[3],[4,4,4],[5,5],[9]] => [[1,2,3,4,5,9],[4,5],[4]]
 -
 - *) draw each row/line:
 -
 -    r = [4,5]
 -    '0123456789' => '    **    '
 -
 - *) stick all the lines together and add x-axis and labels
-}

histogram :: [Int] -> String
-- 116 (118?) chars
histogram = (++ "==========\n0123456789\n")
          . unlines
          . reverse
          . map (\r -> map (\c -> bool ' ' '*' (c ∈ r)) [0..9])
          . transpose
          . group
          . sort
