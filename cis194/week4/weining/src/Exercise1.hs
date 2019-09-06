module Exercise1
    ( fun1Original
    , fun1
    , fun2Original
    , fun2
    ) where

import           Data.Bool

fun1Original :: [Integer] -> Integer
fun1Original [] = 1
fun1Original (x:xs)
    | even x = (x - 2) * fun1Original xs
    | otherwise = fun1Original xs

fun1 :: [Integer] -> Integer
fun1 = (foldr (*) 1)
     . map calc
  where calc = bool <$> (const 1) <*> (subtract 2) <*> even
-- alternative (shorter)
-- fun1 = (foldr (*) 1) . map (subtract 2) . (filter even)
-- even better
-- fun1 = product . map (flip (-) 2) . (filter even)
-- or (squeeze out the flip)
-- fun1 = product . map (+ (-2)) . (filter even)

fun2Original :: Integer -> Integer
fun2Original 1 = 0
fun2Original n | even n    = n + fun2Original (n `div` 2)
               | otherwise = fun2Original (3 * n + 1)

fun2 = sum .
       filter even .
       takeWhile (> 1) .
       iterate (bool <$> ((+ 1) . (* 3)) <*> (flip div 2) <*> even)
