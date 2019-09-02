module Lib where
import Data.Bool.HT
--Task1

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
 | even x = (x - 2) * fun1 xs
 | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
--fun1' = product . fmap (subtract 2) . filter even
--fun1' = product . map (\x -> x - 2) . filter even
fun1' = product . (subtract 2 <$>) . filter even


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
 | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
{-
fun2' = sum 
 . filter even
 . takeWhile (>1) 
 . iterate (\n -> 
    if even n
    then n `div` 2
    else 3 * n + 1) 
-}

fun2' = sum 
 . filter even
 . takeWhile (>1) 
 . iterate (\n -> (even n ) ?: (n `div` 2, (3 * n + 1)))


-- task 3
xor :: [Bool] -> Bool
{-

True (/=) True == False
True (/=) False == True

so this can be used to calculate the result:

foldr (/=) False [True, False, True]
step1 list: [True, False, True] => True:(False:(True:[]))
step2 apply foldr: True (/=) (False (/=) (True (/=) False)) 
step3 result: False
-}
xor = foldr (/=) False

