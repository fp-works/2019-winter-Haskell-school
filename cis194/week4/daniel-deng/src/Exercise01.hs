module CIS194.Homework04.Exercise01 (fun1, fun2) where

import Data.Bool

fun1 ::  [Integer] -> Integer
fun1 = product . fmap (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 = sum
     . filter even
     . takeWhile (>1)
     . iterate (bool <$> handleOdd <*> handleEven <*> even)
   where
     handleOdd  = (1+) . (3*)
     handleEven = (`div` 2)
{-
fun2 = bool <$> handleN <*> const 0 <*> (<=1)
     where
       handleN    = bool <$> handleOdd <*> handleEven <*> even
       handleOdd  = fun2 . (1+) . (3*)
       handleEven = (+) <*> fun2 . (`div` 2)
-}
