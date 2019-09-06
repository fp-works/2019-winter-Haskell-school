{-# OPTIONS_GHC -Wall #-}
module Homework where

xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\curr acc -> [f curr] ++ acc) [] xs
