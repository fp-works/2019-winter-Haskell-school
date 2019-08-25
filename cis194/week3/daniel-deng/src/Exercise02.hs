module CIS194.Homework03.Exercise02 (localMaxima) where

import Data.List
import Data.Maybe

maybeLocalMax :: [Integer] -> Maybe Integer
maybeLocalMax [x, y, z] | y > x && y > z = Just y
maybeLocalMax _                          = Nothing

window :: Int -> [a] -> [[a]]
window n = filter ((>= n) . length) . fmap (take n) . tails

localMaxima :: [Integer] -> [Integer]
localMaxima = mapMaybe maybeLocalMax . window 3
