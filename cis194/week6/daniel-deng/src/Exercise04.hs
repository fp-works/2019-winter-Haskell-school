module CIS194.Homework06.Exercise04 (streamRepeat, streamMap, streamFromSeed) where

import CIS194.Homework06.Exercise03 (Stream(..))

streamRepeat :: a -> Stream a
streamRepeat = Stream <*> streamRepeat

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x xs) = Stream (f x) . streamMap f $ xs

streamFromSeed :: (a -> a) -> a -> Stream a
-- streamFromSeed f seed = Stream seed $ streamFromSeed f (f seed)
streamFromSeed f = Stream <*> streamFromSeed f . f
