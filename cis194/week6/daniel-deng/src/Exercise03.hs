module CIS194.Homework06.Exercise03 (Stream(..), streamToList) where

data Stream a = Stream a (Stream a)

-- we skip unit test for this function
streamToList :: Stream a -> [a]
streamToList (Stream x xs) = x : streamToList xs
