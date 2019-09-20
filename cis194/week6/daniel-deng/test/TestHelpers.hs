module CIS194.Homework06.TestHelpers (streamTake) where

import CIS194.Homework06.Exercise03 (Stream, streamToList)

streamTake :: Int -> Stream a -> [a]
streamTake n = take n . streamToList
