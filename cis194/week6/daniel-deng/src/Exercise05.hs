module CIS194.Homework06.Exercise05 (nats, ruler) where

import CIS194.Homework06.Exercise03 (Stream(..))
import CIS194.Homework06.Exercise04 (streamRepeat, streamMap, streamFromSeed)

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream x xs) ys = Stream x . interleaveStreams ys $ xs

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap (+1) ruler)
