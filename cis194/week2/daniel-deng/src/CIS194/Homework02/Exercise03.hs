module CIS194.Homework02.Exercise03 (build) where

import CIS194.Homework02.Log
import CIS194.Homework02.Exercise02 (insert)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf
