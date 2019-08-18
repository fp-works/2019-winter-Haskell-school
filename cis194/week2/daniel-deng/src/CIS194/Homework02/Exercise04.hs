module CIS194.Homework02.Exercise04 (inOrder) where

import CIS194.Homework02.Log

-- assumption: the given MessageTree is sorted by timestamp
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                  = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right
