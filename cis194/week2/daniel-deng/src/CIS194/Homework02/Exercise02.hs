module CIS194.Homework02.Exercise02 (insert) where

import CIS194.Homework02.Log

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logMsg      Leaf = Node Leaf logMsg Leaf
insert logMsg1@(LogMessage _ ts1 _) (Node left logMsg2@(LogMessage _ ts2 _) right)
  | ts1 > ts2 = Node left logMsg2 (insert logMsg1 right)
  | otherwise = Node (insert logMsg1 left) logMsg2 right
