module CIS194.Homework02.Exercise02 (insert) where

import CIS194.Homework02.Log

insert :: LogMessage -> MessageTree -> MessageTree
insert logMsg@LogMessage {} Leaf = Node Leaf logMsg Leaf
insert msg1@(LogMessage _ ts1 _) (Node left msg2@(LogMessage _ ts2 _) right)
  | ts1 > ts2                    = Node left msg2 (insert msg1 right)
  | otherwise                    = Node (insert msg1 left) msg2 right
insert _ tree                    = tree
