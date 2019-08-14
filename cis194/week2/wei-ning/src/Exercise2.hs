module Exercise2
  ( insert
  ) where

import           Log
import           Prelude hiding (Error)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert m Leaf        = Node Leaf m Leaf
insert m@(LogMessage _ tt _) (Node l n@(LogMessage _ this _) r)
  | tt > this = Node l n (insert m r)
  | tt < this = Node (insert m l) n r
