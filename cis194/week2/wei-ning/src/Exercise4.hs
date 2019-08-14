module Exercise4
  ( inOrder
  ) where

import qualified Exercise3
import           Log
import           Prelude   hiding (Error)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf         = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r
