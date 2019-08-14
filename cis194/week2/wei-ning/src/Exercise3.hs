module Exercise3
  ( build
  ) where

import qualified Exercise2
import           Log
import           Prelude   hiding (Error)

build :: [LogMessage] -> MessageTree
build = foldr Exercise2.insert Leaf
