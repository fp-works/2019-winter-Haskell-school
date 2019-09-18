module Exercise3 (streamToList) where

import           Stream

streamToList :: Stream a -> [a]
streamToList (Cons x s) = x : streamToList s

instance Show a => Show (Stream a) where
  show s = "[" ++ show (head . streamToList $ s) ++ "..]"
