module JoinList where

import Data.Monoid

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m jl1 jl2) = (tag jl1) <> (tag jl2)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) jl1 jl2 = Append (tag jl1 <> tag jl2) jl1 jl2
