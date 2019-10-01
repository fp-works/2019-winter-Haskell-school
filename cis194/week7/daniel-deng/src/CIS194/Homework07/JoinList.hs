module CIS194.Homework07.JoinList ( JoinList(..), tag ) where

import CIS194.Homework07.Sized ( Sized(..) )

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

instance Monoid m => Semigroup (JoinList m a) where
  Empty <> jl = jl
  jl <> Empty = jl
  jl1 <> jl2  = Append (tag jl1 <> tag jl2) jl1 jl2

instance Monoid m => Monoid (JoinList m a) where
  mempty = Empty

instance Sized b => Sized (JoinList b a) where
  size Empty          = 0
  size (Single s _)   = size s
  size (Append s _ _) = size s
