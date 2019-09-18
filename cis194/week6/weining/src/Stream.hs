module Stream
  ( Stream(..)
  , singletonStream
  ) where

data Stream a = Cons a (Stream a)

singletonStream v = Cons v (singletonStream v)
