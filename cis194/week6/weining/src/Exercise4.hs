module Exercise4
  ( streamRepeat
  , streamMap
  , streamFromSeed
  ) where

import           Stream

streamRepeat :: a -> Stream a
streamRepeat v = singletonStream v

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons v vs) = Cons (f v) (streamMap f vs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed =
  let next = f seed
  in Cons seed (streamFromSeed f next)

