module CIS194.Homework07.Exercise02 ( indexJ, dropJ, takeJ ) where

import CIS194.Homework07.JoinList ( JoinList(..) )
import CIS194.Homework07.Sized ( Sized, getSize, size )

sizeOf :: Sized b => JoinList b a -> Int
sizeOf = getSize . size

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ a) = Just a
indexJ i (Append _ jl1 jl2)
  | i < s1            = indexJ i jl1
  | otherwise         = indexJ (i - s1) jl2
  where s1            = sizeOf jl1
indexJ _ _            = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty       = Empty
dropJ n jl | 0 >= n = jl
dropJ _ Single {}   = Empty
dropJ n jl0@(Append _ jl1 jl2)
  | n >= s0         = Empty
  | n == s1         = jl2
  | n <  s1         = dropJ n jl1 <> jl2
  | otherwise       = dropJ (n - s1) jl2
  where
    s0 = sizeOf jl0
    s1 = sizeOf jl1

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty       = Empty
takeJ n _ | 0 >= n  = Empty
takeJ _ s@Single {} = s
takeJ n jl0@(Append _ jl1 jl2)
  | n >= s0         = jl0
  | n == s1         = jl1
  | n <  s1         = takeJ n jl1
  | otherwise       = jl1 <> jl2'
  where
    s0              = sizeOf jl0
    s1              = sizeOf jl1
    jl2'            = takeJ (n - s1) jl2
