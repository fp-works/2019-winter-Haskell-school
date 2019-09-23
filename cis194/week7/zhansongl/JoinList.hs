{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module JoinList where

import Sized
import Buffer
import Scrabble

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty l = l
(+++) l Empty = l
(+++) l1 l2 = Append (tag l1 <> tag l2) l1 l2

instance Sized b => Sized (JoinList b a) where
  size Empty = 0
  size (Single _ _) = 1
  size (Append b _ _) = size b

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single _ a)
  | i == 0 = Just a
  | otherwise = Nothing
indexJ i (Append _ l1 l2) = case compare i s1 of
                              LT -> indexJ i l1
                              _  -> indexJ (i - s1) l2
  where s1 = getSize . size $ l1

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n jl@(Single _ _)
  | n <= 0 = jl
  | otherwise = Empty
dropJ n (Append _ jl1 jl2) = case compare n s1 of
                                  LT -> (dropJ n jl1) +++ jl2
                                  EQ -> jl2
                                  GT -> dropJ (n - s1) jl2
  where s1 = getSize . size $ jl1

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n jl@(Single _ _)
  | n > 0 = jl
  | otherwise = Empty
takeJ n (Append _ jl1 jl2) = case compare n s1 of
                                  LT -> (takeJ n jl1)
                                  EQ -> jl1
                                  GT -> jl1 +++ (takeJ (n - s1) jl2)
  where s1 = getSize . size $ jl1

scoreLine :: String -> JoinList Score String
scoreLine l = Single (scoreString l) l

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

makeSingle :: String -> JoinList (Score, Size) String
makeSingle l = Single (scoreString l, 1) l

instance Buffer (JoinList (Score, Size) String) where
  toString = unlines . jlToList

  fromString = foldr (+++) Empty . map makeSingle . lines

  line = indexJ

  replaceLine n l jl
    | n < 0 || n >= numLines jl = jl
    | otherwise = takeJ n jl +++ makeSingle l +++ dropJ (n+1) jl

  numLines = getSize . size

  value Empty = 0
  value (Single (v,_) _) = getScore v
  value (Append (v,_) _ _) = getScore v

