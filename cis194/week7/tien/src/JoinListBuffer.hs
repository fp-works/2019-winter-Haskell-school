{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module JoinListBuffer where

import JoinList
  ( JoinList(Empty, Single)
  , (+++)
  , dropJ
  , indexJ
  , jlToList
  , tag
  , takeJ
  )
import Scrabble
import Sized

import Buffer

-- exercise 4 --
instance Buffer (JoinList (Score, Size) String) where
  toString = unlines . jlToList
  fromString = foldr ((+++) . formJointList) Empty . lines
    where
      formJointList = (\y -> (Single (scoreString y, Size 1) y))
  line = indexJ
  replaceLine n l jl
    | line n jl == Nothing = jl
    | otherwise =
      takeJ n jl +++ (Single (scoreString l, Size 1) l) +++ dropJ (n + 1) jl
  numLines = getSize . size . tag
  value = getScore . fst . tag
