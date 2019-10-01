{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scrabble where

import Data.Char

newtype Score =
  Score Int
  deriving (Num, Eq, Show, Ord)

getScore :: Score -> Int
getScore (Score x) = x

instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

score :: Char -> Score
score c
  | toUpper c == 'A' = Score 1
  | toUpper c == 'B' = Score 3
  | toUpper c == 'C' = Score 3
  | toUpper c == 'D' = Score 2
  | toUpper c == 'E' = Score 1
  | toUpper c == 'F' = Score 4
  | toUpper c == 'G' = Score 2
  | toUpper c == 'H' = Score 4
  | toUpper c == 'I' = Score 1
  | toUpper c == 'J' = Score 8
  | toUpper c == 'K' = Score 5
  | toUpper c == 'L' = Score 1
  | toUpper c == 'M' = Score 3
  | toUpper c == 'N' = Score 1
  | toUpper c == 'O' = Score 1
  | toUpper c == 'P' = Score 3
  | toUpper c == 'Q' = Score 10
  | toUpper c == 'R' = Score 1
  | toUpper c == 'S' = Score 1
  | toUpper c == 'T' = Score 1
  | toUpper c == 'U' = Score 1
  | toUpper c == 'V' = Score 4
  | toUpper c == 'W' = Score 4
  | toUpper c == 'X' = Score 8
  | toUpper c == 'Y' = Score 4
  | toUpper c == 'Z' = Score 10
  | otherwise = Score 0

scoreString :: String -> Score
scoreString = foldr ((<>) . score) (Score 0)
