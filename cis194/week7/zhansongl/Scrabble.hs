{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scrabble where

import qualified Data.Map as Map
import qualified Data.Char as Char

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mempty = Score 0

getScore :: Score -> Int
getScore (Score i) = i

score :: Char -> Score
score c = Score $ Map.findWithDefault 0 (Char.toUpper c) m
  where m = Map.fromList scoreMap
        scoreMap = [ ('A', 1)
                   , ('B', 3)
                   , ('C', 3)
                   , ('E', 1)
                   , ('D', 2)
                   , ('G', 2)
                   , ('F', 4)
                   , ('I', 1)
                   , ('H', 4)
                   , ('K', 5)
                   , ('J', 8)
                   , ('M', 3)
                   , ('L', 1)
                   , ('O', 1)
                   , ('N', 1)
                   , ('Q', 10)
                   , ('P', 3)
                   , ('S', 1)
                   , ('R', 1)
                   , ('U', 1)
                   , ('T', 1)
                   , ('W', 4)
                   , ('V', 4)
                   , ('Y', 4)
                   , ('X', 8)
                   , ('Z', 10)
                   ]

scoreString :: String -> Score
scoreString = foldMap score

