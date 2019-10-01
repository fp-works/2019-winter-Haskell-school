{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CIS194.Homework07.Scrabble ( Score(..), getScore ) where

newtype Score = Score Int
  deriving (Num, Eq, Show, Ord)

getScore :: Score -> Int
getScore (Score s) = s

instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mempty = Score 0
