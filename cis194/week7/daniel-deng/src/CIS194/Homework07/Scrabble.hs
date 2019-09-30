{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CIS194.Homework07.Scrabble where

newtype Score = Score Int
  deriving (Num, Eq, Show, Ord)

instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mempty = Score 0
