{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scrabble where

import Data.Monoid
import Data.Char

newtype Score = Score Int
  deriving (Eq, Read, Show, Ord, Num)

instance Monoid Score where
  mempty  = Score 0

instance Semigroup Score where
  (<>) = (+)

score :: Char -> Score
-- attempt 1
score c 
  | toUpper c `elem` "AEILNORSTU" = Score 1
  | toUpper c `elem` "DG"         = Score 2
  | toUpper c `elem` "BCMP"       = Score 3
  | toUpper c `elem` "FHVWY"      = Score 4
  | toUpper c `elem` "U"          = Score 5
  | toUpper c `elem` "JX"         = Score 8
  | toUpper c `elem` "QZ"         = Score 10
  | otherwise                     = Score 0

-- 
scoreString :: String -> Score
-- attemp 1
-- scoreString = foldl (\x c -> x + score c) (Score 0)  

-- attemp 2
-- foldMap :: Monoid m => (a -> m) -> t a -> m
scoreString = foldMap score

