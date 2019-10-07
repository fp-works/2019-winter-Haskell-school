{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module Scrabble
  ( Score(..)
  , score
  , scoreString
  , scoreLine
  )
where

import           Data.Char
import           Buffer
import           Sized
import           Editor
import           JoinList

-- Exercise 3

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score x) = x

instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mempty = Score 0

score :: Char -> Score
score (toUpper -> 'A') = 1
score (toUpper -> 'B') = 3
score (toUpper -> 'C') = 3
score (toUpper -> 'D') = 2
score (toUpper -> 'E') = 1
score (toUpper -> 'F') = 4
score (toUpper -> 'G') = 2
score (toUpper -> 'H') = 4
score (toUpper -> 'I') = 1
score (toUpper -> 'J') = 8
score (toUpper -> 'K') = 5
score (toUpper -> 'L') = 1
score (toUpper -> 'M') = 3
score (toUpper -> 'N') = 1
score (toUpper -> 'O') = 1
score (toUpper -> 'P') = 3
score (toUpper -> 'Q') = 10
score (toUpper -> 'R') = 1
score (toUpper -> 'S') = 1
score (toUpper -> 'T') = 1
score (toUpper -> 'U') = 1
score (toUpper -> 'V') = 4
score (toUpper -> 'W') = 4
score (toUpper -> 'X') = 8
score (toUpper -> 'Y') = 4
score (toUpper -> 'Z') = 10
score _                = 0

scoreString :: String -> Score
scoreString = foldMap score

scoreLine :: String -> JoinList Score String
scoreLine x = Single (scoreString x) x

-- Exercise 4
scoreSizeLine :: String -> JoinList (Score, Size) String
scoreSizeLine x = Single (scoreString x, Size 1) x

instance Buffer (JoinList (Score, Size) String) where
  toString Empty          = ""
  toString (Single _ a  ) = a
  toString (Append _ l r) = toString l ++ "\n" ++ toString r

  fromString = foldr ((+++) . scoreSizeLine) Empty . lines

  line       = indexJ

  replaceLine n l j | n < numLines j = takeJ n j +++ scoreSizeLine l +++ dropJ (n + 1) j
                    | otherwise      = j

  numLines = getSize . size

  value    = getScore . fst . tag

initialBuffer :: JoinList (Score, Size) String
initialBuffer = fromString "Type the character L followed the name of a file."

main :: IO ()
main = runEditor editor initialBuffer
