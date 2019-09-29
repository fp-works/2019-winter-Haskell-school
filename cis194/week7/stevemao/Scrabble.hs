{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where
import Data.Char
import Buffer
import JoinList
import Sized
import Editor

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mempty = Score 0

score :: Char -> Score
score a
  | toUpper a `elem` ['A', 'E', 'I', 'L', 'N', 'O', 'R', 'S', 'T', 'U'] = Score 1
  | toUpper a `elem` ['D', 'G'] = Score 2
  | toUpper a `elem` ['B', 'C', 'M', 'P'] = Score 3
  | toUpper a `elem` ['F', 'H', 'V', 'W', 'Y'] = Score 4
  | toUpper a == 'K' = Score 5
  | toUpper a `elem` ['J', 'X'] = Score 8
  | toUpper a `elem` ['Q', 'Z'] = Score 10
score _ = Score 0

scoreString :: String -> Score
scoreString = foldMap score

getScore :: Score -> Int
getScore (Score i) = i

instance Buffer (JoinList (Score, Size) String) where
  toString Empty = mempty
  toString (Single _ a) = a
  toString (Append _ a b) = toString a <> "\n" <> toString b
  
  fromString = foldr (+++) Empty . fmap (\l -> Single (scoreString l, Size 1) l) . lines
  
  line = indexJ
  
  replaceLine _ _ Empty = Empty
  replaceLine i s l@(Single (_, si) _)
    | i == 0 = Single (scoreString s, si) s
    | otherwise = l
  replaceLine i s (Append _ a b)
    | i < sizeA = replaceLine i s a +++ b
    | otherwise = a +++ replaceLine (i - sizeA) s b
    where sizeA = getIntSize a

  numLines Empty = 0
  numLines (Single (_, si) _) = getIntSize si
  numLines (Append (_, si) _ _) = getIntSize si
  
  value Empty = 0
  value (Single (sc, _) _) = getScore sc
  value (Append (sc, _) _ _) = getScore sc

jl :: JoinList (Score, Size) String
jl = fromString . unlines $
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]

main :: IO ()
main = runEditor editor jl
