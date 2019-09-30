module CIS194.Homework07.Exercise03 ( scoreString ) where

import CIS194.Homework07.Scrabble ( Score(..) )

import Data.Char

import Data.Map ( findWithDefault )
import qualified Data.Map as M

score :: Char -> Score
score c = Score
        . findWithDefault 0 (toUpper c)
        . M.fromList
        $ [ ('A', 1)  , ('B', 3)
          , ('C', 3)  , ('D', 2)
          , ('E', 1)  , ('F', 4)
          , ('G', 2)  , ('H', 4)
          , ('I', 1)  , ('J', 8)
          , ('K', 5)  , ('L', 1)
          , ('M', 3)  , ('N', 1)
          , ('O', 1)  , ('P', 3)
          , ('Q', 10) , ('R', 1)
          , ('S', 1)  , ('T', 1)
          , ('U', 1)  , ('V', 4)
          , ('W', 4)  , ('X', 8)
          , ('Y', 4)  , ('Z', 10) ]

scoreString :: String -> Score
scoreString = mconcat . fmap score
