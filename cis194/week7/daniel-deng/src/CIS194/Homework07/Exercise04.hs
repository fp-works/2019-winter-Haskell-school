{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module CIS194.Homework07.Exercise04 where

import CIS194.Homework07.Buffer     ( Buffer(..) )
import CIS194.Homework07.Scrabble   ( Score(..), getScore )
import CIS194.Homework07.Sized      ( Size(..), getSize, size )
import CIS194.Homework07.JoinList   ( JoinList(..), tag )
import CIS194.Homework07.Exercise02 ( indexJ, dropJ, takeJ )
import CIS194.Homework07.Exercise03 ( scoreString )

type JoinListBuffer = JoinList (Score, Size) String

jlToList :: Monoid m => JoinList m a -> [a]
jlToList Empty              = []
jlToList (Single _ a)       = [a]
jlToList (Append _ jl1 jl2) = jlToList jl1 ++ jlToList jl2

instance Buffer JoinListBuffer where

  -- | Convert a buffer to a String.
  toString = unlines . jlToList

  -- | Create a buffer from a String.
  fromString = mconcat . fmap toSingle . lines
             where
               toSingle = flip Single <*> annotations
               annotations s = (scoreString s,  Size 1)

  -- | Extract the nth line (0-indexed) from a buffer.  Return Nothing
  -- for out-of-bounds indices.
  line = indexJ

  -- | @replaceLine n ln buf@ returns a modified version of @buf@,
  --   with the @n@th line replaced by @ln@.  If the index is
  --   out-of-bounds, the buffer should be returned unmodified.
  replaceLine n s jl
    | outOfBounds = jl
    | otherwise   = takeJ n jl <> fromString s <> dropJ (n + 1) jl
    where
      outOfBounds = 0 > n || n >= numLines jl

  -- | Compute the number of lines in the buffer.
  numLines = getSize . size

  -- | Compute the value of the buffer, i.e. the amount someone would
  --   be paid for publishing the contents of the buffer.
  value = getScore . fst . tag
