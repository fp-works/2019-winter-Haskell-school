{-# OPTIONS_GHC -Wall #-}

module JoinListEditor where

import JoinList
import Editor
import Scrabble
import Sized
import Buffer

main :: IO ()
main = runEditor editor defBuf
  where defBuf :: JoinList (Score, Size) String
        defBuf = fromString . unlines $
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
