module Main where

import Editor
import JoinList (JoinList(Empty, Single), (+++))
import JoinListBuffer
import Scrabble (scoreString)
import Sized

main =
  runEditor editor $
  foldr (+++) Empty $
  fmap
    (\x -> Single (scoreString x, Size 1) x)
    [ "This buffer is for notes you don't want to save, and for"
    , "evaluation of steam valve coefficients."
    , "To load a different file, type the character L followed"
    , "by the name of the file."
    ]
