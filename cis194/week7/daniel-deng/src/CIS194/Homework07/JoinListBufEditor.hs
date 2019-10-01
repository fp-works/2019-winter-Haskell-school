module CIS194.Homework07.JoinListBufEditor where

import CIS194.Homework07.Buffer
import CIS194.Homework07.Exercise04
import CIS194.Homework07.Editor (runEditor, editor)

main :: IO ()
main = runEditor editor $ buffer
     where
       buffer = (fromString prompt) :: JoinListBuffer
       prompt = unlines
                [ "This buffer is for notes you don't want to save, and for"
                , "evaluation of steam valve coefficients."
                , "To load a different file, type the character L followed"
                , "by the name of the file."
                ]
