module CIS194.Homework07.StringBufEditor where

import CIS194.Homework07.StringBuffer ()
import CIS194.Homework07.Editor (runEditor, editor)

main :: IO ()
main = runEditor editor $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
