module StringBufEditor where

import Buffer (fromString)
import StringBuffer (BufferString)
import Editor

fromBufferString :: String -> BufferString
fromBufferString = fromString

main :: IO ()
main = runEditor editor $ fromBufferString . unlines $
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
