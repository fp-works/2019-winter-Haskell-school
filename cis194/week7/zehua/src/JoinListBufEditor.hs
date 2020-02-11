module JoinListBufEditor where

import Buffer (fromString)
import JoinListBuffer (BufferJoinList)
import Editor

fromBufferJoinList :: String -> BufferJoinList
fromBufferJoinList = fromString

main :: IO ()
main = runEditor editor $ fromBufferJoinList . unlines $
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
