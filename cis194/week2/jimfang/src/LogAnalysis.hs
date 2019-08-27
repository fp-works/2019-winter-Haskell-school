{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module LogAnalysis(
    parseMessage,
    parse
) where

import Text.Read ( readMaybe )
import Log

-- ex 1
parseMessage :: String -> LogMessage
parseMessage( words -> "I"
            : (readMaybe -> Just timestamp)
            : body ) = LogMessage Info timestamp $ unwords body
parseMessage( words -> "W"
            : (readMaybe -> Just timestamp)
            : body ) = LogMessage Warning timestamp $ unwords body            
parseMessage( words -> "E"
            : (readMaybe -> Just level)
            : (readMaybe -> Just timestamp)
            : body ) = LogMessage (Error level) timestamp $ unwords body            
parseMessage logLine = Unknown logLine

parse :: String -> [ LogMessage ]
parse = fmap parseMessage . lines

-- ex 2
insert :: LogMessage -> MessageTree -> MessageTree
insert m Leaf        = Node Leaf m Leaf
insert log1@(LogMessage _ ts1 _) (Node left log2@(LogMessage _ ts2 _) right)
  | ts1 > ts2 = Node left log2 (insert log1 right)
  | otherwise = Node (insert log1 left) log2 right
insert _ t = t

-- ex 3
build :: [ LogMessage ] -> MessageTree
build = foldr insert Leaf

-- ex 4
inOrder :: MessageTree -> [ LogMessage ]
inOrder Leaf = []
inOrder ( Node left a right ) = inOrder left ++ [ a ] ++ inOrder right

-- ex 5
whatWentWrongNotSorted :: [ LogMessage ] -> [ String ]
whatWentWrongNotSorted [] = []
whatWentWrongNotSorted (LogMessage (Error severity) _ message: logMessages)
                    | severity > 50 = [ message ] ++ whatWentWrong logMessages
                    | otherwise = whatWentWrong logMessages
whatWentWrongNotSorted ( _ : logMessages ) = whatWentWrong logMessages


whatWentWrong :: [ LogMessage ] -> [ String ]
whatWentWrong = whatWentWrongNotSorted . inOrder . build