{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module LogAnalysis(
    parseMessage,
    parse
) where

import Text.Read ( readMaybe )
import Log

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