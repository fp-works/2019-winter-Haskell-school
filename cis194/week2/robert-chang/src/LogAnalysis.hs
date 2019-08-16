{-# LANGUAGE ViewPatterns, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where
import Text.Read ( readMaybe )
import Log

-- Exercise 1 Solution 1
{-
safeIntReader :: String -> Maybe Int
safeIntReader = readMaybe

getIntFromJust :: String -> Int 
getIntFromJust s = fromJust . safeIntReader $ s 

isJustValue :: String -> Bool
isJustValue s = isJust . safeIntReader $ s

parseMessage :: String -> LogMessage
parseMessage s = case words s of 
    ("E": level : timestamp : msg)  -> 
      if (and [isJustValue level, isJustValue timestamp])
      then LogMessage (Error . getIntFromJust $ level) (getIntFromJust timestamp) (unwords msg)
      else Unknown s
    ("I": timestamp : msg) ->
      if (isJustValue timestamp)
      then LogMessage Info (getIntFromJust timestamp) (unwords msg)
      else Unknown s
    ("W": timestamp : msg) ->
      if (isJustValue timestamp)
      then LogMessage Warning (getIntFromJust timestamp) (unwords msg)
      else Unknown s
    (_) -> Unknown s
-}

-- Exercise 1 Solution 2
-- Mind blowing...
parseMessage :: String -> LogMessage
parseMessage ( words -> "I"
             : (readMaybe -> Just timestamp)
             : body ) = LogMessage Info timestamp $ unwords body
parseMessage ( words -> "W"
             : (readMaybe -> Just timestamp)
             : body ) = LogMessage Warning timestamp $ unwords body
parseMessage ( words -> "E"
             : (readMaybe -> Just level)
             : (readMaybe -> Just timestamp)
             : body ) = LogMessage (Error level) timestamp $ unwords body
parseMessage logLine  = Unknown logLine


parse :: String -> [LogMessage]
parse [] = []
parse file = map parseMessage $ lines file

