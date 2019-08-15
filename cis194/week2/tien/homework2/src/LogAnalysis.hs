{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import           Log
import           Text.Read (readMaybe)

parseMessage :: String -> LogMessage
parseMessage logStr =
  case words logStr of
    ("I":ts:rest) ->
      case readMaybe ts :: Maybe Int of
        Just i -> LogMessage Info i (unwords rest)
        _      -> Unknown logStr
    ("W":ts:rest) ->
      case readMaybe ts :: Maybe Int of
        Just i -> LogMessage Warning i (unwords rest)
        _      -> Unknown logStr
    ("E":lv:ts:rest) ->
      case (readMaybe lv :: Maybe Int, readMaybe ts :: Maybe Int) of
        (Just i, Just j) -> LogMessage (Error i) j (unwords rest)
        _                -> Unknown logStr
    _ -> Unknown logStr

parse :: String -> [LogMessage]
parse = fmap parseMessage . lines
