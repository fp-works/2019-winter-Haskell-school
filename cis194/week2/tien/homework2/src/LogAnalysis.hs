{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ViewPatterns #-}

module LogAnalysis where

import           Log
import           Text.Read (readMaybe)

-- Exercise 1 --
-- parseMessage :: String -> LogMessage
-- parseMessage logStr =
--   case words logStr of
--     ("I":ts:rest) ->
--       case readMaybe ts :: Maybe Int of
--         Just i -> LogMessage Info i (unwords rest)
--         _      -> Unknown logStr
--     ("W":ts:rest) ->
--       case readMaybe ts :: Maybe Int of
--         Just i -> LogMessage Warning i (unwords rest)
--         _      -> Unknown logStr
--     ("E":lv:ts:rest) ->
--       case (readMaybe lv :: Maybe Int, readMaybe ts :: Maybe Int) of
--         (Just i, Just j) -> LogMessage (Error i) j (unwords rest)
--         _                -> Unknown logStr
--     _ -> Unknown logStr
-- Use ViewPatterns --
parseMessage :: String -> LogMessage
parseMessage (words -> "I":(readMaybe -> Just ts):msg) =
  LogMessage Info ts (unwords msg)
parseMessage (words -> "W":(readMaybe -> Just ts):msg) =
  LogMessage Warning ts (unwords msg)
parseMessage (words -> "E":(readMaybe -> Just lvl):(readMaybe -> Just ts):msg) =
  LogMessage (Error lvl) ts (unwords msg)
parseMessage logStr = Unknown logStr

parse :: String -> [LogMessage]
parse = fmap parseMessage . lines

-- Exercise 2 --
constrcutTreeByTs ::
     Ord a
  => a
  -> a
  -> LogMessage
  -> LogMessage
  -> MessageTree
  -> MessageTree
  -> MessageTree
constrcutTreeByTs ts1 ts2 log1 log2 left right
  | ts1 > ts2 = Node left log2 (insert log1 right)
  | ts1 < ts2 = Node (insert log1 left) log2 right
  | otherwise = Node left log2 right

insert :: LogMessage -> MessageTree -> MessageTree
insert log1 Leaf = Node Leaf log1 Leaf
insert log1@(LogMessage _ ts1 _) (Node left log2@(LogMessage _ ts2 _) right) =
  constrcutTreeByTs ts1 ts2 log1 log2 left right
insert _ msgTree = msgTree

-- Exercise 3 --
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4 --
inOrder :: MessageTree -> [LogMessage]
inOrder (Node left log2 right) = inOrder left ++ [log2] ++ inOrder right
inOrder _                      = []

--Exercise 5 --
getErrorLvlOver50 :: LogMessage -> Bool
getErrorLvlOver50 (LogMessage (Error lvl) _ _) = lvl > 50
getErrorLvlOver50 _                            = False

getErrorDescription :: LogMessage -> String
getErrorDescription (LogMessage (Error _) _ description) = description
getErrorDescription _                                    = ""

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong =
  map getErrorDescription . filter getErrorLvlOver50 . inOrder . build
