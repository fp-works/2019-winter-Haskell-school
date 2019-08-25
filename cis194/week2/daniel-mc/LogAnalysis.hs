{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import           Log

-- Exercise 1

safeRead :: Read a => String -> Maybe a
safeRead x = case reads x of
  [(r, "")] -> Just r
  _         -> Nothing

parseMessageType :: [String] -> Maybe (MessageType, [String])
parseMessageType ("E" : (safeRead -> Just level) : rest) = Just (Error level, rest)
parseMessageType ("W" : rest) = Just (Warning, rest)
parseMessageType ("I" : rest) = Just (Info, rest)
parseMessageType _ = Nothing

parseTsMsg :: [String] -> Maybe (TimeStamp, String)
parseTsMsg ((safeRead -> Just ts) : rest) = Just (ts, unwords rest)
parseTsMsg _                              = Nothing

parseMessageWords :: [String] -> LogMessage
parseMessageWords (parseMessageType -> Just (msgType, parseTsMsg -> Just (ts, msg))) =
  LogMessage msgType ts msg
parseMessageWords msg = Unknown (unwords msg)

parseMessage :: String -> LogMessage
parseMessage = parseMessageWords . words

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- Exercise 2

insert :: LogMessage -> MessageTree -> MessageTree
insert msg@LogMessage{} Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ ts _) (Node left root@(LogMessage _ rootTs _) right)
  | ts < rootTs = Node (insert msg left) root right
  | otherwise   = Node left root (insert msg right)
insert _ mt = mt

-- Exercise 3

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                   = []
inOrder (Node left root right) = inOrder left ++ [root] ++ inOrder right

-- Exercise 5

isRelevantInfo :: LogMessage -> Bool
isRelevantInfo (LogMessage (Error level) _ _) = level >= 50
isRelevantInfo _                              = False

addMessageIfRelevant :: LogMessage -> [String] -> [String]
addMessageIfRelevant lm@(LogMessage _ _ msg) msgs | isRelevantInfo lm = msg : msgs
addMessageIfRelevant _ msgs = msgs

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = foldr addMessageIfRelevant [] . inOrder . build
