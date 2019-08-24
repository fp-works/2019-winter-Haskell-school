-- Week 2 Exercise: https://www.seas.upenn.edu/~cis194/spring13/hw/02-ADTs.pdf

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage message = case words message of
                         "E":n:t:m -> LogMessage (Error (read n)) (read t) (unwords m)
                         "W":t:m   -> LogMessage Warning (read t) (unwords m)
                         "I":t:m   -> LogMessage Info (read t) (unwords m)
                         m         -> Unknown (unwords m)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert logMessage@(LogMessage _ t1 _) (Node l current@(LogMessage _ t2 _) r)
  | t1 < t2 = Node (insert logMessage l) current r
  | otherwise = Node l current (insert logMessage r)
insert logMessage Leaf = Node Leaf logMessage Leaf
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder (Node l current r) = inOrder l ++ [current] ++ inOrder r
inOrder _ = []

severity :: Int -> LogMessage -> Bool
severity level (LogMessage (Error l) _ _) = l >= level
severity _ _ = False

highSeverity :: LogMessage -> Bool
highSeverity = severity 50

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ m) = m
getMessage _ = ""

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMessage . filter highSeverity . inOrder . build
