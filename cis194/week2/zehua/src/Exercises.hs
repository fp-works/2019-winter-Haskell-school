module Exercises where

import           Data.Maybe (mapMaybe)
import           Log
import           Text.Read  (readMaybe)

-- ex1
parseInt :: String -> Maybe TimeStamp
parseInt = readMaybe

createLogMessage :: MessageType -> String -> [String] -> Maybe LogMessage
createLogMessage mt ts msg =
  case parseInt ts of
    Just t  -> Just . LogMessage mt t . unwords $ msg
    Nothing -> Nothing

parseWords :: [String] -> Maybe LogMessage
parseWords ("I":t:xs)   = createLogMessage Info t xs
parseWords ("W":t:xs)   = createLogMessage Warning t xs
parseWords ("E":e:t:xs)
    | errno >= 1 && errno <= 100 = createLogMessage (Error errno) t xs
    | otherwise                  = Nothing
  where
    errno = read e :: Int
parseWords _            = Nothing

parseMessage :: String -> LogMessage
parseMessage s =
  case parseWords . words $ s of
    Just t  -> t
    Nothing -> Unknown s

parse :: String -> [LogMessage]
parse = fmap parseMessage . lines

-- ex2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert lm Leaf       = Node Leaf lm Leaf
insert lm@(LogMessage _ ts _ ) (Node l lm1@(LogMessage _ ts1 _) r)
  | ts < ts1 = Node (insert lm l) lm1 r
  | otherwise = Node l lm1 (insert lm r)
insert _ t           = t

-- ex3
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- ex4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf          = []
inOrder (Node l lm r) = inOrder l ++ [lm] ++ inOrder r

-- ex5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = mapMaybe getMessageMaybe . filter error50 . inOrder . build
  where
    error50 (LogMessage (Error s) _ _) = s >= 50
    error50 _                          = False
    getMessageMaybe (LogMessage _ _ m) = Just m
    getMessageMaybe _                  = Nothing

-- ex6
-- sort events and find the events happened before a particular log message
findEventsBefore :: String -> [LogMessage] -> [LogMessage]
findEventsBefore event = takeWhile (notseen event) . filter known . inOrder . build
  where
    notseen e (LogMessage _ _ m) = e /= m
    notseen _ _                  = False
    known (Unknown _) = False
    known _           = True

findEventsBeforeFromFile :: String -> String -> IO [LogMessage]
findEventsBeforeFromFile event file = findEventsBefore event . parse <$> readFile file

-- To use it, run `stack ghci`.
-- from ex5, we run the following to get the list of severe errors:
--  testWhatWentWrong parse whatWentWrong  "testdata/error.log" >>= mapM_ putStrLn
-- to print the list of events right before the first severe error:
--  fmap show <$> findEventsBeforeFromFile "Mustardwatch opened, please close for proper functioning!" "testdata/error.log" >>= mapM_ putStrLn
-- It looks like Alice (the girl in the wonderland) was being naughty and
-- touched the jar to open the mustardwatch?
