{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Text.Read

rmi :: String -> Maybe Int
rmi a = readMaybe a :: Maybe Int

process :: String -> [String] -> String -> MessageType -> LogMessage
process a b d c = maybe (Unknown a) (\ts -> LogMessage c ts (unwords b)) (rmi d)

processError :: String -> [String] -> String -> String -> LogMessage
processError a b c d = maybe (Unknown a) (process a b d . Error) (rmi c)

parseMessage :: String -> LogMessage
parseMessage a = case words a of
  ("I":x:y) -> process a y x Info
  ("W":x:y) -> process a y x Warning
  ("E":c:x:y) -> processError a y c x 
  _ -> Unknown a

parse :: String -> [LogMessage]
parse = fmap parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert a Leaf = Node Leaf a Leaf
insert log1@(LogMessage _ ts1 _) (Node left log2@(LogMessage _ ts2 _) right)
  | ts1 > ts2 = Node left log2 (insert log1 right)
  | otherwise = Node (insert log1 left) log2 right
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left a right) = inOrder left ++ [a] ++ inOrder right

whatWentWrongNotSorted :: [LogMessage] -> [String]
whatWentWrongNotSorted [] = []
whatWentWrongNotSorted (LogMessage (Error severity) _ message: logMessages)
  | severity > 50 = [message] ++ whatWentWrong logMessages
  | otherwise = whatWentWrong logMessages
whatWentWrongNotSorted (_: logMessages) = whatWentWrong logMessages

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = whatWentWrongNotSorted . inOrder . build
