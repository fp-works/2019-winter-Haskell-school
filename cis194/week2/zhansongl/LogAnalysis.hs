module LogAnalysis where

import Log

import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

type Parser = Parsec Void String

messageTypeParser :: Parser MessageType
messageTypeParser = do
  charType <- oneOf "IEW"
  case charType of
       'I' -> return Info
       'W' -> return Warning
       'E' -> do 
          space1
          errorCode <- L.decimal
          return (Error errorCode)

messageParser :: Parser LogMessage
messageParser = do
  messageType <- messageTypeParser
  space1
  timeStamp <- L.decimal
  space
  message <- many asciiChar
  return $ LogMessage messageType timeStamp message

parseMessage :: String -> LogMessage
parseMessage s = case (runParser messageParser "" s) of
                      Left _ -> Unknown s
                      Right m -> m

parse :: String -> [LogMessage]
parse = (map parseMessage) . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert log@(LogMessage _ t _) tree =
  case tree of
       Leaf -> Node Leaf log Leaf
       (Node left log'@(LogMessage _ t' _) right) ->
         if t < t'
         then (Node (insert log left) log' right)
         else (Node left log' (insert log right))

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left log right) = (inOrder left) ++ (log : (inOrder right))

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logs = map getMessage $ filter isRelevant sortedLogs
  where sortedLogs = inOrder (build logs)
        isRelevant log@(LogMessage (Error ec) _ _) = ec >= 50
        isRelevant _ = False
        getMessage (LogMessage _ _ s) = s

