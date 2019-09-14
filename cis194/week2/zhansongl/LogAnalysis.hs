{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Prelude hiding(log)

import Log

import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
-- need lens
import Control.Lens ((<&>))

type Parser = Parsec Void String

messageTypeParser :: Parser MessageType
messageTypeParser = do
  charType <- oneOf "IEW"
  case charType of
       'I' -> pure Info
       'W' -> pure Warning
       'E' -> space1 *> L.decimal <&> Error
       _   -> undefined

messageParser :: Parser LogMessage
messageParser = LogMessage
            <$> (messageTypeParser <* space1)
            <*> (L.decimal <* space)
            <*> (many asciiChar)

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
       _ -> undefined

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left log right) = (inOrder left) ++ (log : (inOrder right))

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logs = map getMessage $ filter isRelevant sortedLogs
  where sortedLogs = inOrder (build logs)
        isRelevant (LogMessage (Error ec) _ _) = ec >= 50
        isRelevant _ = False
        getMessage (LogMessage _ _ s) = s
        getMessage (Unknown s) = s

