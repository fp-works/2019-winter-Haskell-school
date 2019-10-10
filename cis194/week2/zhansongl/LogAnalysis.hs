{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

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
parse = (fmap parseMessage) . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert l Leaf = Node Leaf l Leaf
insert _ l'@(Node _ (Unknown _) _) = l' -- l' is malformed, nothing we can do here
insert l@(LogMessage _ t _) (Node left l'@(LogMessage _ t' _) right) =
  case compare t t' of
    LT -> Node (insert l left) l' right
    _  -> Node left l' (insert l right)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left l right) = (inOrder left) ++ (l : (inOrder right))

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = fmap getMessage . filter isRelevant . inOrder . build
  where isRelevant (LogMessage (Error ec) _ _) = ec >= 50
        isRelevant _ = False
        getMessage (LogMessage _ _ s) = s
        getMessage (Unknown s) = s

