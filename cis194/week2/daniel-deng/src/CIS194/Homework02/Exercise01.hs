{-# LANGUAGE FlexibleContexts #-}

module CIS194.Homework02.Exercise01 (parseMessage, parseMessages) where

import qualified CIS194.Homework02.Log as Log

import           Data.Text             hiding (lines, map)
import           Text.Parsec
import           Text.Parsec.Text

skipSpaces :: Parser ()
skipSpaces = skipMany1 . char $ ' '

skipSpacesIfAny :: Parser ()
skipSpacesIfAny = skipMany . char $ ' '

digitsAfter :: Char -> Parser String
digitsAfter c = char c *> skipSpaces *> many1 digit

is :: Char -> a -> Parser a
is c t = char c >> pure t

msgTypeParser :: Parser Log.MessageType
msgTypeParser =
      'I' `is` Log.Info
  <|> 'W' `is` Log.Warning
  <|> Log.Error . read <$> digitsAfter 'E'

tsParser :: Parser Log.TimeStamp
tsParser = read <$> many1 digit

bodyParser :: Parser String
bodyParser = manyTill anyChar eof

logMsgParser :: Parser Log.LogMessage
logMsgParser =
  Log.LogMessage
  <$> msgTypeParser <* skipSpaces
  <*> tsParser <* skipSpacesIfAny
  <*> bodyParser

parseMessage :: String -> Log.LogMessage
parseMessage msgLine = case parse logMsgParser "" . pack $ msgLine of
                         Right logMsg -> logMsg
                         _            -> Log.Unknown msgLine

parseMessages :: String -> [Log.LogMessage]
parseMessages = fmap parseMessage . lines
