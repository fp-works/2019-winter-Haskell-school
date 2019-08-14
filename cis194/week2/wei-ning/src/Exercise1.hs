{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}

module Exercise1
  ( parseMessage
  , parseMessages
  ) where

import qualified Log
import           Text.Parsec
import           Text.Parsec.String

parseMessages :: String -> [Log.LogMessage]
parseMessages s =
  case parse (parseLogMessage `endBy` newline) [] s of
    Right ls -> ls
    Left _   -> []

parseMessage :: String -> Log.LogMessage
parseMessage s =
  case parse parseLogMessage [] s of
    Right l -> l
    Left _  -> Log.Unknown s

parseLogMessage :: Parser Log.LogMessage
parseLogMessage =
    Log.LogMessage
    <$> skipSpace parseTypeToken skipMany1
    <*> skipSpace parseTimeStamp skipMany
    <*> manyTill anyChar (try . lookAhead $ newline)
  where
    skipSpace p s = p >>= \x -> s (char ' ') >> pure x

parseTypeToken :: Parser Log.MessageType
parseTypeToken =
      (char 'I' >> pure Log.Info)
  <|> (char 'W' >> pure Log.Warning)
  <|> fmap (Log.Error . read) (char 'E' >> many1 (char ' ') >> many1 digit)

parseTimeStamp :: Parser Log.TimeStamp
parseTimeStamp = read <$> many1 digit
