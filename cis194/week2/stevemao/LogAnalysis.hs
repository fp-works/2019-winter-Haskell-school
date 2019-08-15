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

-- "I 29 la la la"
parseMessage :: String -> LogMessage
parseMessage a = case words a of
  ("I":x:y) -> process a y x Info
  ("W":x:y) -> process a y x Warning
  ("E":c:x:y) -> processError a y c x 
  _ -> Unknown a

parse :: String -> [LogMessage]
parse = fmap parseMessage . lines
