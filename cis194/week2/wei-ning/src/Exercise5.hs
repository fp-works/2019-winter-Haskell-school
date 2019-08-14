-- perl -wnl -E '/Error\s+(\d+)/ or next; say $_ if $1 >= 50' /var/tmp/sut/prt.txt

module Exercise5
  ( whatWentWrong
  ) where

import           Log
import           Prelude hiding (Error)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = filterMessages (Error 50)

filterMessages :: MessageType -> [LogMessage] -> [String]
filterMessages (Error lv) =
  map extractMessage . filter isCriticalError
  where
    isCriticalError (LogMessage (Error lv') _ _) = lv' >= lv
    isCriticalError _                            = False
filterMessages t =
  map extractMessage . filter (\(LogMessage t' _ _) -> t' == t)

extractMessage :: LogMessage -> String
extractMessage (LogMessage _ _ msg) = msg
