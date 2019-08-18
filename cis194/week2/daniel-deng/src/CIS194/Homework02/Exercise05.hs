module CIS194.Homework02.Exercise05 (whatWentWrong) where

import CIS194.Homework02.Log

import Data.List
import Data.Maybe

extract :: LogMessage -> Maybe (TimeStamp, String)
extract (LogMessage (Error sv) ts body)
  | 50 <= sv  = Just (ts, body)
  | otherwise = Nothing
extract _     = Nothing

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map snd . sortOn fst . mapMaybe extract
