module CIS194.Homework02.Log where

import Control.Applicative

type TimeStamp   = Int
type Severity    = Int

data MessageType = Info
                 | Warning
                 | Error Severity
                 deriving (Show, Eq)

data LogMessage  = LogMessage MessageType TimeStamp String
                 | Unknown String
                 deriving (Show, Eq)

data MessageTree = Leaf
                 | Node MessageTree LogMessage MessageTree
                 deriving (Show, Eq)
