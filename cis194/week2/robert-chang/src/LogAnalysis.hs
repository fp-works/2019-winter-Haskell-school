{-# LANGUAGE ViewPatterns, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where
import Text.Read ( readMaybe )
import Log

-- Exercise 1 Solution 1
{-
safeIntReader :: String -> Maybe Int
safeIntReader = readMaybe

getIntFromJust :: String -> Int 
getIntFromJust s = fromJust . safeIntReader $ s 

isJustValue :: String -> Bool
isJustValue s = isJust . safeIntReader $ s

parseMessage :: String -> LogMessage
parseMessage s = case words s of 
    ("E": level : timestamp : msg)  -> 
      if (and [isJustValue level, isJustValue timestamp])
      then LogMessage (Error . getIntFromJust $ level) (getIntFromJust timestamp) (unwords msg)
      else Unknown s
    ("I": timestamp : msg) ->
      if (isJustValue timestamp)
      then LogMessage Info (getIntFromJust timestamp) (unwords msg)
      else Unknown s
    ("W": timestamp : msg) ->
      if (isJustValue timestamp)
      then LogMessage Warning (getIntFromJust timestamp) (unwords msg)
      else Unknown s
    (_) -> Unknown s
-}

-- Exercise 1 Solution 2
-- Mind blowing...
parseMessage :: String -> LogMessage
parseMessage ( words -> "I"
             : (readMaybe -> Just timestamp)
             : body ) = LogMessage Info timestamp $ unwords body
parseMessage ( words -> "W"
             : (readMaybe -> Just timestamp)
             : body ) = LogMessage Warning timestamp $ unwords body
parseMessage ( words -> "E"
             : (readMaybe -> Just level)
             : (readMaybe -> Just timestamp)
             : body ) = LogMessage (Error level) timestamp $ unwords body
parseMessage logLine  = Unknown logLine


parse :: String -> [LogMessage]
parse [] = []
parse file = map parseMessage $ lines file



-- Exercise 2

insert :: LogMessage -> MessageTree -> MessageTree
{- if insert is given a LogMessage which is
Unknown, it should return the MessageTree unchanged
-}
{- insert to empty tree, should have LogMessage inserted -} 
insert x Leaf        = Node Leaf x Leaf 
insert x@(LogMessage _ t1 _) (Node left xs@(LogMessage _ t2 _) right) 
  | t1 == t2 = Node left xs right
  | t1 < t2  = Node (insert x left) xs right
  | t1 > t2  = Node left xs (insert x right)

insert _ tree = tree

-- Exercise 3
build :: [LogMessage] -> MessageTree
-- foldr :: (a -> b -> b) -> b -> t a -> b
-- e.g.
-- foldr f z []     = z 
-- foldr f z (x:xs) = f x (foldr f z xs)
-- In this case:
-- foldr insert Leaf [x1, x2, ..., xn] == x1 `insert` (x2 `insert` ... (xn `insert` Leaf)...)
build = foldr insert Leaf 

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left x right) = inOrder left ++ [x] ++ inOrder right

-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong =  fmap getMsg . inOrder . build . filter . severity $ 50

severity :: Int -> LogMessage -> Bool
severity level (LogMessage (Error lvl) _ _)
  | lvl >= level = True
  | otherwise = False
severity _ _ = False

getMsg :: LogMessage -> String
getMsg (LogMessage (Error _) _ msg) = msg
getMsg _ = undefined
