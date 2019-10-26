{-# OPTIONS_GHC -Wall #-}

{- CIS 194 HW 10
-}
module AParser where

import Control.Applicative
import Control.Monad

import Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a =
  Parser
    { runParser :: String -> Maybe (a, String)
    }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing -- fail on the empty input
    f (x:xs) -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
      | p x = Just (x, xs)
      | otherwise = Nothing -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}
-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns = Nothing
      | otherwise = Just (read ns, rest)
      where
        (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------
-- exercise 1 --
first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

instance Functor Parser where
  fmap f (Parser a) = Parser $ fmap (first f) . a

-- exercise 2 --
instance Applicative Parser where
  pure a = Parser x
    where
      x y = Just (a, y)
  Parser f <*> Parser a = Parser b
    where
      b xs =
        case f xs of
          Just (_, s) -> first <$> fmap fst (f xs) <*> a s
          Nothing -> Nothing

-- exercise 3 --
-- 3.1 --
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

-- 3.2 --
abParser_ :: Parser ()
abParser_ = void abParser

-- 3.3 --
intPair :: Parser [Integer]
intPair = f <$> posInt <* char ' ' <*> posInt
  where
    f x y = x : y : []

-- exercise 4 --
instance Alternative Parser where
  empty = Parser a
    where
      a _ = Nothing
  Parser a <|> Parser b = Parser c
    where
      c xs =
        case a xs of
          Nothing -> b xs
          Just (_, _) -> a xs

-- exercise 5 --
intOrUppercase :: Parser ()
intOrUppercase = void $ (const ' ' <$> posInt) <|> satisfy isUpper
