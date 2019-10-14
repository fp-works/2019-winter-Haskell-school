{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char


-- import Control.Arrow
import Data.Bifunctor

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

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
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

{-
-- first :: (a -> b) -> (a,c) -> (b,c)
-- first f (a,c) = (f a,c)

-- import Control.Arrow 
-- first :: a b c -> a (b, d) (c, d)
--
-- import Data.Bifunctor
-- first :: (a -> b) -> p a c -> p b c

class Functor f where  
    fmap :: (a -> b) -> f a -> f b  

  Functor law:
    fmap id = id
    fmap (g . h) = fmap g . fmap h
-}

instance Functor Parser where
  fmap f (Parser g) = Parser $ fmap (first f) . g


{-

Applicative Functor:

 class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

Applicative law:
  pure id <*> x = x
  pure (g x) = pure g <*> pure x
  x <*> pure y = pure (\g -> g y) <*> x
  x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
-}

instance Applicative Parser where
  pure x = Parser $ \s -> Just (x, s)
  Parser f <*> Parser g = Parser $ \s ->
                          case f s of
                            Nothing -> Nothing
                            Just (r, s') -> fmap (first r) . g $ s'
