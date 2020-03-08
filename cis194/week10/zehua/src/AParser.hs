module AParser where

import           Control.Applicative
import           Control.Monad       (join)
import           Data.Char

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

-- ex1
first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  -- fmap :: (a -> b) -> (String -> Maybe (a, String)) -> (String -> Maybe (b, String))
  fmap f pa = Parser $ fmap (first f) . runParser pa
  -- fmap f = Parser . (.) (fmap (first f)) . runParser
  -- fmap f = Parser . (fmap . fmap . first) f . runParser


-- ex2
instance Applicative Parser where
  -- pure :: a -> Parser a
  pure a = Parser (\s -> Just (a, s))
  -- simplified version using Parser Functor with Maybe Applicative
  pa <*> pb = Parser pc
    where
      pc = join . liftA2 fb (pure pb) . runParser pa
      fb :: (Parser b) -> (b -> c, String) -> Maybe (c, String)
      fb p (fa, sa) = runParser (fmap fa p) sa
  {- using Parser Functor with Maybe Applicative
  pa <*> pb = Parser pc
    where
      pc s = oa
        where
          ra = runParser pa s
          -- using join to flatten the nested Maybe
          oa = join (liftA2 fb (pure pb) ra)
          fb :: (Parser b) -> (b -> c, String) -> Maybe (c, String)
          fb p (fa, sa) = runParser (fmap fa p) sa
  -}
  {- using Parser Functor and Maybe Monad, but we haven't learned Monad, yet :)
  pa <*> pb = Parser pc
    where
      pc s = runParser pa s >>= fb
      fb (fa, sa) = runParser (fmap fa pb) sa
  -}
  {- using Parser Functor and Maybe Monad with do notation, probably the simplest
  pa <*> pb = Parser pc
    where
      pc s = do
        (fa, sa) <- runParser pa s
        runParser (fmap fa pb) sa
  -}
  {- using Parser Functor and manual Maybe mapping / flattening
  pa <*> pb = Parser pc
    where
      pc s = oa
        where
          ra = runParser pa s
          oa = case ra of
                 Nothing -> Nothing
                 Just (fa, sa) -> runParser (fmap fa pb) sa
  -}
  {- without using Parser Functor
  pa <*> pb = Parser pc
    where
      pc s = oa
        where
          ra = runParser pa s
          oa = case ra of
                 Nothing -> Nothing
                 Just (fa, sa) -> ob
                   where
                     rb = runParser pb sa
                     ob = case rb of
                            Nothing -> Nothing
                            Just (vb, sb) -> Just (fa vb, sb)
  -}


-- ex3
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = const () <$> abParser

createIntPair :: Integer -> Char -> Integer -> [Integer]
createIntPair i1 _ i2 = [i1, i2]

intPair :: Parser [Integer]
intPair = createIntPair <$> posInt <*> char ' ' <*> posInt


-- ex4
instance Alternative Parser where
  empty = Parser (const Nothing)
  pa <|> pb = Parser pc
    where
      pc s = runParser pa s <|> runParser pb s
  {- not sure if this Applicative Functor way of free version is more readable
  pa <|> pb = Parser $ (<|>) <$> runParser pa <*> runParser pb
  -}


-- ex5
intOrUppercase :: Parser ()
intOrUppercase = const () <$> posInt <|> const () <$> satisfy isUpper
