{-# OPTIONS_GHC -Wall #-}

module AParser where

import Data.Char (isDigit, isUpper)
import Control.Applicative (Alternative(..))
import Control.Monad (void)

newtype Parser a
  = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where f [] = Nothing    -- fail on the empty input
        f (x:xs)          -- check if x satisfies the predicate
                          -- if so, return x along with the remainder
                          -- of the input (that is, xs)
            | p x       = Just (x, xs)
            | otherwise = Nothing  -- otherwise, fail

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
  where f xs
          | null ns = Nothing
          | otherwise = Just (read ns, rest)
          where (ns, rest) = span isDigit xs

first :: (a -> b) -> (a, c) -> (b, c)
first f p = (f . fst $ p, snd p)

instance Functor Parser where
  fmap f (Parser g) = Parser $ fmap (first f) . g
  (<$) a (Parser b) = Parser $ fmap (first $ const a) . b

instance Applicative Parser where
  pure a = Parser $ Just . ((,) a)
  (<*>) (Parser pf) (Parser pa) = Parser $ \s -> pf s >>= \(f, s') -> first f <$> pa s'

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = () <$ abParser

intPair :: Parser [Integer]
intPair = (\x y -> [x, y]) <$> posInt <* char ' ' <*> posInt

instance Alternative Parser where
  empty = Parser $ const Nothing
  (<|>) (Parser a) (Parser a') = Parser $ \s -> a s <|> a' s

intOrUppercase :: Parser ()
intOrUppercase = void posInt <|> void (satisfy isUpper)
