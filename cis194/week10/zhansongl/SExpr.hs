{-# OPTIONS_GHC -Wall #-}

module SExpr where

import Prelude hiding (sequenceA)
import Control.Applicative (Applicative(..), Alternative(..))

import AParser (Parser(..), satisfy, posInt, char)

import Data.Char (isSpace, isAlpha, isAlphaNum)

-- the following \w+A functions are not part of the homework
-- they are questions at the end of lecture note
sequenceA :: Applicative f => [f a] -> f [a]
sequenceA = foldr (\fa r -> (:) <$> fa <*> r) (pure [])

mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA f = sequenceA . fmap f

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA 0 _ = pure []
replicateA n a = (:) <$> a <*> replicateA (n-1) a

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = (:) <$> (satisfy isAlpha) <*> zeroOrMore (satisfy isAlphaNum)

type Ident = String
data Atom = N Integer | I Ident
  deriving (Show, Eq)
data SExpr = A Atom | Comb [SExpr]
  deriving (Show, Eq)

parseAtom :: Parser Atom
parseAtom = spaces *> atom <* spaces
  where atom = (I <$> ident) <|> (N <$> posInt)

parseSExpr :: Parser SExpr
parseSExpr = A <$> parseAtom
             <|>
             openParenthesis *> (Comb <$> oneOrMore parseSExpr) <* closeParenthesis
  where openParenthesis = spaces *> char '('
        closeParenthesis = char ')' <* spaces
