module SExpr where

import           AParser
import           Control.Applicative
import           Data.Char           (isAlpha, isAlphaNum, isSpace)

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
-- use `<|> pure []` to stop whenever we could not match further
zeroOrMore p = (:) <$> p <*> zeroOrMore p <|> pure []
{-
zeroOrMore p = pp
  where
    pp = Parser p0
    p0 s = case (runParser p s) of
             Nothing -> Just ([], s)
             Just (a, s1) -> runParser (fmap (a:) pp) s1
-}

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p
{-
-- making it free of `p` using Applicative Functor :D
oneOrMore = (<*>) <$> (<$>) (:) <*> zeroOrMore
-}

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving (Eq, Show)

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving (Eq, Show)


-- posInt but with look ahead to ensure that it's not followed by alpha
posIntAlone :: Parser Integer
posIntAlone = Parser f
  where
    f s = do
      (i, s1) <- runParser posInt s
      case s1 of
        (s0:_) | isAlpha s0 -> Nothing
        _                   -> return (i, s1)

parseAtom :: Parser Atom
parseAtom = fmap N posInt <|> fmap I ident

parseAtomIntAlone :: Parser Atom
parseAtomIntAlone = fmap N posIntAlone <|> fmap I ident

parseSExpr :: Parser SExpr
-- handle "3foo" as a special case
parseSExpr = spaces *> (fmap A parseAtom <|> parseSExprN) <* spaces

parseSExprN :: Parser SExpr
parseSExprN = spaces *>
              (fmap A parseAtomIntAlone
               <|> fmap Comb (char '(' *> oneOrMore parseSExprN <* char ')'))
              <* spaces
{- this version incorrectly parses "(3foo)" as (3 foo)
parseSExpr = spaces *>
             (fmap A parseAtom
              <|> fmap Comb (char '(' *> oneOrMore parseSExpr <* char ')'))
             <* spaces
-}
