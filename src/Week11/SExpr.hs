module Week11.SExpr where

import           Control.Applicative
import qualified Data.Char as C

import Week10.AParser

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

-- YUCK! Need to figure out how to use the patterns.
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy C.isSpace)

ident :: Parser String
ident = (:) <$> satisfy C.isAlpha <*> zeroOrMore (satisfy C.isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving (Show, Eq)

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving (Show, Eq)

parseSExpr :: Parser SExpr
parseSExpr = spaces *> (atom <|> comb) <* spaces
  where
    atom = A <$> parseAtom
    comb = Comb <$> (char '(' *> oneOrMore parseSExpr <* char ')')
  
parseAtom :: Parser Atom
parseAtom = (N <$> posInt) <|> (I <$> ident)
