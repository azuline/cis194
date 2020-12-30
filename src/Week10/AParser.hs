module Week10.AParser where

import Control.Applicative ((<|>), Alternative(..))
import Control.Monad       ((>=>))
import Data.Char           (isDigit, isUpper)

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
    f (x:xs) | p x = Just (x, xs)
    f _            = Nothing

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

instance Functor Parser where
  fmap f p = Parser $ fmap (first f) . runParser p

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Applicative Parser where
  pure a    = Parser $ \s -> Just (a, s)
  p1 <*> p2 = Parser $ runParser p1 >=> (\(g, s') -> first g <$> runParser p2 s')

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = const () <$ char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = f <$> posInt <*> char ' ' <*> posInt
  where
    f i1 _ i2 = [i1, i2]

instance Alternative Parser where
  empty     = Parser $ const Nothing
  p1 <|> p2 = Parser $ \s -> runParser p1 s <|> runParser p2 s

intOrUppercase :: Parser ()
intOrUppercase = () <$ posInt <|> () <$ satisfy isUpper
