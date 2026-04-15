{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module ParserCombinators where

import Control.Applicative hiding (many, some)
import Control.Monad (void)
import Data.Char (isDigit, isSpace)
import Parser

-- | Parses single character
--
-- Usage example:
--
-- >>> parse (char 'b') "bar"
-- Parsed 'b' (Position 1 "ar")
-- >>> parse (char 'b') "abc"
-- Failed [Position 0 (Unexpected 'a')]
char :: Char -> Parser Char
char c = satisfy (== c)

digit :: Parser Char
digit = satisfy isDigit

-- >>> parse nonZeroDigit "1"
-- Parsed '1' (Position 1 "")
-- >>> parse nonZeroDigit "12"
-- Parsed '1' (Position 1 "2")
-- >>> parse nonZeroDigit "012"
-- Failed [Position 0 (Unexpected '0')]
nonZeroDigit :: Parser Char
nonZeroDigit = satisfy (\c -> isDigit c && c /= '0')

space :: Parser ()
space = void $ satisfy isSpace

-- | Parses given string
--
-- Usage example:
--
-- >>> parse (string "ba") "bar"
-- Parsed "ba" (Position 2 "r")
-- >>> parse (string "ba") "abc"
-- Failed [Position 0 (Unexpected 'a')]
string :: String -> Parser String
string = traverse char

-- | Skips zero or more space characters
--
-- Usage example:
--
-- >>> parse spaces "  bar"
-- Parsed () (Position 2 "bar")
-- >>> parse spaces "bar"
-- Parsed () (Position 0 "bar")
-- >>> parse (spaces *> string "bar") "bar"
-- Parsed "bar" (Position 3 "")
spaces :: Parser ()
spaces = many (satisfy isSpace) *> pure ()

-- | Tries to consecutively apply each of given list of parsers until one succeeds.
-- Returns the *first* succeeding parser as result or 'empty' if all of them failed.
--
-- Usage example:
--
-- >>> parse (choice [char 'a', char 'b']) "bar"
-- Parsed 'b' (Position 1 "ar")
-- >>> parse (choice [char 'a', char 'b']) "foo"
-- Failed [Position 0 (Unexpected 'f')]
-- >>> parse (choice [string "ba", string "bar"]) "bar"
-- Parsed "ba" (Position 2 "r")
choice :: (Foldable t, Alternative f) => t (f a) -> f a
choice = foldr (<|>) empty

many :: Parser a -> Parser [a]
many p = some p <|> pure []

some :: Parser a -> Parser [a]
some p = (:) <$> p <*> many p

-- Discover and implement more useful parser combinators below
--
-- - <https://hackage.haskell.org/package/parser-combinators-1.3.0/docs/Control-Applicative-Combinators.html>
-- - <https://hackage.haskell.org/package/parsec-3.1.18.0/docs/Text-Parsec-Char.html>
