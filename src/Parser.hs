{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- The above pragma temporarily disables warnings about Parser constructor and runParser not being used

module Parser
  ( -- * Important note

    --

    -- | The implementation of 'Parser' is intentionally
    -- hidden to other modules to encourage use of high level
    -- combinators like 'satisfy' and the ones from 'ParserCombinators'
    Parser,
    parse,
    parseMaybe,
    satisfy,
    Error (..),
    Position (..),
    Parsed (..),
    Input,
  )
where

import Control.Applicative
import Data.List (nub)

-- | Value annotated with position of parsed input starting from 0
data Position a = Position Int a
  deriving (Show, Eq)

-- | Parser input encapsulating remaining string to be parsed with current position
type Input = Position String

-- | Parsing error
data Error
  = -- | Unexpected character
    Unexpected Char
  | -- | Unexpected end of input
    EndOfInput
  deriving (Show, Eq)

-- | Parsing result of value of type @a@
data Parsed a
  = -- | Successfully parsed value of type @a@ with remaining input to be parsed
    Parsed a Input
  | -- | Failed to parse value of type @a@ with accumulated list of errors
    Failed [Position Error]
  deriving (Show)

-- | Parser of value of type @a@
newtype Parser a = Parser {runParser :: Input -> Parsed a}

-- | Runs given 'Parser' on given input string
parse :: Parser a -> String -> Parsed a
parse Parser {runParser} = runParser . Position 0

-- | Runs given 'Parser' on given input string with erasure of @Parsed a@ to @Maybe a@
parseMaybe :: Parser a -> String -> Maybe a
parseMaybe p str = case parse p str of
  Parsed a _ -> Just a
  Failed _ -> Nothing

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p =
    Parser
      { runParser =
          ( \(Position _ str) -> case parse p str of
              Parsed a rest -> Parsed (f a) rest
              Failed errs -> Failed errs
          )
      }

instance Applicative Parser where
  pure a = Parser {runParser = \inp -> Parsed a inp}
  (<*>) :: forall a b. Parser (a -> b) -> Parser a -> Parser b
  (<*>) p1 p2 = Parser {runParser = runParser'}
    where
      runParser' :: Input -> Parsed b
      runParser' (Position _ str) = case parse p1 str of
        Failed errs -> Failed errs
        Parsed f (Position pos rest) -> case parse p2 rest of
          Failed errs' -> Failed errs'
          Parsed x (Position pos' rest') -> Parsed (f x) (Position (pos + pos') rest')

instance Alternative Parser where
  empty = Parser {runParser = \_ -> Failed []}

  -- Note: when both parsers fail, their errors are accumulated and *deduplicated* to simplify debugging
  (<|>) p1 p2 = Parser $ \input ->
    case runParser p1 input of
      Failed errs -> case runParser p2 input of
        Failed errs' -> Failed $ nub $ errs <> errs'
        out@(Parsed _ _) -> out
      out@(Parsed _ _) -> out

-- | Parses single character satisfying given predicate
--
-- Usage example:
--
-- >>> parse (satisfy (>= 'b')) "foo"
-- Parsed 'f' (Position 1 "oo")
-- >>> parse (satisfy (>= 'b')) "bar"
-- Parsed 'b' (Position 1 "ar")
-- >>> parse (satisfy (>= 'b')) "abc"
-- Failed [Position 0 (Unexpected 'a')]
-- >>> parse (satisfy (>= 'b')) ""
-- Failed [Position 0 EndOfInput]
-- >>> parse (satisfy (>= 'b')) ""
-- Failed [Position 0 EndOfInput]
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser {runParser = runParser'}
  where
    runParser' :: Input -> Parsed Char
    runParser' (Position pos []) = Failed [Position pos EndOfInput]
    runParser' (Position pos (x : xs))
      | p x = Parsed x (Position (pos + 1) xs)
      | otherwise = Failed [Position pos (Unexpected x)]
