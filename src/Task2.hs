{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task2 where

import Parser
import ParserCombinators (char, choice, digit, many, nonZeroDigit, string)

-- | Date representation
--
-- Date parts are expected to be in following ranges
--
-- 'Day' in @[1..31]@
-- 'Month' in @[1..12]@
-- 'Year' is any non-negative integer
data Date = Date Day Month Year
  deriving (Show, Eq)

newtype Day = Day Int deriving (Show, Eq)

newtype Month = Month Int deriving (Show, Eq)

newtype Year = Year Int deriving (Show, Eq)

-- | Parses date in one of three formats given as BNF
--
-- @
-- date ::= dotFormat | hyphenFormat | usFormat
--
-- dotFormat ::= day "." month "." year
-- hyphenFormat ::= day "-" month "-" year
-- usFormat ::= monthName " " usDay " " year
--
-- usDay ::= nonZeroDigit | "1" digit | "2" digit | "30" | "31"
-- day ::= "0" nonZeroDigit | "1" digit | "2" digit | "30" | "31"
-- month ::= "0" nonZeroDigit | "10" | "11" | "12"
-- year ::= number
--
-- number ::= digit | number digit
-- digit ::= "0" | nonZeroDigit
-- nonZeroDigit ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
--
-- monthName ::= "Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" | "Jul" | "Aug" | "Sep" | "Oct" | "Nov" | "Dec"
-- @
--
-- Usage example:
--
-- >>> parse date "01.01.2012"
-- Parsed (Date (Day 1) (Month 1) (Year 2012)) (Input 10 "")
-- >>> parse date "12.12.2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 10 "")
-- >>> parse date "12-12-2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 10 "")
-- >>> parse date "Dec 12 2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 11 "")
-- >>> parse date "Jan 1 2012"
-- Parsed (Date (Day 1) (Month 1) (Year 2012)) (Input 10 "")
-- >>> parse date "Feb 31 2012"
-- Parsed (Date (Day 31) (Month 2) (Year 2012)) (Input 11 "")
-- >>> parse date "12/12/2012"
-- Failed [PosError 2 (Unexpected '/'),PosError 0 (Unexpected '1')]
date :: Parser Date
date = choice [dotFormat, hyphenFormat, usFormat]

-- >>> parse usFormat "Jun 18 0"
-- Failed [Position 0 (Unexpected '8')]
dotFormat :: Parser Date
dotFormat =
  Date
    <$> (day <* char '.')
    <*> (month <* char '.')
    <*> year

hyphenFormat :: Parser Date
hyphenFormat =
  Date
    <$> (day <* char '-')
    <*> (month <* char '-')
    <*> year

usFormat :: Parser Date
usFormat =
  (\m d y -> Date d m y)
    <$> (monthName <* char ' ')
    <*> (usDay <* char ' ')
    <*> year

day :: Parser Day
day =
  Day
    <$> read
    <$> choice
      [ (:) <$> char '0' <*> ((: []) <$> nonZeroDigit),
        (:) <$> char '1' <*> ((: []) <$> digit),
        (:) <$> char '2' <*> ((: []) <$> digit),
        string "30",
        string "31"
      ]

month :: Parser Month
month = Month <$> read <$> choice [((:) <$> char '0' <*> ((: []) <$> nonZeroDigit)), string "10", string "11", string "12"]

year :: Parser Year
year = Year <$> read <$> number

number :: Parser String
number = (:) <$> digit <*> many digit

monthName :: Parser Month
monthName =
  choice
    [ Month n <$ string name
    | (n, name) <- zip [1 .. 12] monthNames
    ]

-- >>> parse usDay "21"
-- Parsed (Day 21) (Position 2 "")
usDay :: Parser Day
usDay =
  Day
    <$> read
    <$> choice
      [ (:) <$> char '1' <*> ((: []) <$> digit),
        (:) <$> char '2' <*> ((: []) <$> digit),
        string "30",
        string "31",
        (: []) <$> nonZeroDigit
      ]

monthNames :: [String]
monthNames =
  [ "Jan",
    "Feb",
    "Mar",
    "Apr",
    "May",
    "Jun",
    "Jul",
    "Aug",
    "Sep",
    "Oct",
    "Nov",
    "Dec"
  ]
