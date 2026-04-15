{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task3 where

import Data.Char (chr, isHexDigit, toLower)
import Data.List (intercalate)
import Numeric (readHex)
import Parser
import ParserCombinators (char, choice, digit, many, nonZeroDigit, some, string)

-- | JSON representation
--
-- See <https://www.json.org>
data JValue
  = JObject [(String, JValue)]
  | JArray [JValue]
  | JString String
  | JNumber Double
  | JBool Bool
  | JNull
  deriving (Show, Eq)

-- \| Parses JSON value
--
-- See full grammar at <https://www.json.org>
--
-- Usage example:
--
-- >>> parse json "{}"
-- Parsed (JObject []) (Input 2 "")
-- >>> parse json "null"
-- Parsed JNull (Input 4 "")
-- >>> parse json "true"
-- Parsed (JBool True) (Input 4 "")
-- >>> parse json "3.14"
-- Parsed (JNumber 3.14) (Input 4 "")
-- >>> parse json "{{}}"
-- Failed [PosError 0 (Unexpected '{'),PosError 1 (Unexpected '{')]
json :: Parser JValue
json = ws *> value <* ws
  where
    value :: Parser JValue
    value =
      choice
        [ JObject <$> object,
          JArray <$> array,
          JString <$> jString,
          JNumber <$> jNumber,
          JBool True <$ token "true",
          JBool False <$ token "false",
          JNull <$ token "null"
        ]

    object :: Parser [(String, JValue)]
    object = between (symbol '{') (symbol '}') (sepBy pair (symbol ','))
      where
        pair :: Parser (String, JValue)
        pair =
          (\k v -> (k, v))
            <$> (lexeme jString <* symbol ':')
            <*> value

    array :: Parser [JValue]
    array = between (symbol '[') (symbol ']') (sepBy value (symbol ','))

    jString :: Parser String
    jString = concat <$> (char '"' *> many piece <* char '"')
      where
        piece :: Parser String
        piece = choice [rawChar, escaped]

        rawChar :: Parser String
        rawChar = (: []) <$> satisfy (\c -> c /= '"' && c /= '\\' && c >= ' ')

        escaped :: Parser String
        escaped =
          char '\\'
            *> choice
              [ "\\\"" <$ char '"',
                "\\\\" <$ char '\\',
                "\\/" <$ char '/',
                "\\b" <$ char 'b',
                "\\f" <$ char 'f',
                "\\n" <$ char 'n',
                "\\r" <$ char 'r',
                "\\t" <$ char 't',
                (\hs -> "\\u" ++ hs) <$> (char 'u' *> count 4 hexDigit)
              ]
        hexDigit :: Parser Char
        hexDigit = satisfy isHexDigit

    jNumber :: Parser Double
    jNumber = read <$> numberText
      where
        numberText :: Parser String
        numberText =
          (\s i f e -> s ++ i ++ f ++ e)
            <$> sign
            <*> intPart
            <*> fracPart
            <*> expPart

        sign :: Parser String
        sign = choice [string "-", pure ""]

        intPart :: Parser String
        intPart =
          choice
            [ string "0",
              ((:) <$> nonZeroDigit <*> many digit)
            ]

        fracPart :: Parser String
        fracPart =
          choice
            [ ((:) <$> char '.' <*> some digit),
              pure ""
            ]

        expPart :: Parser String
        expPart =
          choice
            [ ( (\e s ds -> [e] ++ s ++ ds)
                  <$> satisfy (\c -> c == 'e' || c == 'E')
                  <*> expSign
                  <*> some digit
              ),
              pure ""
            ]

        expSign :: Parser String
        expSign =
          choice
            [ ((: []) <$> satisfy (\c -> c == '+' || c == '-')),
              pure ""
            ]

-- * Helpers

ws :: Parser ()
ws = () <$ many (satisfy isJsonSpace)
  where
    isJsonSpace c = c == ' ' || c == '\n' || c == '\r' || c == '\t'

lexeme :: Parser a -> Parser a
lexeme p = ws *> p <* ws

token :: String -> Parser String
token = lexeme . string

symbol :: Char -> Parser Char
symbol = lexeme . char

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = open *> p <* close

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = choice [((:) <$> p <*> many (sep *> p)), pure []]

count :: Int -> Parser a -> Parser [a]
count n p
  | n <= 0 = pure []
  | otherwise = (:) <$> p <*> count (n - 1) p

hexToChar :: String -> Char
hexToChar s =
  case readHex s of
    [(n, "")] -> chr n
    _ -> error "invalid hex escape"

-- * Rendering helpers

-- | Renders given JSON value as oneline string
render :: JValue -> String
render = concatMap readable . renderTokens
  where
    -- Adds some nice spacing for readability
    readable ":" = ": "
    readable "," = ", "
    readable s = s

-- | Renders given JSON value as list of separate tokens ready for pretty printing
renderTokens :: JValue -> [String]
renderTokens JNull = ["null"]
renderTokens (JBool b) = [map toLower $ show b]
renderTokens (JNumber d) = [show d]
renderTokens (JString s) = ["\"" ++ s ++ "\""]
renderTokens (JArray xs) = ["["] ++ intercalate [","] (map renderTokens xs) ++ ["]"]
renderTokens (JObject xs) = ["{"] ++ intercalate [","] (map renderPair xs) ++ ["}"]
  where
    renderPair :: (String, JValue) -> [String]
    renderPair (k, v) = ["\"" ++ k ++ "\""] ++ [":"] ++ renderTokens v

-- | Renders 'Parsed' or 'Failed' value as string
renderParsed :: Parsed JValue -> String
renderParsed (Parsed v _) = render v
renderParsed (Failed err) = show err

-- | Parses given file as JSON and renders result
renderJSONFile :: String -> IO String
renderJSONFile file = renderParsed <$> parseJSONFile file

-- | Parses given file as JSON
parseJSONFile :: String -> IO (Parsed JValue)
parseJSONFile file = parse json <$> readFile file
