module MicroCabal.YAML(
  YAMLValue(..),
  YAMLFieldName,
  showYAML,
  parseYaml,
  ) where
import Control.Applicative (Alternative(..))
import Data.Char (isSpace, isAlphaNum)
import Data.List (isPrefixOf)

type YAMLFieldName = String

data YAMLValue
  = YString String
  | YBool   Bool
  | YInt    Int
  | YRecord [(YAMLFieldName, YAMLValue)]
  | YArray  [YAMLValue]
  | YNull
  deriving (Show)

showYAML :: YAMLValue -> String
showYAML = show

----------
-- Code mostly by Claude Sonnet 2.5
-- Not exactly a work of beauty, but it seems to work.

newtype Parser a = Parser { runParser :: String -> Either String (a, String) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (x, rest) <- p input
    return (f x, rest)

instance Applicative Parser where
  pure x = Parser $ \input -> Right (x, input)
  (Parser pf) <*> (Parser px) = Parser $ \input -> do
    (f, rest1) <- pf input
    (x, rest2) <- px rest1
    return (f x, rest2)

instance Monad Parser where
  (Parser p) >>= f = Parser $ \input -> do
    (x, rest) <- p input
    runParser (f x) rest

instance Alternative Parser where
  empty = Parser $ \_ -> Left "empty"
  (Parser p1) <|> (Parser p2) = Parser $ \input ->
    case p1 input of
      Right x -> Right x
      Left _ -> p2 input

-- | Count leading spaces
countSpaces :: String -> Int
countSpaces = length . takeWhile (== ' ')

-- | Skip to next non-empty, non-comment line and return its indentation
peekIndent :: String -> Maybe Int
peekIndent input = go input
  where
    go "" = Nothing
    go ('\n':rest) = go rest
    go s@(' ':_) =
      let spaces = countSpaces s
          afterSpaces = drop spaces s
      in case afterSpaces of
           ('#':_) -> go (dropWhile (/= '\n') afterSpaces)
           ('\n':_) -> go afterSpaces
           "" -> Nothing
           _ -> Just spaces
    go ('#':rest) = go (dropWhile (/= '\n') rest)
    go _ = Just 0

-- | Skip spaces (not newlines)
skipSpaces :: Parser ()
skipSpaces = Parser $ \input -> Right ((), dropWhile (\c -> c == ' ' || c == '\t') input)

-- | Skip empty lines and comments
skipEmpty :: Parser ()
skipEmpty = Parser go
  where
    go "" = Right ((), "")
    go ('\n':rest) = go rest
    go input@(' ':_) =
      let afterSpaces = dropWhile (== ' ') input
      in case afterSpaces of
           ('\n':rest) -> go rest
           ('#':_) -> go (dropWhile (/= '\n') afterSpaces)
           _ -> Right ((), input)
    go input@('#':_) = go (dropWhile (/= '\n') input)
    go input = Right ((), input)

-- | Consume exact spaces
exactSpaces :: Int -> Parser ()
exactSpaces n = Parser $ \input ->
  let prefix = take n input
  in if length prefix == n && all (== ' ') prefix
     then Right ((), drop n input)
     else Left $ "Expected " ++ show n ++ " spaces at indent, got: " ++ show (countSpaces input)

-- | Parse a specific character
char :: Char -> Parser Char
char c = Parser $ \input -> case input of
  (x:xs) | x == c -> Right (c, xs)
  _ -> Left $ "Expected '" ++ [c] ++ "'"

-- | Parse until predicate is true
takeWhileP :: (Char -> Bool) -> Parser String
takeWhileP prd = Parser $ \input ->
  let (taken, rest) = span prd input
  in Right (taken, rest)

-- | Parse a YAML key (alphanumeric, dash, underscore, but not starting with dash alone)
parseKey :: Parser String
parseKey = do
  first <- Parser $ \input -> case input of
    (c:cs) | isAlphaNum c || c == '_' -> Right (c, cs)
    _ -> Left "Expected key character"
  rest <- takeWhileP (\c -> isAlphaNum c || c == '_' || c == '-')
  return (first:rest)

-- | Parse the rest of the line (value on same line)
parseLineValue :: Parser String
parseLineValue = Parser $ \input ->
  let trimmed = dropWhile (== ' ') input
      (value, rest) = break (\c -> c == '\n' || c == '#') trimmed
      cleaned = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace value
  in if null cleaned
     then Left "Empty line value"
     else Right (cleaned, rest)

-- | Parse boolean
parseBool :: String -> Maybe Bool
parseBool "true" = Just True
parseBool "false" = Just False
parseBool _ = Nothing

-- | Try to parse as integer
parseInt :: String -> Maybe Int
parseInt t = case reads t of
  [(n, "")] -> Just n
  _ -> Nothing

-- | Convert text to YAMLValue
textToValue :: String -> YAMLValue
textToValue t
  | Just b <- parseBool t = YBool b
  | Just n <- parseInt t = YInt n
  | t == "null" = YNull
  | otherwise = YString t

-- | Parse value on same line as key
parseInlineValue :: Parser YAMLValue
parseInlineValue = do
  val <- parseLineValue
  return $ textToValue val

-- | Parse key-value pair at given indentation
parseKV :: Int -> Parser (String, YAMLValue)
parseKV indent = Parser $ \input -> do
  -- Skip empty lines first
  ((), afterEmpty) <- runParser skipEmpty input
  -- Check indentation
  case runParser (exactSpaces indent) afterEmpty of
    Left err -> Left $ "parseKV indent error: " ++ err ++ " at: " ++ take 50 afterEmpty
    Right ((), afterIndent) -> do
      -- Parse key
      case runParser parseKey afterIndent of
        Left err -> Left $ "parseKV key error: " ++ err ++ " at: " ++ take 50 afterIndent
        Right (key, afterKey) -> do
          -- Skip spaces
          ((), afterSpaces) <- runParser skipSpaces afterKey
          -- Expect colon
          case runParser (char ':') afterSpaces of
            Left err -> Left $ "parseKV colon error at key '" ++ key ++ "': " ++ err ++ " at: " ++ take 50 afterSpaces
            Right (_, afterColon) -> do
              -- Skip spaces after colon
              ((), afterColonSpaces) <- runParser skipSpaces afterColon
              -- Check if value is on same line or next line
              case afterColonSpaces of
                ('\n':rest) -> do
                  -- Multi-line value (object or array)
                  case peekIndent rest of
                    Just nextIndent | nextIndent >= indent -> do
                      -- Check if it's an array or object
                      if isArrayAtIndent nextIndent rest
                        then do
                          (val, rest2) <- runParser (parseArray nextIndent) rest
                          Right ((key, val), rest2)
                        else if nextIndent > indent
                          then do
                            (val, rest2) <- runParser (parseObject nextIndent) rest
                            Right ((key, val), rest2)
                          else Right ((key, YRecord []), rest)
                    _ -> Right ((key, YRecord []), rest)
                _ -> do
                  -- Inline value
                  (val, rest) <- runParser parseInlineValue afterColonSpaces
                  Right ((key, val), rest)

-- | Parse YAML object - collect all KVs at this indent level
parseObject :: Int -> Parser YAMLValue
parseObject indent = Parser $ \input ->
  let loop acc inp = case peekIndent inp of
        Just i | i == indent ->
          -- Check if it's a key-value pair (not an array item)
          case runParser skipEmpty inp of
            Right ((), afterEmpty) ->
              let afterSpaces = drop i afterEmpty
              in if "-" `isPrefixOf` afterSpaces
                 then Right (reverse acc, inp)  -- Stop if we hit an array item
                 else case runParser (parseKV indent) inp of
                        Right ((k, v), rest) -> loop ((k, v):acc) rest
                        Left err -> Left err
            Left err -> Left err
        _ -> Right (reverse acc, inp)
  in case loop [] input of
       Right (pairs, rest) -> Right (YRecord pairs, rest)
       Left err -> Left err

-- | Parse array item
parseArrayItem :: Int -> Parser YAMLValue
parseArrayItem indent = Parser $ \input -> do
  -- Skip empty and get to the dash
  ((), afterEmpty) <- runParser skipEmpty input
  ((), afterIndent) <- runParser (exactSpaces indent) afterEmpty
  (_, afterDash) <- runParser (char '-') afterIndent
  ((), afterSpaces) <- runParser skipSpaces afterDash

  case afterSpaces of
    ('\n':rest) -> do
      -- Multi-line item (object or array on next line)
      case peekIndent rest of
        Just nextIndent | nextIndent > indent ->
          runParser (parseValue nextIndent) rest
        _ -> Right (YRecord [], rest)
    _ -> do
      -- Content on same line as dash
      -- Try to parse as "key: value" to start an object
      case runParser parseKey afterSpaces of
        Right (key, afterKey) -> do
          ((), afterKeySpaces) <- runParser skipSpaces afterKey
          case runParser (char ':') afterKeySpaces of
            Right (_, afterColon) -> do
              -- This is "- key: ..." format, parse as object
              -- Parse this first key-value pair
              ((), afterColonSpaces) <- runParser skipSpaces afterColon
              case afterColonSpaces of
                ('\n':rest) -> do
                  -- Value on next lines
                  case peekIndent rest of
                    Just nextIndent | nextIndent > indent -> do
                      -- Parse the value for this key
                      (val, rest2) <- runParser (parseValue nextIndent) rest
                      -- Check for more sibling keys at indent+2
                      let continueParsingKeys acc r = case peekIndent r of
                            Just i | i == indent + 2 && not (isArrayAtIndent i r) ->
                              case runParser (parseKV (indent + 2)) r of
                                Right ((k, v), r2) -> continueParsingKeys ((k,v):acc) r2
                                Left _ -> Right (reverse acc, r)
                            _ -> Right (reverse acc, r)
                      (morePairs, rest3) <- continueParsingKeys [(key, val)] rest2
                      Right (YRecord morePairs, rest3)
                    _ -> Right (YRecord [(key, YRecord [])], rest)
                _ -> do
                  -- Inline value
                  (val, rest) <- runParser parseInlineValue afterColonSpaces
                  -- Check for more keys at indent+2
                  let continueParsingKeys acc r = case peekIndent r of
                        Just i | i == indent + 2 && not (isArrayAtIndent i r) ->
                          case runParser (parseKV (indent + 2)) r of
                            Right ((k, v), r2) -> continueParsingKeys ((k,v):acc) r2
                            Left _ -> Right (reverse acc, r)
                        _ -> Right (reverse acc, r)
                  (morePairs, rest2) <- continueParsingKeys [(key, val)] rest
                  Right (YRecord morePairs, rest2)
            Left _ -> do
              -- Not "key:", treat whole line as string
              runParser parseInlineValue afterSpaces
        Left _ -> do
          -- Can't parse as key, treat as inline string value
          runParser parseInlineValue afterSpaces

-- | Parse YAML array - collect all items at this indent level
parseArray :: Int -> Parser YAMLValue
parseArray indent = Parser $ \input ->
  let loop acc inp = case peekIndent inp of
        Just i | i == indent && isArrayAtIndent i inp ->
          case runParser (parseArrayItem indent) inp of
            Right (item, rest) -> loop (item:acc) rest
            Left err -> Left err
        _ -> Right (reverse acc, inp)
  in case loop [] input of
       Right (items, rest) -> Right (YArray items, rest)
       Left err -> Left err

-- | Check if there's an array item at the given indent
isArrayAtIndent :: Int -> String -> Bool
isArrayAtIndent indent input =
  let cleaned = dropWhile (\c -> c == '\n' || c == '#') (dropComment input)
      spaces = countSpaces cleaned
      rest = drop spaces cleaned
  in spaces == indent && "-" `isPrefixOf` rest
  where
    dropComment s = case s of
      ('#':rest) -> dropWhile (/= '\n') rest
      _ -> s

-- | Parse any YAML value
parseValue :: Int -> Parser YAMLValue
parseValue indent = Parser $ \input ->
  case runParser skipEmpty input of
    Right ((), rest) ->
      if isArrayAtIndent indent rest
        then runParser (parseArray indent) rest
        else runParser (parseObject indent) rest
    Left err -> Left err

-- | Parse entire YAML document
parseYaml :: String -> Either String YAMLValue
parseYaml input = do
  (val, _) <- runParser (parseObject 0) input
  return val
