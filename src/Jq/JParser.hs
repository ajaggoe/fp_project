module Jq.JParser where

import Parsing.Parsing
import Jq.Json
import Data.Char (readLitChar)

parseJNull :: Parser JSON
parseJNull = do _ <- string "null"
                return JNull

parseJBool :: Parser JSON
parseJBool = parseJTrue <|> parseJFalse

parseJTrue :: Parser JSON
parseJTrue = do _ <- string "true"
                return $ JBool True

parseJFalse :: Parser JSON
parseJFalse = do _ <- string "false"
                 return $ JBool False
  
parseJNum :: Parser JSON
parseJNum = do JNum <$> int

parseJNumDouble :: Parser JSON
parseJNumDouble = do parseJNumE <|> parseJDouble 

parseJDouble :: Parser JSON 
parseJDouble = do
    full <- int
    _ <- char '.'
    decimal <- some digit
    return (JFloat (read (show full ++ "." ++ decimal)))

parseJNumE :: Parser JSON
parseJNumE = do
    full <- int
    _ <- char '.'
    decimal <- many digit <|> pure "0"
    _ <- symbol "e" <|> symbol "E"
    sign <- symbol "-" <|> symbol "+" <|> symbol ""
    expon <- int
    return $ JFloat (read (show full ++ "." ++ decimal ++ "e" ++ sign ++ show expon))

parseJString :: Parser JSON
parseJString = JString <$> parseString

parseString :: Parser String
parseString = do
  _ <- char '\"'
  str <- many ((:[]) <$> sat (\c -> c /= '"' && c /= '\\') <|> ((: []) <$> parseUnicode) <|> parseEscape)
  _ <- char '\"'
  return (concat str)

-- >>> parse parseJSON "hello"
-- []

parseUnicode :: Parser Char
parseUnicode = do
  _ <- symbol "\\u"
  c1 <- sat (`elem` ['a', 'b', 'c', 'd', 'e', 'f', 'A', 'B', 'C', 'D', 'E', 'F']) <|> digit
  c2 <- sat (`elem` ['a', 'b', 'c', 'd', 'e', 'f', 'A', 'B', 'C', 'D', 'E', 'F']) <|> digit
  c3 <- sat (`elem` ['a', 'b', 'c', 'd', 'e', 'f', 'A', 'B', 'C', 'D', 'E', 'F']) <|> digit
  c4 <- sat (`elem` ['a', 'b', 'c', 'd', 'e', 'f', 'A', 'B', 'C', 'D', 'E', 'F']) <|> digit
  return (fst $ head $ readLitChar ("\\" ++ show (read ("0x" ++ [c1,c2,c3,c4]):: Int)))

parseEscape :: Parser String
parseEscape = do
    _ <- char '\\'
    e <- item
    case e of  
        'n' -> return "\\n"
        't' -> return "\\t"
        'r' -> return "\\r"
        'b' -> return "\\b"
        '\\' -> return "\\\\"
        '/' -> return "/"
        'f' -> return "\\f"
        '"' -> return "\\\""
        _ -> empty

parseJArray :: Parser JSON
parseJArray =  parseJArrayNotEmpt <|> parseJArrayEmpt 

parseJArrayEmpt :: Parser JSON
parseJArrayEmpt = do 
    _ <- string "[]"
    return (JArray [])

parseJArrayNotEmpt :: Parser JSON
parseJArrayNotEmpt = do
    _ <- string "["
    xs <- parseJSON `sepBy` token (char ',')
    _ <- string "]"
    return (JArray xs)

parseJObject :: Parser JSON
parseJObject = parseJObjectNotEmpt <|> parseJObjectEmpt

parseJObjectEmpt :: Parser JSON
parseJObjectEmpt = do
    _ <- symbol "{}"
    return (JObject [])

parseJObjectNotEmpt :: Parser JSON
parseJObjectNotEmpt = do  
  _ <- symbol "{"
  xs <- parseKeyVal `sepBy` char ','
  _ <- symbol "}"
  return (JObject xs)


sepBy :: Parser a -> Parser b -> Parser [a]
p `sepBy` sep = (p `sepByHelper` sep) <|> return []

sepByHelper :: Parser a -> Parser b -> Parser [a]
p `sepByHelper` sep = do
  x <- p
  xs <- many $ do
    _ <- sep
    p
  return $ x:xs

parseKeyVal :: Parser (String, JSON)
parseKeyVal = do
    k <- parseString
    _ <- token $ char ':'
    v <- parseJSON
    return (k, v)

parseJSON :: Parser JSON
parseJSON = token $  parseJNull
  <|> parseJArray
  <|> parseJBool
  <|> parseJObject
  <|> parseJNumDouble
  <|> parseJNum 
  <|> parseJString 

