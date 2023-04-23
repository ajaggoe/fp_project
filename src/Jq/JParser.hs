module Jq.JParser where

import Parsing.Parsing
import Jq.Json

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

parseString :: Parser String
parseString = do
  _ <- symbol "\""
  str <- many (sat (/= '\\'))
  _ <- symbol "\""
  return str

parseJString :: Parser JSON
parseJString = do 
  str <- parseString
  return (JString str)

parseJSON :: Parser JSON
parseJSON = token $  parseJNull
  <|> parseJBool
  <|> parseJNumDouble
  <|> parseJNum 
  <|> parseJString
