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
parseJNumDouble = do parseJDouble <|> parseJNumE

parseJDouble :: Parser JSON 
parseJDouble = do
    l <- int 
    _ <- symbol "."
    r <- some digit
    return (JFloat (read (show l ++ "." ++ r)))

-- >>> parseJSON "true"
-- Couldn't match expected type: String -> t_apUZ[sk:1]
--             with actual type: Parser JSON
-- The function `parseJSON' is applied to one value argument,
--   but its type `Parser JSON' has none
-- In the expression: parseJSON "true"
-- In an equation for `it_apTX': it_apTX = parseJSON "true"
-- Relevant bindings include
--   it_apTX :: t_apUZ[sk:1]
--     (bound at D:\CSE Year 4\Q3 - Functional Programming\fpajaggoe\src\Jq\JParser.hs:33:2)

parseJNumE :: Parser JSON
parseJNumE = do
    full <- int
    _ <- char '.'
    decimal <- many digit <|> pure "0"
    _ <- char 'e' <|> char 'E'
    sign <- symbol "-" <|> symbol "+"
    expon <- int
    return $ JFloat (read (show full ++ "." ++ decimal ++ "e" ++ sign ++ show expon))

parseJSON :: Parser JSON
parseJSON = token $  parseJBool <|> parseJNull
  <|> parseJNum 
  <|> parseJNumDouble
