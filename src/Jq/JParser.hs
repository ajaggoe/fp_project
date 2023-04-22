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

-- parseJNumDouble :: Parser JSON
-- parseJNumDouble = do parseJDouble <|> parseJNumE

-- parseJDouble :: Parser Double
-- parseJDouble = do
--     full <- int
--     _ <- char '.'
--     decimal <- many digit <|> pure "0"
--     return (read (show full ++ "." ++ decimal))

-- parseJNumE :: Parser Double
-- parseJNumE = do
--     i <- parseJDouble
--     _ <- char 'e' <|> char 'E'
--     sign <- symbol "-" <|> symbol "+"
--     expon <- int
--     return (read (show i ++ "e" ++ sign ++ show expon))

parseJSON :: Parser JSON
parseJSON = token $  parseJBool <|> parseJNull
  <|> parseJNum 
  -- <|> parseJNumDouble