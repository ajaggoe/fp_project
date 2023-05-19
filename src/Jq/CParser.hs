module Jq.CParser where

import Parsing.Parsing
import Jq.JParser
import Jq.Filters

parseIdentity :: Parser Filter
parseIdentity = do
  _ <- token . char $ '.'
  return Identity

parseFilter :: Parser Filter
parseFilter = 
  parseIndexing
  <|> parseParenthesis
  <|> parseIdentity

parseParenthesis :: Parser Filter
parseParenthesis = do
  _ <- char '('
  f <- parseFilter
  _ <- char ')'
  return (Parenthesis [f])

parseIndexing :: Parser Filter
parseIndexing = parseIndexingArray <|> parseIndexingObject

parseIndexingObject :: Parser Filter
parseIndexingObject = parseIndexingN <|> parseIndexingArr

parseIndexingArr :: Parser Filter
parseIndexingArr = do
  _ <- string ".[\""
  space
  ind <- token (char ',') `sepBy` natural
  space
  _ <- string "\"]"
  return $ Indexing ind

parseIndexingN :: Parser Filter
parseIndexingN = do
  _ <- char '.' 
  Indexing <$> ident

parseIndexingArray :: Parser Filter
parseIndexingArray = do
  _ <- string ".["
  x <- int
  _ <- string "]"
  return $ ArrayIndexing x

parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
