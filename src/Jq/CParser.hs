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
  parseParenthesis
  <|> parseObjectIndexing
  <|> parseIdentity

parseParenthesis :: Parser Filter
parseParenthesis = do
  _ <- char '('
  f <- parseFilter
  _ <- char ')'
  return (Parenthesis f)

parseObjectIndexing :: Parser Filter
parseObjectIndexing =parseObjectIndexingBrack <|> parseObjectIndexingN

parseObjectIndexingBrack :: Parser Filter
parseObjectIndexingBrack = do
  _ <- string ".["
  ind <- parseString
  _ <- string "]"
  return $ Indexing ind

parseObjectIndexingN :: Parser Filter
parseObjectIndexingN = do
  _ <- char '.' 
  ind <- ident <|> parseString
  return $ Indexing ind

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
