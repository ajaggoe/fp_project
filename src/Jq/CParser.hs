module Jq.CParser where

import Parsing.Parsing
import Jq.JParser
import Jq.Filters

parseIdentity :: Parser Filter
parseIdentity = do
  _ <- token . char $ '.'
  return Identity

parseFilter :: Parser Filter
parseFilter = parseMultiFilter 
  <|> parseSingleFilter 

parseSingleFilter :: Parser Filter
parseSingleFilter = 
  parseParenthesis  
  -- <|> parseOptional
  <|> parseArrayIndexingOpt
  <|> parseObjectIndexingOpt 
  <|> parseArraySliceOpt 
  <|> parseArrayIteratorOpt 
  <|> parseObjectIteratorOpt
  <|> parseArrayIndexing 
  <|> parseObjectIndexing 
  <|> parseArraySlice
  <|> parseArrayIterator 
  <|> parseObjectIterator
  <|> parseIdentity

parseMultiFilter :: Parser Filter
parseMultiFilter = parseComma
  <|> parsePipe

parseParenthesis :: Parser Filter
parseParenthesis = do
  _ <- char '('
  f <- parseFilter
  _ <- char ')'
  return (Parenthesis f)

parseObjectIndexing :: Parser Filter
parseObjectIndexing = parseObjectIndexingBrack <|> parseObjectIndexingN

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

parseObjectIndexingOpt :: Parser Filter
parseObjectIndexingOpt = parseObjectIndexingBrackOpt <|> parseObjectIndexingNOpt

parseObjectIndexingBrackOpt :: Parser Filter
parseObjectIndexingBrackOpt = do
  _ <- string ".["
  ind <- parseString
  _ <- string "]"
  _ <- string "?"
  return $ IndexingOpt ind

parseObjectIndexingNOpt :: Parser Filter
parseObjectIndexingNOpt = do
  _ <- char '.' 
  ind <- ident <|> parseString
  _ <- string "?"
  return $ IndexingOpt ind

parseArrayIndexing :: Parser Filter
parseArrayIndexing = do
  _ <- string ".["
  x <- int
  _ <- string "]"
  return $ ArrayIndexing x


parseArrayIndexingOpt :: Parser Filter
parseArrayIndexingOpt = do
  _ <- string ".["
  x <- int
  _ <- string "]"
  _ <- string "?"
  return $ ArrayIndexingOpt x

parseArraySlice :: Parser Filter
parseArraySlice = do 
  _ <- string ".["
  first <- token int <|> pure 0
  _ <- token $ char ':'
  second <- token int
  _ <- token $ string "]"
  return $ ArraySlicer first second 

parseArraySliceOpt :: Parser Filter
parseArraySliceOpt = do 
  _ <- string ".["
  first <- token int <|> pure 0
  _ <- token $ char ':'
  second <- token int
  _ <- token $ string "]"
  return $ ArraySlicerOpt first second 

parseArrayIterator :: Parser Filter
parseArrayIterator = do
  _ <- string ".["
  x <- int
  xs <- many (do
    _ <- char ','
    int)
  _ <- string "]"
  return (ArrayIterator (x:xs))

parseArrayIteratorOpt :: Parser Filter
parseArrayIteratorOpt = do
  _ <- string ".["
  xs <- token $ int `sepBy` symbol ","
  _ <- string "]"
  _ <- string "?"
  return (ArrayIteratorOpt xs)

parseObjectIterator :: Parser Filter
parseObjectIterator = do
  _ <- string ".["
  xs <- token $ parseString `sepBy` char ','
  _ <- string "]"
  return (Iterator xs)

parseObjectIteratorOpt :: Parser Filter
parseObjectIteratorOpt = do
  _ <- string ".["
  xs <- token $ parseString `sepBy` char ','
  _ <- string "]"
  _ <- string "?"
  return (IteratorOpt xs)

-- parseOptional :: Parser Filter
-- parseOptional = undefined

parseComma :: Parser Filter
parseComma = do 
  x1 <- parseSingleFilter
  x2 <- do 
    _ <- symbol ","
    parseFilter
  return (Comma x1 x2)

parsePipe :: Parser Filter
parsePipe = do  
  x1 <- parseComma <|> parseSingleFilter
  _ <- symbol "|"
  x2 <- parseFilter
  return (Pipe x1 x2)

parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
