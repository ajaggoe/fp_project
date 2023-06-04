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
  parseValueConstructors
  <|> parseSingleFilter 
  <|>parseMultiFilter 

parseSingleFilter :: Parser Filter
parseSingleFilter =  parseParenthesis  
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
  <|> parseRecursive
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
  -- <|> do
  -- _ <- string ".\""
  -- ind <- ident <|> parseString
  -- _ <- string "\""
  -- return $ Indexing ind

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
  -- <|> do
  -- _ <- string ".\""
  -- ind <- ident <|> parseString
  -- _ <- string "\""
  -- return $ Indexing ind

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
  first <- token integer <|> pure 0
  _ <- token $ char ':'
  second <- token integer
  _ <- token $ string "]"
  return $ ArraySlicer first second 

parseArraySliceOpt :: Parser Filter
parseArraySliceOpt = do 
  _ <- string ".["
  first <- token integer <|> pure 0
  _ <- token $ char ':'
  second <- token integer
  _ <- token $ string "]"
  return $ ArraySlicerOpt first second 

parseArrayIterator :: Parser Filter
parseArrayIterator = do
  _ <- string ".["
  x <- token integer
  xs <- many (do
    _ <- char ','
    integer)
  _ <- string "]"
  return (ArrayIterator (x:xs))

parseArrayIteratorOpt :: Parser Filter
parseArrayIteratorOpt = do
  _ <- string ".["
  x <- int
  xs <- many (do
    _ <- char ','
    int)
  _ <- string "]"
  _ <- string "?"
  return (ArrayIteratorOpt (x:xs))

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

parseValueConstructors :: Parser Filter
parseValueConstructors = 
  parseValueNull
  <|> parseValueBool
  <|> parseValueNum
  <|> parseValueString
  <|> parseValueArray
  <|> parseValueObject     

parseValueNull :: Parser Filter
parseValueNull = do
  _ <- string "null"
  return CVNull

parseValueBool :: Parser Filter
parseValueBool = do
  _ <- string "true"
  return (CVBool True)
  <|>
  do
  _ <- string "false"
  return (CVBool False)

parseValueNum :: Parser Filter
parseValueNum = CVNum <$> int

parseValueString :: Parser Filter
parseValueString = do
  CVString <$> parseString

parseValueArray :: Parser Filter
parseValueArray = do
  _ <- string "["
  xs <- parseFilter `sepBy` token (char ',')
  _ <- string "]"
  return (CVArray xs)

parseValueObject :: Parser Filter
parseValueObject = do
  _ <- char '{' 
  xs <- space *>  (space *> char ',' <* space) `seperBY` kvPairs <* space
  _ <- char '}'
  return (CVObject xs)
  where
     seperBY sep element = (:) <$> element <*> many (sep *> element)
       <|> pure []
     kvPairs :: Parser (Filter, Filter)
     kvPairs = do
       k <- space *> parseFilter
       _ <- token (char ':' )
       v <- parseFilter
       return (k,v)


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

parseRecursive :: Parser Filter
parseRecursive = do
  _ <- string ".."
  return RecursiveDescent

parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
