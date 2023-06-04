{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Jq.Compiler where

import           Jq.Filters
import           Jq.Json
import Data.Either (fromRight)

type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile Identity inp = return [inp]

compile (Parenthesis f) inp = compile f inp

compile (Indexing field) (JObject elements) = Right [findElem field elements]
compile (Indexing _) JNull = Right [JNull]
compile (Indexing _) inp = Left ("Input is not JObject: " ++ show inp)

compile (IndexingOpt field) (JObject elements) = Right [findElem field elements]
compile (IndexingOpt _) JNull = Right [JNull]
compile (IndexingOpt _) _ = Right []

compile (ArrayIndexing i) (JArray xs) = Right [findByIndex xs i]
compile (ArrayIndexing _) JNull = Right [JNull]
compile (ArrayIndexing _) _ = Left "Array Indexing on non array"

compile (ArrayIndexingOpt i) (JArray xs) = Right [findByIndex xs i]
compile (ArrayIndexingOpt _) JNull = Right [JNull]
compile (ArrayIndexingOpt _) _ = Right []

compile (ArraySlicer _ _) (JArray []) = Right [JArray []]
compile (ArraySlicer start end) (JArray xs) 
  | start < 0 = compile (ArraySlicer (length xs - min (-start) (length xs)) end) (JArray xs)
  | end < 0 = compile (ArraySlicer start (length xs - min (-end) (length xs))) (JArray xs)
  | start >= end = Right [JArray []]
  | otherwise = Right [JArray (take (start - end) (drop start xs))]
compile (ArraySlicer _ _) _ = Left "Array slice on non array"

compile (ArraySlicerOpt _ _) (JArray []) = Right [JArray []]
compile (ArraySlicerOpt start end) (JArray xs) 
  | start < 0 = compile (ArraySlicer (length xs - min (-start) (length xs)) end) (JArray xs)
  | end < 0 = compile (ArraySlicer start (length xs - min (-end) (length xs))) (JArray xs)
  | start >= end = Right [JArray []]
  | otherwise = Right [JArray (take (start - end) (drop start xs))]
compile (ArraySlicerOpt _ _) _ = Right []

compile (ArrayIterator _) JNull = Right [JNull]
compile (ArrayIterator ind) (JArray xs) = Right [JArray (map (findByIndex xs) ind)]
compile (ArrayIterator _) _ = Left "Array iterator used with non array object"

compile (ArrayIteratorOpt _) JNull = Right [JNull]
compile (ArrayIteratorOpt ind) (JArray xs) = Right [JArray (map (findByIndex xs) ind)]
compile (ArrayIteratorOpt _) _ = Right []

-- >>> findByIndex [2,3,4]

compile (Iterator keys) (JObject xs) = Right (getByKey keys xs) 
compile (Iterator keys) JNull = Right [JNull | _ <- [1..(length keys)]]
compile (Iterator _) _ = Left "Object value iterator used with non object."

compile (IteratorOpt keys) (JObject xs) = Right (getByKey keys xs)
compile (IteratorOpt keys) JNull = Right [JNull | _ <- [1..(length keys)]] 
compile (IteratorOpt _) _ = Right []

compile (Comma f1 f2) inp = case compile f1 inp of
    Left e1 -> Left e1
    Right v1 -> case compile f2 inp of  
        Left e2 -> Left e2
        Right v2 -> Right (v1++v2)

compile (Pipe f1 f2) inp = case compile f1 inp of
  Left e1 -> Left e1
  Right av -> f av
  where
    f [] = Right []
    f (x:xs) = do
        out <- compile f2 x
        (out++) <$> f xs

compile (CVNull) _ = Right [JNull]
compile (CVBool bool) _ = Right [JBool bool]
compile (CVString str) _ = Right [JString str]
compile (CVNum num) _ = Right [JNum num]

compile (CVArray []) _ = Right [JArray []]
compile (CVArray (x:xs)) inp = case compile (CVArray xs) inp of
  Right res -> concatMap (\y -> map (\(JArray ys) -> JArray (y:ys)) res) <$> compile x inp

compile (CVObject xs) inp = Right [JObject (map (\(k,v) -> (compileKey k, compileVal v)) xs)]
  where
    compileKey k = case compile k inp of
      Right [JString s] -> s
    compileVal v = case compile v inp of
      Right [x] -> x

-- compile (ArraySlicer first second) (JArray xs) 
--     | first >= second || xs == [] = Right [JArray []]
--     | otherwise = [JArray ]

compile RecursiveDescent (JArray []) = Right [JArray []]
compile RecursiveDescent (JArray (x:xs)) = Right ([JArray (x:xs)] ++ (fromRight [JNull] (compile RecursiveDescent x) ++ recursiveDescArrays xs))

compile RecursiveDescent (JObject ([])) = Right ([(JObject [])])
compile RecursiveDescent (JObject ((s,v):xs)) = Right (([(JObject ((s,v):xs))]) ++ (fromRight [JNull] (compile (Iterator []) (JObject ((s,v):xs)))))
compile RecursiveDescent inp = Right [inp]
-- ----------------------------------------------
run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j

findElem :: String -> [(String, JSON)] -> JSON
findElem _ [] = JNull
findElem ind (x:xs) = if show ind == show (fst x) then snd x else findElem ind xs

findByIndex :: [JSON] -> Int -> JSON
findByIndex xs ind
    | ind < 0 = findByIndex xs (ind + length xs)
    | otherwise = if ind < length xs then xs !! ind else JNull

getByKey :: [String] -> [(String, JSON)] -> [JSON]
getByKey xs obj = [findElem x obj | x <- xs]

recursiveDescArrays :: [JSON] -> [JSON]
recursiveDescArrays [] = []
recursiveDescArrays (x:xs) = fromRight [JNull] (compile RecursiveDescent x) ++ recursiveDescArrays xs