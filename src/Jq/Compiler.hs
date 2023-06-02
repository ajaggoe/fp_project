module Jq.Compiler where

import           Jq.Filters
import           Jq.Json
import Data.Ix (Ix(index))

type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile (Identity) inp = return [inp]

compile (Parenthesis f) inp = compile f inp

compile (Indexing field) (JObject elements) = Right [findElem field elements]
compile (Indexing _) JNull = Right [JNull]
compile (Indexing _) inp = Left ("Input is not JObject: " ++ show inp)

compile (ArrayIndexing 0) (JArray (x:xs)) = Right [x]
compile (ArrayIndexing 0) (JArray []) = Left "Index out of bounds"
compile (ArrayIndexing i) (JArray (x:xs)) = compile (ArrayIndexing (i-1)) (JArray xs)
compile (ArrayIndexing _) (JNull) = Right [JNull]

compile (ArraySlicer _ _) (JArray []) = Right [JArray []]
compile (ArraySlicer start end) (JArray xs) 
  | start < 0 = compile (ArraySlicer (length xs - min (-start) (length xs)) end) (JArray xs)
  | end < 0 = compile (ArraySlicer start (length xs - min (-end) (length xs))) (JArray xs)
  | start >= end = Right [JArray []]
  | otherwise = Right [JArray (take (start - end) (drop start xs))]

compile (ArrayIterator _) (JArray []) = Right [JArray []]
compile (ArrayIterator []) (JArray _) = Right [JArray []]
compile (ArrayIterator ind) (JArray xs) = Right [JArray (map (findByIndex xs) ind)]

-- compile (ArraySlicer first second) (JArray xs) 
--     | first >= second || xs == [] = Right [JArray []]
--     | otherwise = [JArray ]

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j

findElem :: String -> [(String, JSON)] -> JSON
findElem _ [] = JNull
findElem ind (x:xs) = if show ind == show (fst x) then snd x else findElem ind xs

findByIndex :: [JSON] -> Int -> JSON
findByIndex xs ind
    | ind < 0 = findByIndex xs (ind + length xs)
    | otherwise = if ind >=0 && ind < length xs then xs!!ind else JNull
