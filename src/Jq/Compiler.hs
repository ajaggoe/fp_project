{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Jq.Compiler where

import           Jq.Filters
import           Jq.Json
import Data.Either (fromRight)
import GHC.Exts (IsList(fromList))

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

compile (ArrayIndexing i) (JArray xs) = Right [findIndex xs i]
compile (ArrayIndexing _) JNull = Right [JNull]
compile (ArrayIndexing _) _ = Left "Array Indexing on non array"

compile (ArrayIndexingOpt i) (JArray xs) = Right [findIndex xs i]
compile (ArrayIndexingOpt _) JNull = Right [JNull]
compile (ArrayIndexingOpt _) _ = Right []

compile (ArraySlicer start end) (JArray arr) = Right [JArray (slice start end arr)]
compile (ArraySlicer start end) (JString str) = Right [JString (slice start end str)]
compile (ArraySlicer _ _) JNull = Right [JNull]
compile (ArraySlicer _ _) _ = Left "Array slicer on non array"

compile (ArraySlicerOpt start end) (JArray arr) = Right [JArray (slice start end arr)]
compile (ArraySlicerOpt start end) (JString str) = Right [JString (slice start end str)]
compile (ArraySlicerOpt _ _) JNull = Right [JNull]
compile (ArraySlicerOpt _ _) _ = return []

compile (ArrayIterator indices) (JArray arr) = Right (findIndexs indices arr)
compile (ArrayIterator indices) JNull = Right [JNull | _ <- [1..(length indices)]]
compile (ArrayIterator _) _ = Left "Array iterator operator used on non array."

compile (ArrayIteratorOpt indices) (JArray arr) = Right (findIndexs indices arr)
compile (ArrayIteratorOpt indices) JNull = Right [JNull | _ <- [1..(length indices)]]
compile (ArrayIteratorOpt _) _ = return []

-- >>> findByIndex [2,3,4]

compile (Iterator keys) (JObject dict) = Right (findKeys keys dict) 
compile (Iterator keys) JNull = Right [JNull | _ <- [1..(length keys)]]
compile (Iterator _) _ = Left "Iterator on non object"

compile (IteratorOpt keys) (JObject dict) = Right (findKeys keys dict)
compile (IteratorOpt keys) JNull = Right [JNull | _ <- [1..(length keys)]] 
compile (IteratorOpt _) _ = return []

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

compile CVNull _ = Right [JNull]
compile (CVBool bool) _ = Right [JBool bool]
compile (CVString str) _ = Right [JString str]
compile (CVNum num) _ = Right [JNum num]

compile (CVArray []) _ = Right [JArray []]
compile (CVArray (x:xs)) inp = case compile (CVArray xs) inp of
  Right res -> concatMap (\y -> map (\(JArray ys) -> JArray (y:ys)) res) <$> compile x inp

compile (CVObject xs) inp = Right [JObject (fromList (map (\(k,v) -> (compileKey k, compileVal v)) xs))]
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

slice :: Int -> Int -> [a] -> [a]
slice s e xs =
    if adFrom <= adTo then []
    else take (end - start) (drop start xs)
        where
            adFrom = if s >= 0 then s else length xs + s 
            adTo = if e >= 0 then e else length xs + e
            start = max 0 adFrom
            end = min (length xs) adTo

findIndexs :: [Int] -> [JSON] -> [JSON]
findIndexs indices xs = map (findIndex xs) indices

findIndex :: [JSON] -> Int -> JSON
findIndex xs i
    | i < 0 = findIndex xs (i + length xs)
    | otherwise = if i < length xs then xs !! i else JNull

findKeys :: [String] -> [(String, JSON)] -> [JSON]
findKeys keys xs = map (`findKey` xs) keys

findKey :: String -> [(String, JSON)] -> JSON
findKey _ [] = JNull 
findKey k (x:xs) = if fst x == k then snd x else findKey k xs

recursiveDescArrays :: [JSON] -> [JSON]
recursiveDescArrays [] = []
recursiveDescArrays (x:xs) = fromRight [JNull] (compile RecursiveDescent x) ++ recursiveDescArrays xs