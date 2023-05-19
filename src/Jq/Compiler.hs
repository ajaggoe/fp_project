module Jq.Compiler where

import           Jq.Filters
import           Jq.Json

type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile (Identity) inp = return [inp]

compile (Parenthesis f) inp = compile f inp

compile (Indexing field) (JObject elements) = Right [lookUp field elements]
compile (Indexing _) JNull = Right [JNull]
compile (Indexing _) inp = Left ("Input is not JObject: " ++ show inp)

compile (ArrayIndexing 0) (JArray (x:xs)) = Right [x]
compile (ArrayIndexing 0) (JArray []) = Left "Index out of bounds"
compile (ArrayIndexing i) (JArray (x:xs)) = compile (ArrayIndexing (i-1)) (JArray xs)
compile (ArrayIndexing _) (JNull) = Right [JNull]

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j

lookUp :: String -> [(String, JSON)] -> JSON
lookUp _ [] = JNull
lookUp ind (x:xs) = if show ind == show (fst x) then snd x else lookUp ind xs
