module Jq.Compiler where

import           Jq.Filters
import           Jq.Json

type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile (Identity) inp = return [inp]

compile (Indexing field) (JObject elements) = Right [lookUp field elements]
compile (Indexing _) JNull = Right [JNull]
compile (Indexing _) inp = Left ("Input is not of type JObject: " ++ show inp)

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j

lookUp :: String -> [(String, JSON)] -> JSON
lookUp _ [] = JNull
lookUp field (x:xs) = if show field == show (fst x) then snd x else lookUp field xs
