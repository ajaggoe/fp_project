module Jq.Json where

import Data.List (intercalate)

data JSON =
    JNull
   | JNum Int
   | JFloat Double
   | JString String
   | JBool Bool
   | JArray [JSON]
   | JObject [(String, JSON)]

instance Show JSON where
  show JNull = "null"
  show (JNum a) = show a
  show (JFloat a) = show a 
  show (JString a) = "\"" ++ a ++ "\""
  show (JBool True) = "true"
  show (JBool False) = "false"
  show (JArray xs) =  "["++ values xs ++"]"
    where values [] = ""
          values vs = "\n  "++ intercalate ", " (map show vs) ++"\n"
  show (JObject x) = "{" ++ pairs x ++ "}"
    where pairs [] = ""
          pairs xs = "\n  "++ intercalate ", " (map render xs) ++"\n"
          render (k,v) = show k ++ ": " ++ show v 

instance Eq JSON where
  JNull == JNull = True
  (JNum a) == (JNum b) = a == b
  (JFloat a) == (JFloat b) = a == b
  (JBool b) == (JBool c) = b == c
  (JString a) == (JString b) = a == b
  (JObject x) == (JObject y) = x == y
  (JArray xs) == (JArray ys) = xs == ys
  _ == _ = False

-- Smart constructors
-- These are included for test purposes and
-- aren't meant to correspond one to one with actual constructors you add to JSON datatype
-- For the tests to succeed fill them in with functions that return correct JSON values
-- Don't change the names or signatures, only the definitions

jsonNullSC :: JSON
jsonNullSC = JNull

jsonNumberSC :: Int -> JSON
jsonNumberSC = JNum

jsonStringSC :: String -> JSON
jsonStringSC = JString

jsonBoolSC :: Bool -> JSON
jsonBoolSC = JBool

jsonArraySC :: [JSON] -> JSON
jsonArraySC  = JArray 

jsonObjectSC :: [(String, JSON)] -> JSON
jsonObjectSC  = JObject 
