module Jq.Json where

data JSON =
    JNull

instance Show JSON where
  show (JNull) = "null"

instance Eq JSON where
  JNull == JNull = True
  _ == _ = undefined

-- Smart constructors
-- These are included for test purposes and
-- aren't meant to correspond one to one with actual constructors you add to JSON datatype
-- For the tests to succeed fill them in with functions that return correct JSON values
-- Don't change the names or signatures, only the definitions

jsonNullSC :: JSON
jsonNullSC = JNull

jsonNumberSC :: Int -> JSON
jsonNumberSC = undefined

jsonStringSC :: String -> JSON
jsonStringSC = undefined

jsonBoolSC :: Bool -> JSON
jsonBoolSC = undefined

jsonArraySC :: [JSON] -> JSON
jsonArraySC = undefined

jsonObjectSC :: [(String, JSON)] -> JSON
jsonObjectSC = undefined
