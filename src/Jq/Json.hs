module Jq.Json where

data JSON =
    JNull

instance Show JSON where
  show (JNull) = "null"

instance Eq JSON where
  (==) = undefined

-- Smart constructors
-- Don't change the names or signatures, only the definitions

jsonNullSC :: JSON
jsonNullSC = JNull

jsonNumberSC :: Int -> JSON
jsonNumberSC = undefined
