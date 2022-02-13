module Jq.Json where

data JSON =
    JNull deriving (Eq)

instance Show JSON where
  show (JNull) = "null"

-- Smart constructors
-- Don't change the names or signatures, only the definitions

jsonNull :: JSON
jsonNull = JNull

jsonNumber :: Int -> JSON
jsonNumber = undefined

jsonString :: String -> JSON
jsonString = undefined

jsonBool :: Bool -> JSON
jsonBool = undefined

jsonArray :: [JSON] -> JSON
jsonArray = undefined

jsonObject :: [(String, JSON)] -> JSON
jsonObject = undefined
