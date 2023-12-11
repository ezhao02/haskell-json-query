module JSONObject where

import Data.Map (Map)
import Data.Map qualified as M

type JSONObj = Map String JSON

data JSON
  = JSONNum Float
  | JSONStr String
  | JSONObj JSONObj
  | JSONBool Bool
  | JSONNull
  | JSONList [JSON]
  deriving (Eq, Show)

-- instance Show JSON where
--   show :: JSON -> String
--   show j = undefined

class ConvertJSON a where
  toJSON :: a -> JSON
  fromJSON :: JSON -> Either String a

instance ConvertJSON Float where
  toJSON :: Float -> JSON
  toJSON = JSONNum

  fromJSON :: JSON -> Either String Float
  fromJSON (JSONNum x) = Right x
  fromJSON _ = Left "Not a number"

instance ConvertJSON String where
  toJSON :: String -> JSON
  toJSON = JSONStr

  fromJSON :: JSON -> Either String String
  fromJSON (JSONStr x) = Right x
  fromJSON _ = Left "Not a string"

instance ConvertJSON Bool where
  toJSON :: Bool -> JSON
  toJSON = JSONBool

  fromJSON :: JSON -> Either String Bool
  fromJSON (JSONBool x) = Right x
  fromJSON _ = Left "Not a boolean"

instance (ConvertJSON a) => ConvertJSON [a] where
  toJSON :: [a] -> JSON
  toJSON = JSONList . map toJSON

  fromJSON :: JSON -> Either String [a]
  fromJSON (JSONList xs) = traverse fromJSON xs
  fromJSON _ = Left "Not a list"

instance (ConvertJSON a) => ConvertJSON (Map String a) where
  toJSON :: Map String a -> JSON
  toJSON = JSONObj . fmap toJSON

  fromJSON :: JSON -> Either String (Map String a)
  fromJSON (JSONObj xs) = traverse fromJSON xs
  fromJSON _ = Left "Not an object"

-- Example usage:
exampleMap :: Map String Float
exampleMap = M.fromList [("key1", 1.0), ("key2", 2.0), ("key3", 3.0)]

-- main :: IO ()
-- main = do
--   let jsonRepresentation = toJSON exampleMap
--   putStrLn "JSON Representation:"
--   print jsonRepresentation

--   let convertedMap :: Either String (Map String Float)
--       convertedMap = fromJSON jsonRepresentation

--   putStrLn "\nConverted Map:"
--   case convertedMap of
--     Right m -> print m
--     Left err -> putStrLn $ "Error: " ++ err
