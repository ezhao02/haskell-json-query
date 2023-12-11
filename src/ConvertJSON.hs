module ConvertJSON where

import Data.Map (Map)
import JSONObject

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