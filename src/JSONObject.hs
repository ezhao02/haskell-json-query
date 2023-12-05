module JSONObject where

import Data.Map (Map)

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