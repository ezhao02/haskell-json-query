module JSONObject where

import Data.List (intercalate)
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
  deriving (Eq)

instance Show JSON where
  show :: JSON -> String
  show = removeTrailingComma . showJSON 0

showJSON :: Int -> JSON -> String
showJSON x (JSONNum n) = show n
showJSON x (JSONStr s) = '\"' : s ++ "\""
showJSON x (JSONObj obj) = showJSONObject x obj
showJSON x (JSONBool b) = if b then "true" else "false"
showJSON x JSONNull = "null"
showJSON x (JSONList list) = "[" ++ intercalate ", " (map (showJSON x) list) ++ "]"

showJSONObject :: Int -> JSONObj -> String
showJSONObject indent obj =
  "{\n"
    ++ removeTrailingComma (unlines (map showPair (M.toList obj)))
    ++ replicate indent ' '
    ++ "}"
  where
    showPair (key, value) =
      replicate (indent + 4) ' ' ++ '\"' : key ++ "\": " ++ showJSON (indent + 4) value ++ ","

-- Remove trailing commas from the last pair in the JSON object
removeTrailingComma :: String -> String
removeTrailingComma str =
  case reverse str of
    '}' : '\n' : ',' : rest -> reverse ('}' : '\n' : rest)
    '}' : ',' : rest -> reverse ('}' : rest)
    '\n' : ',' : rest -> reverse ('\n' : rest)
    _ -> str

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
