module HJQL where

import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as M
import JSONObject
import System.IO
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))

type Key = String

data Query
  = Write (QueryTree JSON)
  | Read (QueryTree ())
  | Delete (QueryTree ())
  deriving (Eq, Show)

data QueryTree a
  = QueryLeaf Key a
  | QueryTwig Key (QueryTree a)
  | QueryBranch [QueryTree a]
  deriving (Eq, Show)

{-
Three possible types of queries (Write, Read, Delete):
 * Delete the pairs corresponding to a list of keys from the JSON object
 * Read key–value pairs from the JSON object to display
 * Update existing key–value pairs in the JSON object
-}

-- type JSONObj = Map String JSON

-- data JSON
--   = JSONNum Float
--   | JSONStr String
--   | JSONObj JSONObj
--   | JSONBool Bool
--   | JSONNull
--   | JSONList [JSON]
--   deriving (Eq)

runQuery :: Query -> JSONObj -> Either String JSONObj
-- Read Query
runQuery (Read queryTree) doc =
  case queryTree of
    QueryBranch [] -> Right M.empty
    QueryBranch (b : ranches) ->
      case runQuery (Read b) doc of
        Left err -> Left err
        Right curr ->
          case runQuery (Read (QueryBranch ranches)) doc of
            Left err -> Left err
            Right rest -> Right $ M.union curr rest
    QueryTwig key tree ->
      case M.lookup key doc of
        Nothing -> Left $ "Key " ++ key ++ " not found"
        Just value -> case value of
          JSONObj obj ->
            case runQuery (Read tree) obj of
              Left err -> Left err
              Right child -> Right $ M.singleton key (JSONObj child)
          _ -> Left $ "Twig used on non-object on key " ++ key
    QueryLeaf key _ -> case M.lookup key doc of
      Nothing -> Left $ "Key " ++ key ++ " not found"
      Just val -> Right $ M.singleton key val
-- Write Query
runQuery (Write queryTree) doc =
  case queryTree of
    QueryBranch [] -> Right doc
    QueryBranch (b : ranches) ->
      case runQuery (Write b) doc of
        Left err -> Left err
        Right first ->
          case runQuery (Write (QueryBranch ranches)) first of
            Left err -> Left err
            Right last -> Right last
    QueryTwig key tree ->
      case M.lookup key doc of
        Nothing -> Left $ "Key " ++ key ++ " not found"
        Just value -> case value of
          JSONObj obj ->
            case runQuery (Write tree) obj of
              Left err -> Left err
              Right child -> Right $ M.adjust (\_ -> JSONObj child) key doc
          _ -> Left $ "Twig used on non-object on key " ++ key
    QueryLeaf key value -> Right $ M.insert key value doc
-- Delete Query
runQuery (Delete queryTree) doc =
  case queryTree of
    QueryBranch [] -> Right doc
    QueryBranch (b : ranches) ->
      case runQuery (Delete b) doc of
        Left err -> Left err
        Right first ->
          case runQuery (Delete (QueryBranch ranches)) first of
            Left err -> Left err
            Right last -> Right last
    QueryTwig key tree ->
      case M.lookup key doc of
        Nothing -> Right doc
        Just next -> case next of
          JSONObj obj ->
            case runQuery (Delete tree) obj of
              Left err -> Left err
              Right child -> Right $ M.adjust (\_ -> JSONObj child) key doc
          _ -> Left $ "Twig used on non-object on key " ++ key
    QueryLeaf key _ -> case M.lookup key doc of
      Nothing -> Left $ "Key " ++ key ++ " not found"
      Just _ -> Right $ M.delete key doc

showJSON :: Int -> JSON -> String
showJSON x (JSONNum n) = show n
showJSON x (JSONStr s) = show s
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
      replicate (indent + 4) ' ' ++ show key ++ ": " ++ showJSON (indent + 4) value ++ ","

-- Remove trailing commas from the last pair in the JSON object
removeTrailingComma :: String -> String
removeTrailingComma str =
  case reverse str of
    '}' : '\n' : ',' : rest -> reverse ('}' : '\n' : rest)
    '}' : ',' : rest -> reverse ('}' : rest)
    '\n' : ',' : rest -> reverse ('\n' : rest)
    _ -> str

-- Example usage:
exampleJSONObj :: JSONObj
exampleJSONObj =
  M.fromList
    [ ("name", JSONStr "John"),
      ("age", JSONNum 25),
      ("isStudent", JSONBool True),
      ("grades", JSONList [JSONNum 90, JSONNum 85, JSONNum 92]),
      ("address", JSONObj (M.fromList [("city", JSONStr "ExampleCity"), ("zip", JSONNum 12345), ("inner", JSONObj (M.fromList [("innerKey", JSONStr "innerValue")]))])),
      ("hasCar", JSONBool False),
      ("pets", JSONList [JSONObj (M.fromList [("type", JSONStr "Dog"), ("name", JSONStr "Buddy")])])
    ]

main :: IO ()
main = do
  let content = removeTrailingComma $ showJSON 0 (JSONObj exampleJSONObj)
  putStrLn content
  writeFile "writeExample.json" content