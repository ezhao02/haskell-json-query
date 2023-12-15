{-# LANGUAGE LambdaCase #-}

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

-- grade <= 100 (left is always a key) && name == eric

data QueryTree a
  = QueryLeaf Key a
  | QueryTwig Key (QueryTree a)
  | QueryBranch [QueryTree a]
  | QueryList Key (Map Key String) [Key] (QueryTree a)
  deriving (Eq, Show)

{-
Three possible types of queries (Write, Read, Delete):
 * Delete the pairs corresponding to a list of keys from the JSON object
 * Read key–value pairs from the JSON object to display
 * Update existing key–value pairs in the JSON object
-}

runQuery :: Query -> JSONObj -> Either String JSONObj
-- Read Query
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
    QueryList key conditions values tree -> Left $ "List used on write query on key " ++ key
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
    QueryList key conditions values tree -> Left $ "List used on delete query on key " ++ key
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
    QueryList key conditions keys tree ->
      case M.lookup key doc of
        Nothing -> Left $ "Key " ++ key ++ " not found"
        Just value -> case value of
          JSONList list ->
            let filteredList = filterList list (M.toList conditions)
             in case filteredList of
                  [] -> Left $ "No items found in list on key " ++ key
                  _ ->
                    let selectedPairs =
                          map
                            ( \case
                                JSONObj obj ->
                                  (key, JSONObj $ M.fromList $ filter (\(k, _) -> k `elem` keys) $ M.toList obj)
                                _ -> error "Unexpected JSON format"
                            )
                            filteredList
                     in Right $ M.fromList selectedPairs
          _ -> Left $ "List used on non-list on key " ++ key

filterList :: [JSON] -> [(Key, String)] -> [JSON]
filterList list conditions =
  filter
    ( \case
        JSONObj obj ->
          all
            ( \(k, v) -> case M.lookup k obj of
                Nothing -> False
                Just val -> show val == v
            )
            conditions
        _ -> False
    )
    list

-- runReadQuery :: Query -> JSONObj -> Either String JSONObj
-- runReadQuery (Read queryTree) doc =
--   case queryTree of
--     QueryBranch [] -> Right M.empty
--     QueryBranch (b : ranches) ->
--       case runReadQuery (Read b) doc of
--         Left err -> Left err
--         Right curr ->
--           case runReadQuery (Read (QueryBranch ranches)) doc of
--             Left err -> Left err
--             Right rest -> Right $ M.union curr rest
--     QueryTwig key tree ->
--       case M.lookup key doc of
--         Nothing -> Left $ "Key " ++ key ++ " not found"
--         Just value -> case value of
--           JSONObj obj ->
--             case runReadQuery (Read tree) obj of
--               Left err -> Left err
--               Right child -> Right $ M.singleton key (JSONObj child)
--           _ -> Left $ "Twig used on non-object on key " ++ key
--     QueryLeaf key _ -> case M.lookup key doc of
--       Nothing -> Left $ "Key " ++ key ++ " not found"
--       Just val -> Right $ M.singleton key val
--     QueryList key conditions values tree ->
--       case M.lookup key doc of
--         Nothing -> Left $ "Key " ++ key ++ " not found"
--         Just value -> case value of
--           JSONList list ->
--             let filteredList = filter (\item -> all (\(k, v) -> case M.lookup k item of
--                   Nothing -> False
--                   Just val -> val == v) conditions) list
--             in case filteredList of
--                   [] -> Left $ "No items found in list on key " ++ key
--                   _ -> Right $ M.singleton key (JSONList (map (JSONObj . M.fromList) filteredList))
--           _ -> Left $ "List used on non-list on key " ++ key
-- runReadQuery _ _ = Left "Not a read query"

-- runWriteQuery :: Query -> JSONObj -> Either String JSONObj
-- runWriteQuery (Write queryTree) doc =
--   case queryTree of
--     QueryBranch [] -> Right doc
--     QueryBranch (b : ranches) ->
--       case runWriteQuery (Write b) doc of
--         Left err -> Left err
--         Right first ->
--           case runWriteQuery (Write (QueryBranch ranches)) first of
--             Left err -> Left err
--             Right last -> Right last
--     QueryTwig key tree ->
--       case M.lookup key doc of
--         Nothing -> Left $ "Key " ++ key ++ " not found"
--         Just value -> case value of
--           JSONObj obj ->
--             case runWriteQuery (Write tree) obj of
--               Left err -> Left err
--               Right child -> Right $ M.adjust (\_ -> JSONObj child) key doc
--           _ -> Left $ "Twig used on non-object on key " ++ key
--     QueryLeaf key value -> Right $ M.insert key value doc
--     QueryList {} -> Left $ "List used on write query on key " ++ key
-- runWriteQuery _ _ = Left "Not a write query"

-- runDeleteQuery :: Query -> JSONObj -> Either String JSONObj
-- runDeleteQuery (Delete queryTree) doc =
--   case queryTree of
--     QueryBranch [] -> Right doc
--     QueryBranch (b : ranches) ->
--       case runDeleteQuery (Delete b) doc of
--         Left err -> Left err
--         Right first ->
--           case runDeleteQuery (Delete (QueryBranch ranches)) first of
--             Left err -> Left err
--             Right last -> Right last
--     QueryTwig key tree ->
--       case M.lookup key doc of
--         Nothing -> Right doc
--         Just next -> case next of
--           JSONObj obj ->
--             case runDeleteQuery (Delete tree) obj of
--               Left err -> Left err
--               Right child -> Right $ M.adjust (\_ -> JSONObj child) key doc
--           _ -> Left $ "Twig used on non-object on key " ++ key
--     QueryLeaf key _ -> case M.lookup key doc of
--       Nothing -> Left $ "Key " ++ key ++ " not found"
--       Just _ -> Right $ M.delete key doc
--     QueryList {} -> Left $ "List used on delete query on key " ++ key
-- runDeleteQuery _ _ = Left "Not a delete query"

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