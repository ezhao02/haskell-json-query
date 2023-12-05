module HJQL where

import Data.Map (Map)
import Data.Map qualified as M
import JSONObject
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

-- TODO: technically the documents should always be JSONObject, not just any JSON, so maybe use GADTs?

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

jsonObj1 :: JSONObj
jsonObj1 = M.fromList [("key", JSONStr "string"), ("key2", JSONObj $ M.fromList [("key3", JSONNum 1.0), ("key6", JSONNull)]), ("key4", JSONBool True), ("key5", JSONList [JSONNum 1.0, JSONNum 2.0])]

queryRead1 :: Query
queryRead1 = Read (QueryBranch [QueryLeaf "key" (), QueryTwig "key2" (QueryLeaf "key6" ()), QueryLeaf "key5" ()])

queryRead2 :: Query
queryRead2 = Read (QueryTwig "key2" (QueryLeaf "key3" ()))

queryDelete1 :: Query
queryDelete1 = Delete (QueryBranch [QueryTwig "key2" (QueryLeaf "key3" ()), QueryLeaf "key5" ()])

queryDelete2 :: Query
queryDelete2 = Delete (QueryBranch [QueryLeaf "key4" (), QueryLeaf "key2" ()])

queryWrite1 :: Query
queryWrite1 = Write (QueryLeaf "key" (JSONStr "string2"))

queryWrite2 :: Query
queryWrite2 = Write (QueryTwig "key2" (QueryLeaf "key3" (JSONBool False)))

queryWrite3 :: Query
queryWrite3 = Write (QueryBranch [QueryTwig "key2" (QueryLeaf "key6" (JSONBool False)), QueryLeaf "key5" (JSONList [JSONNum 3.0]), QueryLeaf "key" (JSONObj M.empty)])

test_a :: Test
test_a =
  TestList
    [ runQuery queryRead1 jsonObj1 ~?= Right (M.fromList [("key", JSONStr "string"), ("key2", JSONObj $ M.fromList [("key6", JSONNull)]), ("key5", JSONList [JSONNum 1.0, JSONNum 2.0])]),
      runQuery queryDelete1 jsonObj1 ~?= Right (M.fromList [("key", JSONStr "string"), ("key2", JSONObj (M.fromList [("key6", JSONNull)])), ("key4", JSONBool True)])
    ]
