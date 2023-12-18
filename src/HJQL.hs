{-# LANGUAGE LambdaCase #-}

module HJQL where

import Data.Either (rights)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (catMaybes)
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
  | QueryList Key (Map Key JSON) (QueryTree a)
  deriving (Eq, Show)

{-
Three possible types of queries (Write, Read, Delete):
 * Delete the pairs corresponding to a list of keys from the JSON object
 * Read key–value pairs from the JSON object to display
 * Update existing key–value pairs in the JSON object
-}

runQuery :: Query -> JSONObj -> Either String JSONObj
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
    QueryList key conditions tree -> Left $ "List used on write query on key " ++ key
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
    QueryList key conditions tree -> Left $ "List used on delete query on key " ++ key
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
    QueryList key conditions tree ->
      case M.lookup key doc of
        Nothing -> Left $ "Key " ++ key ++ " not found"
        Just value -> case value of
          JSONList list ->
            let jsonobjList =
                  catMaybes
                    [ case json of
                        JSONObj obj -> Just obj
                        _ -> Nothing
                      | json <- list
                    ]
                queriedList = map (runQuery (Read tree)) jsonobjList
                rightList = rights queriedList
                jsonList = map JSONObj rightList
                filteredList = filterList jsonList (M.toList conditions)
             in case filteredList of
                  [] -> Right M.empty
                  _ -> Right $ M.singleton key (JSONList filteredList)
          _ -> Left $ "List used on non-list on key " ++ key

-- | Filter a list of JSON objects by a list of key–value pairs
filterList :: [JSON] -> [(Key, JSON)] -> [JSON]
filterList list conditions =
  filter
    ( \case
        JSONObj obj ->
          all
            ( \(k, v) -> case M.lookup k obj of
                Nothing -> False
                Just val -> val == v
            )
            conditions
        _ -> False
    )
    list

-- jsonObj2 :: JSONObj
-- jsonObj2 =
--   M.fromList
--     [ ( "students",
--         JSONList
--           [ JSONStr "random string",
--             JSONNum 3.14,
--             JSONBool False,
--             JSONNull,
--             JSONList [JSONStr "hello", JSONBool True, JSONNum 5],
--             JSONObj $ M.fromList [("name", JSONStr "Jack"), ("major", JSONStr "CS"), ("gpa", JSONNum 3.5), ("classes", JSONObj $ M.fromList [("cis110", JSONNum 91), ("cis120", JSONNum 97), ("math201", JSONNum 93), ("math202", JSONNum 95)])],
--             JSONObj $ M.fromList [("name", JSONStr "Emma"), ("major", JSONStr "Math"), ("gpa", JSONNum 3.8), ("classes", JSONObj $ M.fromList [("cis110", JSONNum 95), ("math201", JSONNum 89), ("math202", JSONNum 92)])],
--             JSONObj $ M.fromList [("name", JSONStr "Charlie"), ("major", JSONStr "Physics"), ("gpa", JSONNum 3.6), ("classes", JSONObj $ M.fromList [("phy101", JSONNum 92), ("phy202", JSONNum 87), ("cis120", JSONNum 95)])],
--             JSONObj $ M.fromList [("name", JSONStr "Olivia"), ("major", JSONStr "CS"), ("gpa", JSONNum 3.9), ("classes", JSONObj $ M.fromList [("cis110", JSONNum 100), ("cis120", JSONNum 100), ("cis121", JSONNum 100), ("cis160", JSONNum 100)])],
--             JSONObj $ M.fromList [("name", JSONStr "Noah"), ("major", JSONStr "Math"), ("gpa", JSONNum 3.7), ("classes", JSONObj $ M.fromList [("math201", JSONNum 95), ("math202", JSONNum 93), ("math203", JSONNum 91)])]
--           ]
--       ),
--       ("obj", JSONObj $ M.fromList [("key1", JSONStr "value1"), ("key2", JSONStr "value2"), ("key3", JSONStr "value3")]),
--       ( "outer",
--         JSONList
--           [ JSONObj $ M.fromList [("middle", JSONList [JSONObj $ M.fromList [("inner", JSONList [JSONObj $ M.fromList [("name", JSONStr "Lambert"), ("occupation", JSONStr "Sheep")]])]])],
--             JSONObj $ M.fromList [("middle", JSONList [JSONObj $ M.fromList [("inner", JSONList [JSONObj $ M.fromList [("name", JSONStr "James"), ("occupation", JSONStr "Cow")]])]])]
--           ]
--       )
--     ]

-- -- Read tests for jsonObj2
-- queryRead11 :: Query
-- queryRead11 = Read (QueryLeaf "students" ())

-- queryRead12 :: Query
-- queryRead12 = Read (QueryList "students" M.empty (QueryLeaf "name" ()))

-- queryRead13 :: Query
-- queryRead13 = Read (QueryList "students" (M.fromList [("name", JSONStr "Jack")]) (QueryLeaf "name" ()))

-- queryRead14 :: Query
-- queryRead14 = Read (QueryList "students" M.empty (QueryTwig "classes" (QueryLeaf "cis110" ())))

-- queryRead15 :: Query
-- queryRead15 = Read (QueryList "students" (M.fromList [("classes", JSONObj (M.fromList [("cis110", JSONNum 100)]))]) (QueryTwig "classes" (QueryLeaf "cis110" ())))

-- queryRead16 :: Query
-- queryRead16 = Read (QueryList "students" M.empty (QueryBranch [QueryLeaf "name" (), QueryLeaf "major" (), QueryLeaf "classes" ()]))

-- queryRead17 :: Query
-- queryRead17 = Read (QueryList "students" M.empty (QueryBranch [QueryLeaf "name" (), QueryTwig "classes" (QueryBranch [QueryLeaf "cis110" (), QueryLeaf "cis120" ()])]))

-- queryRead18 :: Query
-- queryRead18 = Read (QueryList "students" (M.fromList [("classes", JSONObj $ M.fromList [("cis110", JSONNum 100)])]) (QueryBranch [QueryLeaf "name" (), QueryLeaf "major" (), QueryLeaf "gpa" (), QueryTwig "classes" (QueryLeaf "cis110" ())]))

-- queryRead19 :: Query
-- queryRead19 = Read (QueryBranch [QueryLeaf "obj" (), QueryList "students" (M.fromList [("classes", JSONObj $ M.fromList [("cis110", JSONNum 100)])]) (QueryBranch [QueryLeaf "name" (), QueryLeaf "major" (), QueryLeaf "gpa" (), QueryTwig "classes" (QueryLeaf "cis110" ())])])

-- queryRead20 :: Query
-- queryRead20 = Read (QueryList "students" M.empty (QueryBranch []))

-- queryRead21 :: Query
-- queryRead21 =
--   Read
--     ( QueryList
--         "outer"
--         (M.fromList [("middle", JSONList [JSONObj $ M.fromList [("inner", JSONList [JSONObj $ M.fromList [("name", JSONStr "Lambert"), ("occupation", JSONStr "Sheep")]])]])])
--         ( QueryList
--             "middle"
--             M.empty
--             (QueryList "inner" M.empty (QueryBranch [QueryLeaf "name" (), QueryLeaf "occupation" ()]))
--         )
--     )

-- test :: Test
-- test =
--   TestList
--     [ runQuery queryRead11 jsonObj2
--         ~?= Right
--           ( M.fromList
--               [ ( "students",
--                   JSONList
--                     [ JSONStr "random string",
--                       JSONNum 3.14,
--                       JSONBool False,
--                       JSONNull,
--                       JSONList [JSONStr "hello", JSONBool True, JSONNum 5],
--                       JSONObj $ M.fromList [("name", JSONStr "Jack"), ("major", JSONStr "CS"), ("gpa", JSONNum 3.5), ("classes", JSONObj $ M.fromList [("cis110", JSONNum 91), ("cis120", JSONNum 97), ("math201", JSONNum 93), ("math202", JSONNum 95)])],
--                       JSONObj $ M.fromList [("name", JSONStr "Emma"), ("major", JSONStr "Math"), ("gpa", JSONNum 3.8), ("classes", JSONObj $ M.fromList [("cis110", JSONNum 95), ("math201", JSONNum 89), ("math202", JSONNum 92)])],
--                       JSONObj $ M.fromList [("name", JSONStr "Charlie"), ("major", JSONStr "Physics"), ("gpa", JSONNum 3.6), ("classes", JSONObj $ M.fromList [("phy101", JSONNum 92), ("phy202", JSONNum 87), ("cis120", JSONNum 95)])],
--                       JSONObj $ M.fromList [("name", JSONStr "Olivia"), ("major", JSONStr "CS"), ("gpa", JSONNum 3.9), ("classes", JSONObj $ M.fromList [("cis110", JSONNum 100), ("cis120", JSONNum 100), ("cis121", JSONNum 100), ("cis160", JSONNum 100)])],
--                       JSONObj $ M.fromList [("name", JSONStr "Noah"), ("major", JSONStr "Math"), ("gpa", JSONNum 3.7), ("classes", JSONObj $ M.fromList [("math201", JSONNum 95), ("math202", JSONNum 93), ("math203", JSONNum 91)])]
--                     ]
--                 )
--               ]
--           ),
--       runQuery queryRead12 jsonObj2
--         ~?= Right
--           ( M.fromList
--               [ ( "students",
--                   JSONList
--                     [ JSONObj $ M.fromList [("name", JSONStr "Jack")],
--                       JSONObj $ M.fromList [("name", JSONStr "Emma")],
--                       JSONObj $ M.fromList [("name", JSONStr "Charlie")],
--                       JSONObj $ M.fromList [("name", JSONStr "Olivia")],
--                       JSONObj $ M.fromList [("name", JSONStr "Noah")]
--                     ]
--                 )
--               ]
--           ),
--       runQuery queryRead13 jsonObj2
--         ~?= Right
--           ( M.fromList
--               [ ( "students",
--                   JSONList [JSONObj $ M.fromList [("name", JSONStr "Jack")]]
--                 )
--               ]
--           ),
--       runQuery queryRead14 jsonObj2
--         ~?= Right
--           ( M.fromList
--               [ ( "students",
--                   JSONList
--                     [ JSONObj $ M.fromList [("classes", JSONObj $ M.fromList [("cis110", JSONNum 91)])],
--                       JSONObj $ M.fromList [("classes", JSONObj $ M.fromList [("cis110", JSONNum 95)])],
--                       JSONObj $ M.fromList [("classes", JSONObj $ M.fromList [("cis110", JSONNum 100)])]
--                     ]
--                 )
--               ]
--           ),
--       runQuery queryRead15 jsonObj2
--         ~?= Right
--           ( M.fromList
--               [ ( "students",
--                   JSONList [JSONObj $ M.fromList [("classes", JSONObj $ M.fromList [("cis110", JSONNum 100)])]]
--                 )
--               ]
--           ),
--       runQuery queryRead16 jsonObj2
--         ~?= Right
--           ( M.fromList
--               [ ( "students",
--                   JSONList
--                     [ JSONObj $ M.fromList [("name", JSONStr "Jack"), ("major", JSONStr "CS"), ("classes", JSONObj $ M.fromList [("cis110", JSONNum 91), ("cis120", JSONNum 97), ("math201", JSONNum 93), ("math202", JSONNum 95)])],
--                       JSONObj $ M.fromList [("name", JSONStr "Emma"), ("major", JSONStr "Math"), ("classes", JSONObj $ M.fromList [("cis110", JSONNum 95), ("math201", JSONNum 89), ("math202", JSONNum 92)])],
--                       JSONObj $ M.fromList [("name", JSONStr "Charlie"), ("major", JSONStr "Physics"), ("classes", JSONObj $ M.fromList [("phy101", JSONNum 92), ("phy202", JSONNum 87), ("cis120", JSONNum 95)])],
--                       JSONObj $ M.fromList [("name", JSONStr "Olivia"), ("major", JSONStr "CS"), ("classes", JSONObj $ M.fromList [("cis110", JSONNum 100), ("cis120", JSONNum 100), ("cis121", JSONNum 100), ("cis160", JSONNum 100)])],
--                       JSONObj $ M.fromList [("name", JSONStr "Noah"), ("major", JSONStr "Math"), ("classes", JSONObj $ M.fromList [("math201", JSONNum 95), ("math202", JSONNum 93), ("math203", JSONNum 91)])]
--                     ]
--                 )
--               ]
--           ),
--       runQuery queryRead17 jsonObj2
--         ~?= Right
--           ( M.fromList
--               [ ( "students",
--                   JSONList
--                     [ JSONObj $ M.fromList [("name", JSONStr "Jack"), ("classes", JSONObj $ M.fromList [("cis110", JSONNum 91), ("cis120", JSONNum 97)])],
--                       JSONObj $ M.fromList [("name", JSONStr "Olivia"), ("classes", JSONObj $ M.fromList [("cis110", JSONNum 100), ("cis120", JSONNum 100)])]
--                     ]
--                 )
--               ]
--           ),
--       runQuery queryRead18 jsonObj2
--         ~?= Right
--           ( M.fromList
--               [ ( "students",
--                   JSONList
--                     [JSONObj $ M.fromList [("name", JSONStr "Olivia"), ("major", JSONStr "CS"), ("gpa", JSONNum 3.9), ("classes", JSONObj $ M.fromList [("cis110", JSONNum 100)])]]
--                 )
--               ]
--           ),
--       runQuery queryRead19 jsonObj2
--         ~?= Right
--           ( M.fromList
--               [ ("obj", JSONObj $ M.fromList [("key1", JSONStr "value1"), ("key2", JSONStr "value2"), ("key3", JSONStr "value3")]),
--                 ( "students",
--                   JSONList
--                     [JSONObj $ M.fromList [("name", JSONStr "Olivia"), ("major", JSONStr "CS"), ("gpa", JSONNum 3.9), ("classes", JSONObj $ M.fromList [("cis110", JSONNum 100)])]]
--                 )
--               ]
--           ),
--       runQuery queryRead20 jsonObj2
--         ~?= Right
--           ( M.fromList
--               [ ( "students",
--                   JSONList [JSONObj M.empty, JSONObj M.empty, JSONObj M.empty, JSONObj M.empty, JSONObj M.empty]
--                 )
--               ]
--           ),
--       runQuery queryRead21 jsonObj2
--         ~?= Right
--           ( M.fromList
--               [ ( "outer",
--                   JSONList
--                     [JSONObj $ M.fromList [("middle", JSONList [JSONObj $ M.fromList [("inner", JSONList [JSONObj $ M.fromList [("name", JSONStr "Lambert"), ("occupation", JSONStr "Sheep")]])]])]]
--                 )
--               ]
--           )
--     ]

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