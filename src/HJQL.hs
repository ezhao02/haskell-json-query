{-# LANGUAGE LambdaCase #-}

module HJQL where

import Data.Either (rights)
import Data.Function ((&))
import Data.List (intercalate, intersperse)
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
  deriving (Eq)

data QueryTree a
  = QueryLeaf Key a
  | QueryTwig Key (QueryTree a)
  | QueryBranch [QueryTree a]
  | QueryList Key (Map Key JSON) (QueryTree a)
  deriving (Eq, Show)

instance Show Query where
  show (Write t) = "write {\n" ++ showIndent ((": " ++) . show) 4 t ++ "\n}"
  show (Read t) = "read {\n" ++ showIndent (const "") 4 t ++ "\n}"
  show (Delete t) = "delete {\n" ++ showIndent (const "") 4 t ++ "\n}"

showQuery :: Query -> String
showQuery (Write t) = "write {\n" ++ showIndent ((": " ++) . show) 4 t ++ "\n}"
showQuery (Read t) = "read {\n" ++ showIndent (const "") 4 t ++ "\n}"
showQuery (Delete t) = "delete {\n" ++ showIndent (const "") 4 t ++ "\n}"

showIndent :: (a -> String) -> Int -> QueryTree a -> String
showIndent f i (QueryLeaf k j) =
  replicate i ' '
    ++ '"'
    : k
    ++ "\""
    ++ f j
showIndent f i (QueryTwig k t) =
  replicate i ' '
    ++ '"'
    : k
    ++ "\" {\n"
    ++ showIndent f (i + 4) t
    ++ "\n"
    ++ replicate i ' '
    ++ "}"
showIndent f i (QueryBranch ts) =
  intercalate ",\n" $ map (showIndent f i) ts
showIndent f i (QueryList k m t) =
  replicate i ' '
    ++ '"'
    : k
    ++ "\""
    ++ (if null m then "" else " | " ++ showFilter m)
    ++ " [\n"
    ++ showIndent f (i + 4) t
    ++ "\n"
    ++ replicate i ' '
    ++ "]"
  where
    showFilter :: JSONObj -> String
    showFilter m =
      m
        & M.toList
        & map (\(k, v) -> '\"' : k ++ "\" == " ++ show v)
        & intercalate " && "

-- instance Show (QueryTree ()) where
--   show = showIndent (const "") 0

-- instance Show (QueryTree JSON) where
--   show = showIndent (show :: JSON -> String) 0

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
