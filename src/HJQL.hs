module HJQL where

import Data.Map (Map)
import Data.Map qualified as M
import JSONObject (JSON, JSONObj)

type Key = String

data Query
  = Write (QueryTree JSON)
  | Read (QueryTree ())
  | Delete (QueryTree ())

data QueryTree a
  = QueryLeaf Key a
  | QueryTwig Key (QueryTree a)
  | QueryBranch [QueryTree a]

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

runQuery :: Query -> JSONObj -> JSONObj
runQuery (Write queryTree) doc = undefined
runQuery (Delete queryTree) doc = undefined
runQuery (Read queryTree) doc = undefined

-- case queryTree of
--   QueryBranch [] -> M.empty
--   QueryBranch (b:ranches) ->
--     let curr = runQuery (Read b) doc
--         rest = runQuery (Read (QueryBranch ranches)) doc
--         in M.union curr rest
--   QueryTwig key tree ->
--     case M.lookup key doc of
--       Nothing -> M.empty
--       Just next -> runQuery (Read ) next