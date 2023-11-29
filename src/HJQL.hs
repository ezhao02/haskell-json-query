module HJQL where

import HJQLParser
import JSONObject (JSON)

{-
Four possible types of queries (CRUD):
 * Delete the pairs corresponding to a list of keys from the JSON object
 * Add key–value pairs to the JSON object
 * Select key–value pairs from the JSON object to display
 * Update existing key–value pairs in the JSON object
-}

-- TODO: technically the documents should always be JSONObject, not just any JSON, so maybe use GADTs?

-- | Delete the pairs with the input strings as keys from the JSON object
deletePairs :: [String] -> JSON -> JSON
deletePairs query doc = undefined

-- | Query a JSON object and return the requested data
readPairs :: [String] -> JSON -> Maybe JSON
readPairs query doc = undefined

-- | Update a JSON object and return the modified data, but only if the keys
-- already exist
updatePairs :: JSON -> JSON -> Maybe JSON
updatePairs query doc = undefined

-- | Update a JSON object and return the modified data regardless of whether
-- the keys exist
createPairs :: JSON -> JSON -> JSON
createPairs query doc = undefined
