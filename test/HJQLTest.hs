module HJQLTest where

import Data.Map qualified as M
import HJQL (Query (..), QueryTree (..), runQuery)
import HJQLParser
import JSONObject
import Parser qualified as P
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))

jsonObj1 :: JSONObj
jsonObj1 = M.fromList [("key", JSONStr "string"), ("key2", JSONObj $ M.fromList [("key3", JSONNum 1.0)]), ("key4", JSONBool True)]

queryObj1 :: Query
queryObj1 = Read (QueryBranch [QueryLeaf "key" (), QueryTwig "key2" (QueryBranch [QueryLeaf "key3" ()])])

-- test_createPairs :: Test
-- test_createPairs =
--   TestList
--     [ createPairs jsonObj1 (JSONObj $ Map.fromList [("key2", JSONStr "string2")]) ~?= JSONObj (Map.fromList [("key", JSONStr "string"), ("key2", JSONStr "string2")]),
--       createPairs jsonObj1 (JSONObj $ Map.fromList [("key", JSONStr "string2")]) ~?= JSONObj (Map.fromList [("key", JSONStr "string2")]),
--       createPairs jsonObj1 (JSONObj $ Map.fromList [("key2", JSONNum 1.0)]) ~?= JSONObj (Map.fromList [("key", JSONStr "string"), ("key2", JSONNum 1.0)])
--     ]

test_read :: Test
test_read =
  TestList
    [ runQuery queryObj1 jsonObj1 ~?= Right (M.fromList [("key", JSONStr "string"), ("key2", JSONObj $ M.fromList [("key3", JSONNum 1.0)])])
    ]

-- >>> runTestTT test_read
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}

-- test_updatePairs :: Test
-- test_updatePairs =
--   TestList
--     [ updatePairs jsonObj1 (JSONObj $ Map.fromList [("key2", JSONStr "string2")]) ~?= Nothing,
--       updatePairs jsonObj1 (JSONObj $ Map.fromList [("key", JSONBool True)]) ~?= Just (JSONObj $ Map.fromList [("key", JSONBool True)]),
--       updatePairs jsonObj1 (JSONObj $ Map.fromList [("key2", JSONNum 1.0)]) ~?= Nothing
--     ]

-- test_deletePairs :: Test
-- test_deletePairs =
--   TestList
--     [ deletePairs ["key"] jsonObj1 ~?= JSONObj Map.empty,
--       deletePairs ["key", "key2"] jsonObj1 ~?= JSONObj Map.empty,
--       deletePairs ["key2"] jsonObj1 ~?= JSONObj (Map.fromList [("key", JSONStr "string")])
-- ]
