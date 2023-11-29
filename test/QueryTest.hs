module QueryTest where

import HJQL
import HJQLParser
import JSONObject
import Parser qualified as P
import Data.Map qualified as Map
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))

test_parseCreate :: Test
test_parseCreate =
    TestList
    [
        P.parse parseCreate "create {\"key\": \"string\"}" ~?= Right (JSONObj $ Map.fromList [("key", JSONStr "string")]),
        P.parse parseCreate "create {\"key\": 1}" ~?= Right (JSONObj $ Map.fromList [("key", JSONNum 1.0)]),
        P.parse parseCreate "create {\"key\": true}" ~?= Right (JSONObj $ Map.fromList [("key", JSONBool True)]),
        P.parse parseCreate "create {\"key\": false}" ~?= Right (JSONObj $ Map.fromList [("key", JSONBool False)]),
        P.parse parseCreate "create {\"key\": null}" ~?= Right (JSONObj $ Map.fromList [("key", JSONNull)]),
        P.parse parseCreate "create {\"key\": []}" ~?= Right (JSONObj $ Map.fromList [("key", JSONList [])]),
        P.parse parseCreate "create {\"key\": [1, 2, 3]}" ~?= Right (JSONObj $ Map.fromList [("key", JSONList [JSONNum 1.0, JSONNum 2.0, JSONNum 3.0])]),
        P.parse parseCreate "create {\"key\": {}}" ~?= Right (JSONObj $ Map.fromList [("key", JSONObj Map.empty)]),
        P.parse parseCreate "create {\"key\": {\"key\": \"string\"}}" ~?= Right (JSONObj $ Map.fromList [("key", JSONObj $ Map.fromList [("key", JSONStr "string")])])
    ]

test_parseRead :: Test
test_parseRead =
    TestList [
        P.parse parseRead "read [\"key\"]" ~?= Right ["key"],
        P.parse parseRead "read [\"key\", \"key2\"]" ~?= Right ["key", "key2"],
        P.parse parseRead "read []" ~?= Right [],
        P.parse parseRead "create [\"key\"]" ~?= Left "Expected 'read' but got 'create'",
        P.parse parseRead "read {\"key\": \"string\"}" ~?= Left "Expected '[' but got '{'"
    ]

test_parseUpdate :: Test
test_parseUpdate =
    TestList [
        P.parse parseUpdate "update {\"key\": \"string\"}" ~?= Right (JSONObj $ Map.fromList [("key", JSONStr "string")]),
        P.parse parseUpdate "update {\"key\": 1}" ~?= Right (JSONObj $ Map.fromList [("key", JSONNum 1.0)]),
        P.parse parseUpdate "update {\"key\": true}" ~?= Right (JSONObj $ Map.fromList [("key", JSONBool True)]),
        P.parse parseUpdate "update {\"key\": false}" ~?= Right (JSONObj $ Map.fromList [("key", JSONBool False)]),
        P.parse parseUpdate "update {\"key\": null}" ~?= Right (JSONObj $ Map.fromList [("key", JSONNull)]),
        P.parse parseUpdate "update {\"key\": []}" ~?= Right (JSONObj $ Map.fromList [("key", JSONList [])]),
        P.parse parseUpdate "update {\"key\": [1, 2, 3]}" ~?= Right (JSONObj $ Map.fromList [("key", JSONList [JSONNum 1.0, JSONNum 2.0, JSONNum 3.0])]),
        P.parse parseUpdate "update {\"key\": {}}" ~?= Right (JSONObj $ Map.fromList [("key", JSONObj $ Map.fromList [])]),
        P.parse parseUpdate "update {\"key\": {\"key\": \"string\"}}" ~?= Right (JSONObj $ Map.fromList [("key", JSONObj $ Map.fromList [("key", JSONStr "string")])])
    ]

test_parseDelete :: Test
test_parseDelete =
    TestList [
        P.parse parseDelete "delete [\"key\"]" ~?= Right ["key"],
        P.parse parseDelete "delete [\"key\", \"key2\"]" ~?= Right ["key", "key2"],
        P.parse parseDelete "delete []" ~?= Right [],
        P.parse parseDelete "create [\"key\"]" ~?= Left "Expected 'delete' but got 'create'",
        P.parse parseDelete "delete {\"key\": \"string\"}" ~?= Left "Expected '[' but got '{'"
    ]

jsonObj1 :: JSON
jsonObj1 = JSONObj $ Map.fromList [("key", JSONStr "string")]

test_createPairs :: Test
test_createPairs =
    TestList [
        createPairs jsonObj1 (JSONObj $ Map.fromList [("key2", "string2")]) ~?= JSONObj (Map.fromList [("key", JSONStr "string"), ("key2", JSONStr "string2")]),
        createPairs jsonObj1 (JSONObj $ Map.fromList [("key", "string2")]) ~?= JSONObj (Map.fromList [("key", JSONStr "string2")]),
        createPairs jsonObj1 (JSONObj $ Map.fromList [("key2", JSONNum 1.0)]) ~?= JSONObj (Map.fromList [("key", JSONStr "string"), ("key2", JSONNum 1.0)])
    ]

test_readPairs :: Test
test_readPairs =
    TestList [
        readPairs ["key"] jsonObj1 ~?= Just (JSONObj $ Map.fromList [("key", JSONStr "string")]),
        readPairs ["key", "key2"] jsonObj1 ~?= Just (JSONObj $ Map.fromList [("key", JSONStr "string")]),
        readPairs ["key2"] jsonObj1 ~?= Nothing
    ]

test_updatePairs :: Test
test_updatePairs =
    TestList [
        updatePairs jsonObj1 (JSONObj $ Map.fromList [("key2", "string2")]) ~?= Nothing,
        updatePairs jsonObj1 (JSONObj $ Map.fromList [("key", JSONBool True)]) ~?= Just (JSONObj $ Map.fromList [("key", JSONBool True)]),
        updatePairs jsonObj1 (JSONObj $ Map.fromList [("key2", JSONNum 1.0)]) ~?= Nothing
    ]

test_deletePairs :: Test
test_deletePairs =
    TestList [
        deletePairs ["key"] jsonObj1 ~?= JSONObj (Map.fromList []),
        deletePairs ["key", "key2"] jsonObj1 ~?= JSONObj (Map.fromList []),
        deletePairs ["key2"] jsonObj1 ~?= JSONObj (Map.fromList [("key", JSONStr "string")])
    ]