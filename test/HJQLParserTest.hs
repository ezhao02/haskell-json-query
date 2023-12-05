module QueryTest where

import Data.Map qualified as M
import HJQL
import HJQLParser
import JSONObject
import Parser qualified as P
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))

test_parseCreate :: Test
test_parseCreate =
  TestList
    [ P.parse parseCreate "create {\"key\": \"string\"}" ~?= Right (JSONObj $ M.fromList [("key", JSONStr "string")]),
      P.parse parseCreate "create {\"key\": 1}" ~?= Right (JSONObj $ M.fromList [("key", JSONNum 1.0)]),
      P.parse parseCreate "create {\"key\": true}" ~?= Right (JSONObj $ M.fromList [("key", JSONBool True)]),
      P.parse parseCreate "create {\"key\": false}" ~?= Right (JSONObj $ M.fromList [("key", JSONBool False)]),
      P.parse parseCreate "create {\"key\": null}" ~?= Right (JSONObj $ M.fromList [("key", JSONNull)]),
      P.parse parseCreate "create {\"key\": []}" ~?= Right (JSONObj $ M.fromList [("key", JSONList [])]),
      P.parse parseCreate "create {\"key\": [1, 2, 3]}" ~?= Right (JSONObj $ M.fromList [("key", JSONList [JSONNum 1.0, JSONNum 2.0, JSONNum 3.0])]),
      P.parse parseCreate "create {\"key\": {}}" ~?= Right (JSONObj $ M.fromList [("key", JSONObj M.empty)]),
      P.parse parseCreate "create {\"key\": {\"key\": \"string\"}}" ~?= Right (JSONObj $ M.fromList [("key", JSONObj $ M.fromList [("key", JSONStr "string")])])
    ]

test_parseRead :: Test
test_parseRead =
  TestList
    [ P.parse parseRead "read [\"key\"]" ~?= Right ["key"],
      P.parse parseRead "read [\"key\", \"key2\"]" ~?= Right ["key", "key2"],
      P.parse parseRead "read []" ~?= Right [],
      P.parse parseRead "create [\"key\"]" ~?= Left "Expected 'read' but got 'create'",
      P.parse parseRead "read {\"key\": \"string\"}" ~?= Left "Expected '[' but got '{'"
    ]

test_parseUpdate :: Test
test_parseUpdate =
  TestList
    [ P.parse parseUpdate "update {\"key\": \"string\"}" ~?= Right (JSONObj $ M.fromList [("key", JSONStr "string")]),
      P.parse parseUpdate "update {\"key\": 1}" ~?= Right (JSONObj $ M.fromList [("key", JSONNum 1.0)]),
      P.parse parseUpdate "update {\"key\": true}" ~?= Right (JSONObj $ M.fromList [("key", JSONBool True)]),
      P.parse parseUpdate "update {\"key\": false}" ~?= Right (JSONObj $ M.fromList [("key", JSONBool False)]),
      P.parse parseUpdate "update {\"key\": null}" ~?= Right (JSONObj $ M.fromList [("key", JSONNull)]),
      P.parse parseUpdate "update {\"key\": []}" ~?= Right (JSONObj $ M.fromList [("key", JSONList [])]),
      P.parse parseUpdate "update {\"key\": [1, 2, 3]}" ~?= Right (JSONObj $ M.fromList [("key", JSONList [JSONNum 1.0, JSONNum 2.0, JSONNum 3.0])]),
      P.parse parseUpdate "update {\"key\": {}}" ~?= Right (JSONObj $ M.fromList [("key", JSONObj M.empty)]),
      P.parse parseUpdate "update {\"key\": {\"key\": \"string\"}}" ~?= Right (JSONObj $ M.fromList [("key", JSONObj $ M.fromList [("key", JSONStr "string")])])
    ]

test_parseDelete :: Test
test_parseDelete =
  TestList
    [ P.parse parseDelete "delete [\"key\"]" ~?= Right ["key"],
      P.parse parseDelete "delete [\"key\", \"key2\"]" ~?= Right ["key", "key2"],
      P.parse parseDelete "delete []" ~?= Right [],
      P.parse parseDelete "create [\"key\"]" ~?= Left "Expected 'delete' but got 'create'",
      P.parse parseDelete "delete {\"key\": \"string\"}" ~?= Left "Expected '[' but got '{'"
    ]
