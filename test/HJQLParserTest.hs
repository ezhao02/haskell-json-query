module QueryTest where

import Data.Map qualified as M
import HJQL
import HJQLParser
import JSONObject
import Parser qualified as P
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))

runHJQLParser :: String -> Either P.ParseError Query
runHJQLParser = P.parse hjqlP

test_parseHJQLWPair :: Test
test_parseHJQLWPair =
  "parse write instructions"
    ~: TestList
      [ "empty"
          ~: runHJQLParser
            "write {}"
          ~?= Right (Write (QueryBranch [])),
        "simple"
          ~: runHJQLParser
            " write {\n\
            \    \"key\": \"value\",\n\
            \}\
            \"
          ~?= Right (Write $ QueryBranch [QueryLeaf "key" (JSONStr "value")]),
        "nested"
          ~: runHJQLParser
            " write {\n\
            \    \"key\" {\
            \         \"otherkey\": 1\
            \      },\n\
            \}\n\
            \"
          ~?= Right
            ( Write $
                QueryBranch
                  [ QueryTwig "key" $
                      QueryBranch [QueryLeaf "otherkey" $ JSONNum 1]
                  ]
            ),
        "multiple, not nested"
          ~: runHJQLParser
            "write {\n\
            \    \"key\": null,\n\
            \    \"meow\": 0\n\
            \}\n\
            \"
          ~?= Right
            ( Write $
                QueryBranch
                  [ QueryLeaf "key" JSONNull,
                    QueryLeaf "meow" $ JSONNum 0
                  ]
            ),
        "multiple, nested, fancy JSON"
          ~: runHJQLParser
            "write {\n\
            \    \"key\": null,\n\
            \    \"meow\" {\n\
            \      \"warum\": {\"x\": \"hi\", \"y\": [\"bonk\", 0]}\n\
            \    }\n\
            \}"
          ~?= Right
            ( Write $
                QueryBranch
                  [ QueryLeaf "key" JSONNull,
                    QueryTwig "meow" $
                      QueryBranch
                        [ QueryLeaf "warum" $
                            JSONObj $
                              M.fromList
                                [ ("x", JSONStr "hi"),
                                  ("y", JSONList [JSONStr "bonk", JSONNum 0])
                                ]
                        ]
                  ]
            )
      ]

test_parseHJQLNoPair :: Test
test_parseHJQLNoPair =
  "parse read/delete instructions"
    ~: TestList
      [ "empty"
          ~: runHJQLParser
            "read {}"
          ~?= Right (Read (QueryBranch [])),
        "simple"
          ~: runHJQLParser
            " delete {\n\
            \    \"key\",\n\
            \}\
            \"
          ~?= Right (Delete $ QueryBranch [QueryLeaf "key" ()]),
        "nested"
          ~: runHJQLParser
            " delete {\n\
            \    \"key\" {\
            \         \"otherkey\"\
            \      },\n\
            \}\n\
            \"
          ~?= Right
            ( Delete $
                QueryBranch
                  [ QueryTwig "key" $
                      QueryBranch [QueryLeaf "otherkey" ()]
                  ]
            ),
        "multiple, not nested"
          ~: runHJQLParser
            "delete {\n\
            \    \"key\",\n\
            \    \"meow\"\n\
            \}\n\
            \"
          ~?= Right
            ( Delete $
                QueryBranch
                  [ QueryLeaf "key" (),
                    QueryLeaf "meow" ()
                  ]
            ),
        "multiple, nested"
          ~: runHJQLParser
            "read {\n\
            \    \"key\",\n\
            \    \"meow\" {\n\
            \      \"warum\",\n\
            \    }\n\
            \}"
          ~?= Right
            ( Read $
                QueryBranch
                  [ QueryLeaf "key" (),
                    QueryTwig "meow" $ QueryBranch [QueryLeaf "warum" ()]
                  ]
            )
      ]

-- >>> runTestTT test_parseHJQLWPair
-- Counts {cases = 5, tried = 5, errors = 0, failures = 0}

-- >>> runTestTT test_parseHJQLNoPair
-- Counts {cases = 5, tried = 5, errors = 0, failures = 0}
