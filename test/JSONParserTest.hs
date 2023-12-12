module JSONParserTest where

import Data.Map qualified as M
import HJQLParser
import JSONObject
import JSONParser
import Parser qualified as P
import Test.HUnit
import Test.QuickCheck

instance Arbitrary JSON where
  arbitrary :: Gen JSON
  arbitrary = undefined

prop_roundtripJSON :: JSON -> Bool
prop_roundtripJSON j = P.doParse jsonP (show j) == Just (j, "")

runJSONParser :: String -> Maybe (JSON, String)
runJSONParser = P.doParse jsonP

testJSONParsePrimitives :: Test
testJSONParsePrimitives =
  "JSON parsing"
    ~: TestList
      [ runJSONParser "{}" ~?= Just (JSONObj M.empty, ""),
        runJSONParser "1" ~?= Just (JSONNum 1.0, ""),
        runJSONParser "-0.3" ~?= Just (JSONNum (-0.3), ""),
        runJSONParser "true" ~?= Just (JSONBool True, ""),
        runJSONParser "[]" ~?= Just (JSONList [], ""),
        runJSONParser "null" ~?= Just (JSONNull, ""),
        runJSONParser "\"xxx\"" ~?= Just (JSONStr "xxx", ""),
        runJSONParser "\"\"" ~?= Just (JSONStr "", "")
      ]

-- >>> runTestTT testJSONParsePrimitives
-- Counts {cases = 8, tried = 8, errors = 0, failures = 0}

testJSONParseObject :: Test
testJSONParseObject =
  "JSON Object parsing"
    ~: TestList
      [ runJSONParser
          "    {\
          \\n           \"a\": \"hi\",\
          \   \
          \  \
          \            \"xwefoijoij\"    :3  ,\
          \           \"bon\":true\
          \     } "
          ~?= Just
            (JSONObj (M.fromList [("bon", JSONBool True), ("xwefoijoij", JSONNum 3.0), ("a", JSONStr "hi")]), ""),
        runJSONParser "{\"a\":{},\"\":[1, true, true, null, {\"x\":3.2}], \"x\": {\"b\":[],}}"
          ~?= Just
            ( JSONObj
                ( M.fromList
                    [ ("a", JSONObj M.empty),
                      ( "",
                        JSONList
                          [ JSONNum 1.0,
                            JSONBool True,
                            JSONBool True,
                            JSONNull,
                            JSONObj (M.fromList [("x", JSONNum 3.2)])
                          ]
                      ),
                      ("x", JSONObj (M.fromList [("b", JSONList [])]))
                    ]
                ),
              ""
            )
      ]

-- >>> runTestTT testJSONParseObject
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}

testJSONParseList :: Test
testJSONParseList =
  "JSON List Parsing"
    ~: TestList
      [ runJSONParser "[1, 2, 3]" ~?= Just (JSONList [JSONNum 1.0, JSONNum 2.0, JSONNum 3.0], ""),
        runJSONParser "[1,]" ~?= Just (JSONList [JSONNum 1.0], ""),
        runJSONParser "[[true, false, true], [null, [true, 0]], [[\"x\",\"y\"],[],[\"z\"]],]"
          ~?= Just
            ( JSONList
                [ JSONList [JSONBool True, JSONBool False, JSONBool True],
                  JSONList [JSONNull, JSONList [JSONBool True, JSONNum 0.0]],
                  JSONList
                    [ JSONList [JSONStr "x", JSONStr "y"],
                      JSONList [],
                      JSONList [JSONStr "z"]
                    ]
                ],
              ""
            )
      ]

-- >>> runTestTT testJSONParseList
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}
