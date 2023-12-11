module HJQLTest where

import Data.Map qualified as M
import HJQL
import HJQLParser
import JSONObject
import Parser qualified as P
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))

jsonObjBig1 :: JSONObj
jsonObjBig1 =
  M.fromList
    [ ("username", JSONStr "johndoe"),
      ("age", JSONNum 28),
      ("isPremiumMember", JSONBool True),
      ( "address",
        JSONObj $
          M.fromList
            [ ("street", JSONStr "123 Elm Street"),
              ("city", JSONStr "Anytown"),
              ("zipcode", JSONNum 12345)
            ]
      ),
      ( "contact",
        JSONObj $
          M.fromList
            [ ("email", JSONStr "john.doe@example.com"),
              ("phone", JSONStr "+123456789")
            ]
      ),
      ("favoriteNumbers", JSONList [JSONNum 7, JSONNum 13, JSONNum 42])
    ]

jsonObjBig2 :: JSONObj
jsonObjBig2 =
  M.fromList
    [ ("productName", JSONStr "UltraWidget 3000"),
      ("productID", JSONNum 98765),
      ("available", JSONBool True),
      ( "details",
        JSONObj $
          M.fromList
            [ ("manufacturer", JSONStr "WidgetCorp"),
              ("price", JSONNum 49.99),
              ( "specs",
                JSONObj $
                  M.fromList
                    [ ("weight", JSONNum 500), -- grams
                      ("dimensions", JSONStr "10x20x5"), -- centimeters
                      ("color", JSONStr "blue"),
                      ("warranty", JSONStr "2 years")
                    ]
              ),
              ("tags", JSONList [JSONStr "electronics", JSONStr "gadgets", JSONStr "widgets"])
            ]
      )
    ]

jsonObj1 :: JSONObj
jsonObj1 =
  M.fromList
    [ ("string", JSONStr "some string"),
      ("emptyString", JSONStr ""),
      ("emptyObject", JSONObj M.empty),
      ("nullValue", JSONNull),
      ("boolValue", JSONBool False),
      ("simpleNum", JSONNum 3.6),
      ("negNum", JSONNum (-56)),
      ("simpleObject", JSONObj $ M.fromList [("key3", JSONNum 1.0), ("key6", JSONNull)]),
      ("numList", JSONList [JSONNum 1.0, JSONNum 2.0]),
      ("mixedList", JSONList [JSONStr "hello", JSONBool True, JSONNum 5]),
      ("deepNest", JSONObj $ M.fromList [("level2", JSONObj $ M.fromList [("level3", JSONStr "deep"), ("level3a", JSONNum 2.0)]), ("level2a", JSONNum 1.0)]),
      ("complex", JSONObj $ M.fromList [("num", JSONNum 3.14), ("obj", JSONObj $ M.fromList [("bool", JSONBool False)])])
    ]

-- Read tests for jsonObj1
queryBadRead1 :: Query
queryBadRead1 = Read (QueryLeaf "badKey" ())

queryBadRead2 :: Query
queryBadRead2 = Read (QueryTwig "emptyString" (QueryLeaf "emptyString" ()))

queryRead1 :: Query
queryRead1 = Read (QueryLeaf "string" ())

queryRead2 :: Query
queryRead2 = Read (QueryLeaf "numList" ())

queryRead3 :: Query
queryRead3 = Read (QueryLeaf "nullValue" ())

queryRead4 :: Query
queryRead4 = Read (QueryLeaf "simpleObject" ())

queryRead5 :: Query
queryRead5 = Read (QueryTwig "simpleObject" (QueryLeaf "key3" ()))

queryRead6 :: Query
queryRead6 = Read (QueryTwig "deepNest" (QueryTwig "level2" (QueryLeaf "level3" ())))

queryRead7 :: Query
queryRead7 = Read (QueryLeaf "numList" ())

queryRead8 :: Query
queryRead8 = Read (QueryLeaf "mixedList" ())

queryRead9 :: Query
queryRead9 =
  Read
    ( QueryBranch
        [ QueryLeaf "emptyString" (),
          QueryLeaf "emptyObject" (),
          QueryLeaf "boolValue" (),
          QueryTwig "simpleObject" (QueryLeaf "key6" ()),
          QueryTwig "complex" (QueryTwig "obj" (QueryLeaf "bool" ()))
        ]
    )

queryRead10 :: Query
queryRead10 = Read (QueryBranch [])

test_read :: Test
test_read =
  TestList
    [ runQuery queryRead1 jsonObj1 ~?= Right (M.fromList [("string", JSONStr "some string")]),
      runQuery queryRead2 jsonObj1 ~?= Right (M.fromList [("numList", JSONList [JSONNum 1.0, JSONNum 2.0])]),
      runQuery queryRead3 jsonObj1 ~?= Right (M.fromList [("nullValue", JSONNull)]),
      runQuery queryRead4 jsonObj1 ~?= Right (M.fromList [("simpleObject", JSONObj $ M.fromList [("key3", JSONNum 1.0), ("key6", JSONNull)])]),
      runQuery queryRead5 jsonObj1 ~?= Right (M.fromList [("simpleObject", JSONObj $ M.fromList [("key3", JSONNum 1.0)])]),
      runQuery queryRead6 jsonObj1 ~?= Right (M.fromList [("deepNest", JSONObj $ M.fromList [("level2", JSONObj $ M.fromList [("level3", JSONStr "deep")])])]),
      runQuery queryRead7 jsonObj1 ~?= Right (M.fromList [("numList", JSONList [JSONNum 1.0, JSONNum 2.0])]),
      runQuery queryRead8 jsonObj1 ~?= Right (M.fromList [("mixedList", JSONList [JSONStr "hello", JSONBool True, JSONNum 5])]),
      runQuery queryRead9 jsonObj1 ~?= Right (M.fromList [("emptyString", JSONStr ""), ("emptyObject", JSONObj M.empty), ("boolValue", JSONBool False), ("simpleObject", JSONObj $ M.fromList [("key6", JSONNull)]), ("complex", JSONObj $ M.fromList [("obj", JSONObj $ M.fromList [("bool", JSONBool False)])])]),
      runQuery queryBadRead1 jsonObj1 ~?= Left "Key badKey not found",
      runQuery queryBadRead2 jsonObj1 ~?= Left "Twig used on non-object on key emptyString",
      runQuery queryRead10 jsonObj1 ~?= Right M.empty
    ]

queryWrite1 :: Query
queryWrite1 = Write (QueryLeaf "string" (JSONStr "string2"))

queryWrite2 :: Query
queryWrite2 = Write (QueryTwig "simpleObject" (QueryLeaf "key3" (JSONBool False)))

queryWrite3 :: Query
queryWrite3 = Write (QueryBranch [])

queryWrite4 :: Query
queryWrite4 = Write (QueryBranch [QueryTwig "simpleObject" (QueryLeaf "key3" (JSONBool False)), QueryTwig "complex" (QueryTwig "obj" (QueryLeaf "bool" (JSONBool True)))])

queryWrite5 :: Query
queryWrite5 = Write (QueryTwig "simpleObject" (QueryBranch [QueryLeaf "key3" JSONNull, QueryLeaf "key6" (JSONNum 2.0)]))

test_write :: Test
test_write =
  TestList
    [ runQuery queryWrite1 jsonObj1
        ~?= Right
          ( M.fromList
              [ ("string", JSONStr "string2"),
                ("emptyString", JSONStr ""),
                ("emptyObject", JSONObj M.empty),
                ("nullValue", JSONNull),
                ("boolValue", JSONBool False),
                ("simpleNum", JSONNum 3.6),
                ("negNum", JSONNum (-56)),
                ("simpleObject", JSONObj $ M.fromList [("key3", JSONNum 1.0), ("key6", JSONNull)]),
                ("numList", JSONList [JSONNum 1.0, JSONNum 2.0]),
                ("mixedList", JSONList [JSONStr "hello", JSONBool True, JSONNum 5]),
                ("deepNest", JSONObj $ M.fromList [("level2", JSONObj $ M.fromList [("level3", JSONStr "deep"), ("level3a", JSONNum 2.0)]), ("level2a", JSONNum 1.0)]),
                ("complex", JSONObj $ M.fromList [("num", JSONNum 3.14), ("obj", JSONObj $ M.fromList [("bool", JSONBool False)])])
              ]
          ),
      runQuery queryWrite2 jsonObj1
        ~?= Right
          ( M.fromList
              [ ("string", JSONStr "some string"),
                ("emptyString", JSONStr ""),
                ("emptyObject", JSONObj M.empty),
                ("nullValue", JSONNull),
                ("boolValue", JSONBool False),
                ("simpleNum", JSONNum 3.6),
                ("negNum", JSONNum (-56)),
                ("simpleObject", JSONObj $ M.fromList [("key3", JSONBool False), ("key6", JSONNull)]),
                ("numList", JSONList [JSONNum 1.0, JSONNum 2.0]),
                ("mixedList", JSONList [JSONStr "hello", JSONBool True, JSONNum 5]),
                ("deepNest", JSONObj $ M.fromList [("level2", JSONObj $ M.fromList [("level3", JSONStr "deep"), ("level3a", JSONNum 2.0)]), ("level2a", JSONNum 1.0)]),
                ("complex", JSONObj $ M.fromList [("num", JSONNum 3.14), ("obj", JSONObj $ M.fromList [("bool", JSONBool False)])])
              ]
          ),
      runQuery queryWrite3 jsonObj1
        ~?= Right
          ( M.fromList
              [ ("string", JSONStr "some string"),
                ("emptyString", JSONStr ""),
                ("emptyObject", JSONObj M.empty),
                ("nullValue", JSONNull),
                ("boolValue", JSONBool False),
                ("simpleNum", JSONNum 3.6),
                ("negNum", JSONNum (-56)),
                ("simpleObject", JSONObj $ M.fromList [("key3", JSONNum 1.0), ("key6", JSONNull)]),
                ("numList", JSONList [JSONNum 1.0, JSONNum 2.0]),
                ("mixedList", JSONList [JSONStr "hello", JSONBool True, JSONNum 5]),
                ("deepNest", JSONObj $ M.fromList [("level2", JSONObj $ M.fromList [("level3", JSONStr "deep"), ("level3a", JSONNum 2.0)]), ("level2a", JSONNum 1.0)]),
                ("complex", JSONObj $ M.fromList [("num", JSONNum 3.14), ("obj", JSONObj $ M.fromList [("bool", JSONBool False)])])
              ]
          ),
      runQuery queryWrite4 jsonObj1
        ~?= Right
          ( M.fromList
              [ ("string", JSONStr "some string"),
                ("emptyString", JSONStr ""),
                ("emptyObject", JSONObj M.empty),
                ("nullValue", JSONNull),
                ("boolValue", JSONBool False),
                ("simpleNum", JSONNum 3.6),
                ("negNum", JSONNum (-56)),
                ("simpleObject", JSONObj $ M.fromList [("key3", JSONBool False), ("key6", JSONNull)]),
                ("numList", JSONList [JSONNum 1.0, JSONNum 2.0]),
                ("mixedList", JSONList [JSONStr "hello", JSONBool True, JSONNum 5]),
                ("deepNest", JSONObj $ M.fromList [("level2", JSONObj $ M.fromList [("level3", JSONStr "deep"), ("level3a", JSONNum 2.0)]), ("level2a", JSONNum 1.0)]),
                ("complex", JSONObj $ M.fromList [("num", JSONNum 3.14), ("obj", JSONObj $ M.fromList [("bool", JSONBool True)])])
              ]
          ),
      runQuery queryWrite5 jsonObj1
        ~?= Right
          ( M.fromList
              [ ("string", JSONStr "some string"),
                ("emptyString", JSONStr ""),
                ("emptyObject", JSONObj M.empty),
                ("nullValue", JSONNull),
                ("boolValue", JSONBool False),
                ("simpleNum", JSONNum 3.6),
                ("negNum", JSONNum (-56)),
                ("simpleObject", JSONObj $ M.fromList [("key3", JSONNull), ("key6", JSONNum 2.0)]),
                ("numList", JSONList [JSONNum 1.0, JSONNum 2.0]),
                ("mixedList", JSONList [JSONStr "hello", JSONBool True, JSONNum 5]),
                ("deepNest", JSONObj $ M.fromList [("level2", JSONObj $ M.fromList [("level3", JSONStr "deep"), ("level3a", JSONNum 2.0)]), ("level2a", JSONNum 1.0)]),
                ("complex", JSONObj $ M.fromList [("num", JSONNum 3.14), ("obj", JSONObj $ M.fromList [("bool", JSONBool False)])])
              ]
          )
    ]

queryDelete1 :: Query
queryDelete1 = Delete (QueryLeaf "string" ())

queryDelete2 :: Query
queryDelete2 = Delete (QueryTwig "simpleObject" (QueryBranch [QueryLeaf "key3" (), QueryLeaf "key6" ()]))

queryDelete3 :: Query
queryDelete3 = Delete (QueryBranch [QueryLeaf "boolValue" (), QueryLeaf "simpleObject" (), QueryTwig "complex" (QueryTwig "obj" (QueryLeaf "bool" ()))])

test_delete :: Test
test_delete =
  TestList
    [ runQuery queryDelete1 jsonObj1
        ~?= Right
          ( M.fromList
              [ ("emptyString", JSONStr ""),
                ("emptyObject", JSONObj M.empty),
                ("nullValue", JSONNull),
                ("boolValue", JSONBool False),
                ("simpleNum", JSONNum 3.6),
                ("negNum", JSONNum (-56)),
                ("simpleObject", JSONObj $ M.fromList [("key3", JSONNum 1.0), ("key6", JSONNull)]),
                ("numList", JSONList [JSONNum 1.0, JSONNum 2.0]),
                ("mixedList", JSONList [JSONStr "hello", JSONBool True, JSONNum 5]),
                ("deepNest", JSONObj $ M.fromList [("level2", JSONObj $ M.fromList [("level3", JSONStr "deep"), ("level3a", JSONNum 2.0)]), ("level2a", JSONNum 1.0)]),
                ("complex", JSONObj $ M.fromList [("num", JSONNum 3.14), ("obj", JSONObj $ M.fromList [("bool", JSONBool False)])])
              ]
          ),
      runQuery queryDelete2 jsonObj1
        ~?= Right
          ( M.fromList
              [ ("string", JSONStr "some string"),
                ("emptyString", JSONStr ""),
                ("emptyObject", JSONObj M.empty),
                ("nullValue", JSONNull),
                ("boolValue", JSONBool False),
                ("simpleNum", JSONNum 3.6),
                ("negNum", JSONNum (-56)),
                ("simpleObject", JSONObj M.empty),
                ("numList", JSONList [JSONNum 1.0, JSONNum 2.0]),
                ("mixedList", JSONList [JSONStr "hello", JSONBool True, JSONNum 5]),
                ("deepNest", JSONObj $ M.fromList [("level2", JSONObj $ M.fromList [("level3", JSONStr "deep"), ("level3a", JSONNum 2.0)]), ("level2a", JSONNum 1.0)]),
                ("complex", JSONObj $ M.fromList [("num", JSONNum 3.14), ("obj", JSONObj $ M.fromList [("bool", JSONBool False)])])
              ]
          ),
      runQuery queryDelete3 jsonObj1
        ~?= Right
          ( M.fromList
              [ ("string", JSONStr "some string"),
                ("emptyString", JSONStr ""),
                ("emptyObject", JSONObj M.empty),
                ("nullValue", JSONNull),
                ("simpleNum", JSONNum 3.6),
                ("negNum", JSONNum (-56)),
                ("numList", JSONList [JSONNum 1.0, JSONNum 2.0]),
                ("mixedList", JSONList [JSONStr "hello", JSONBool True, JSONNum 5]),
                ("deepNest", JSONObj $ M.fromList [("level2", JSONObj $ M.fromList [("level3", JSONStr "deep"), ("level3a", JSONNum 2.0)]), ("level2a", JSONNum 1.0)]),
                ("complex", JSONObj $ M.fromList [("num", JSONNum 3.14), ("obj", JSONObj M.empty)])
              ]
          )
    ]
