module HJQLTest where

import Data.Map qualified as M
import HJQL
import HJQLParser
import JSONObject
import Parser qualified as P
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))

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

jsonObj2 :: JSONObj
jsonObj2 =
  M.fromList
    [ ( "students",
        JSONList
          [ JSONStr "random string",
            JSONNum 3.14,
            JSONBool False,
            JSONNull,
            JSONList [JSONStr "hello", JSONBool True, JSONNum 5],
            JSONObj $ M.fromList [("name", JSONStr "Jack"), ("major", JSONStr "CS"), ("gpa", JSONNum 3.5), ("classes", JSONObj $ M.fromList [("cis110", JSONNum 91), ("cis120", JSONNum 97), ("math201", JSONNum 93), ("math202", JSONNum 95)])],
            JSONObj $ M.fromList [("name", JSONStr "Emma"), ("major", JSONStr "Math"), ("gpa", JSONNum 3.8), ("classes", JSONObj $ M.fromList [("cis110", JSONNum 95), ("math201", JSONNum 89), ("math202", JSONNum 92)])],
            JSONObj $ M.fromList [("name", JSONStr "Charlie"), ("major", JSONStr "Physics"), ("gpa", JSONNum 3.6), ("classes", JSONObj $ M.fromList [("phy101", JSONNum 92), ("phy202", JSONNum 87), ("cis120", JSONNum 95)])],
            JSONObj $ M.fromList [("name", JSONStr "Olivia"), ("major", JSONStr "CS"), ("gpa", JSONNum 3.9), ("classes", JSONObj $ M.fromList [("cis110", JSONNum 100), ("cis120", JSONNum 100), ("cis121", JSONNum 100), ("cis160", JSONNum 100)])],
            JSONObj $ M.fromList [("name", JSONStr "Noah"), ("major", JSONStr "Math"), ("gpa", JSONNum 3.7), ("classes", JSONObj $ M.fromList [("math201", JSONNum 95), ("math202", JSONNum 93), ("math203", JSONNum 91)])]
          ]
      ),
      ("obj", JSONObj $ M.fromList [("key1", JSONStr "value1"), ("key2", JSONStr "value2"), ("key3", JSONStr "value3")]),
      ( "outer",
        JSONList
          [ JSONObj $ M.fromList [("middle", JSONList [JSONObj $ M.fromList [("inner", JSONList [JSONObj $ M.fromList [("name", JSONStr "Lambert"), ("occupation", JSONStr "Sheep")]])]])],
            JSONObj $ M.fromList [("middle", JSONList [JSONObj $ M.fromList [("inner", JSONList [JSONObj $ M.fromList [("name", JSONStr "James"), ("occupation", JSONStr "Cow")]])]])]
          ]
      )
    ]

-- Read tests for jsonObj2
queryRead11 :: Query
queryRead11 = Read (QueryLeaf "students" ())

queryRead12 :: Query
queryRead12 = Read (QueryList "students" M.empty (QueryLeaf "name" ()))

queryRead13 :: Query
queryRead13 = Read (QueryList "students" (M.fromList [("name", JSONStr "Jack")]) (QueryLeaf "name" ()))

queryRead14 :: Query
queryRead14 = Read (QueryList "students" M.empty (QueryTwig "classes" (QueryLeaf "cis110" ())))

queryRead15 :: Query
queryRead15 = Read (QueryList "students" (M.fromList [("classes", JSONObj (M.fromList [("cis110", JSONNum 100)]))]) (QueryTwig "classes" (QueryLeaf "cis110" ())))

queryRead16 :: Query
queryRead16 = Read (QueryList "students" M.empty (QueryBranch [QueryLeaf "name" (), QueryLeaf "major" (), QueryLeaf "classes" ()]))

queryRead17 :: Query
queryRead17 = Read (QueryList "students" M.empty (QueryBranch [QueryLeaf "name" (), QueryTwig "classes" (QueryBranch [QueryLeaf "cis110" (), QueryLeaf "cis120" ()])]))

queryRead18 :: Query
queryRead18 = Read (QueryList "students" (M.fromList [("classes", JSONObj $ M.fromList [("cis110", JSONNum 100)])]) (QueryBranch [QueryLeaf "name" (), QueryLeaf "major" (), QueryLeaf "gpa" (), QueryTwig "classes" (QueryLeaf "cis110" ())]))

queryRead19 :: Query
queryRead19 = Read (QueryBranch [QueryLeaf "obj" (), QueryList "students" (M.fromList [("classes", JSONObj $ M.fromList [("cis110", JSONNum 100)])]) (QueryBranch [QueryLeaf "name" (), QueryLeaf "major" (), QueryLeaf "gpa" (), QueryTwig "classes" (QueryLeaf "cis110" ())])])

queryRead20 :: Query
queryRead20 = Read (QueryList "students" M.empty (QueryBranch []))

queryRead21 :: Query
queryRead21 =
  Read
    ( QueryList
        "outer"
        (M.fromList [("middle", JSONList [JSONObj $ M.fromList [("inner", JSONList [JSONObj $ M.fromList [("name", JSONStr "Lambert"), ("occupation", JSONStr "Sheep")]])]])])
        ( QueryList
            "middle"
            M.empty
            (QueryList "inner" M.empty (QueryBranch [QueryLeaf "name" (), QueryLeaf "occupation" ()]))
        )
    )

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
      runQuery queryRead10 jsonObj1 ~?= Right M.empty,
      runQuery queryRead11 jsonObj2
        ~?= Right
          ( M.fromList
              [ ( "students",
                  JSONList
                    [ JSONStr "random string",
                      JSONNum 3.14,
                      JSONBool False,
                      JSONNull,
                      JSONList [JSONStr "hello", JSONBool True, JSONNum 5],
                      JSONObj $ M.fromList [("name", JSONStr "Jack"), ("major", JSONStr "CS"), ("gpa", JSONNum 3.5), ("classes", JSONObj $ M.fromList [("cis110", JSONNum 91), ("cis120", JSONNum 97), ("math201", JSONNum 93), ("math202", JSONNum 95)])],
                      JSONObj $ M.fromList [("name", JSONStr "Emma"), ("major", JSONStr "Math"), ("gpa", JSONNum 3.8), ("classes", JSONObj $ M.fromList [("cis110", JSONNum 95), ("math201", JSONNum 89), ("math202", JSONNum 92)])],
                      JSONObj $ M.fromList [("name", JSONStr "Charlie"), ("major", JSONStr "Physics"), ("gpa", JSONNum 3.6), ("classes", JSONObj $ M.fromList [("phy101", JSONNum 92), ("phy202", JSONNum 87), ("cis120", JSONNum 95)])],
                      JSONObj $ M.fromList [("name", JSONStr "Olivia"), ("major", JSONStr "CS"), ("gpa", JSONNum 3.9), ("classes", JSONObj $ M.fromList [("cis110", JSONNum 100), ("cis120", JSONNum 100), ("cis121", JSONNum 100), ("cis160", JSONNum 100)])],
                      JSONObj $ M.fromList [("name", JSONStr "Noah"), ("major", JSONStr "Math"), ("gpa", JSONNum 3.7), ("classes", JSONObj $ M.fromList [("math201", JSONNum 95), ("math202", JSONNum 93), ("math203", JSONNum 91)])]
                    ]
                )
              ]
          ),
      runQuery queryRead12 jsonObj2
        ~?= Right
          ( M.fromList
              [ ( "students",
                  JSONList
                    [ JSONObj $ M.fromList [("name", JSONStr "Jack")],
                      JSONObj $ M.fromList [("name", JSONStr "Emma")],
                      JSONObj $ M.fromList [("name", JSONStr "Charlie")],
                      JSONObj $ M.fromList [("name", JSONStr "Olivia")],
                      JSONObj $ M.fromList [("name", JSONStr "Noah")]
                    ]
                )
              ]
          ),
      runQuery queryRead13 jsonObj2
        ~?= Right
          ( M.fromList
              [ ( "students",
                  JSONList [JSONObj $ M.fromList [("name", JSONStr "Jack")]]
                )
              ]
          ),
      runQuery queryRead14 jsonObj2
        ~?= Right
          ( M.fromList
              [ ( "students",
                  JSONList
                    [ JSONObj $ M.fromList [("classes", JSONObj $ M.fromList [("cis110", JSONNum 91)])],
                      JSONObj $ M.fromList [("classes", JSONObj $ M.fromList [("cis110", JSONNum 95)])],
                      JSONObj $ M.fromList [("classes", JSONObj $ M.fromList [("cis110", JSONNum 100)])]
                    ]
                )
              ]
          ),
      runQuery queryRead15 jsonObj2
        ~?= Right
          ( M.fromList
              [ ( "students",
                  JSONList [JSONObj $ M.fromList [("classes", JSONObj $ M.fromList [("cis110", JSONNum 100)])]]
                )
              ]
          ),
      runQuery queryRead16 jsonObj2
        ~?= Right
          ( M.fromList
              [ ( "students",
                  JSONList
                    [ JSONObj $ M.fromList [("name", JSONStr "Jack"), ("major", JSONStr "CS"), ("classes", JSONObj $ M.fromList [("cis110", JSONNum 91), ("cis120", JSONNum 97), ("math201", JSONNum 93), ("math202", JSONNum 95)])],
                      JSONObj $ M.fromList [("name", JSONStr "Emma"), ("major", JSONStr "Math"), ("classes", JSONObj $ M.fromList [("cis110", JSONNum 95), ("math201", JSONNum 89), ("math202", JSONNum 92)])],
                      JSONObj $ M.fromList [("name", JSONStr "Charlie"), ("major", JSONStr "Physics"), ("classes", JSONObj $ M.fromList [("phy101", JSONNum 92), ("phy202", JSONNum 87), ("cis120", JSONNum 95)])],
                      JSONObj $ M.fromList [("name", JSONStr "Olivia"), ("major", JSONStr "CS"), ("classes", JSONObj $ M.fromList [("cis110", JSONNum 100), ("cis120", JSONNum 100), ("cis121", JSONNum 100), ("cis160", JSONNum 100)])],
                      JSONObj $ M.fromList [("name", JSONStr "Noah"), ("major", JSONStr "Math"), ("classes", JSONObj $ M.fromList [("math201", JSONNum 95), ("math202", JSONNum 93), ("math203", JSONNum 91)])]
                    ]
                )
              ]
          ),
      runQuery queryRead17 jsonObj2
        ~?= Right
          ( M.fromList
              [ ( "students",
                  JSONList
                    [ JSONObj $ M.fromList [("name", JSONStr "Jack"), ("classes", JSONObj $ M.fromList [("cis110", JSONNum 91), ("cis120", JSONNum 97)])],
                      JSONObj $ M.fromList [("name", JSONStr "Olivia"), ("classes", JSONObj $ M.fromList [("cis110", JSONNum 100), ("cis120", JSONNum 100)])]
                    ]
                )
              ]
          ),
      runQuery queryRead18 jsonObj2
        ~?= Right
          ( M.fromList
              [ ( "students",
                  JSONList
                    [JSONObj $ M.fromList [("name", JSONStr "Olivia"), ("major", JSONStr "CS"), ("gpa", JSONNum 3.9), ("classes", JSONObj $ M.fromList [("cis110", JSONNum 100)])]]
                )
              ]
          ),
      runQuery queryRead19 jsonObj2
        ~?= Right
          ( M.fromList
              [ ("obj", JSONObj $ M.fromList [("key1", JSONStr "value1"), ("key2", JSONStr "value2"), ("key3", JSONStr "value3")]),
                ( "students",
                  JSONList
                    [JSONObj $ M.fromList [("name", JSONStr "Olivia"), ("major", JSONStr "CS"), ("gpa", JSONNum 3.9), ("classes", JSONObj $ M.fromList [("cis110", JSONNum 100)])]]
                )
              ]
          ),
      runQuery queryRead20 jsonObj2
        ~?= Right
          ( M.fromList
              [ ( "students",
                  JSONList [JSONObj M.empty, JSONObj M.empty, JSONObj M.empty, JSONObj M.empty, JSONObj M.empty]
                )
              ]
          ),
      runQuery queryRead21 jsonObj2
        ~?= Right
          ( M.fromList
              [ ( "outer",
                  JSONList
                    [JSONObj $ M.fromList [("middle", JSONList [JSONObj $ M.fromList [("inner", JSONList [JSONObj $ M.fromList [("name", JSONStr "Lambert"), ("occupation", JSONStr "Sheep")]])]])]]
                )
              ]
          )
    ]

-- >>> runTestTT test_read
-- Counts {cases = 23, tried = 23, errors = 0, failures = 0}

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

-- >>> runTestTT test_write
-- Counts {cases = 5, tried = 5, errors = 0, failures = 0}

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

-- >>> runTestTT test_delete
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}
