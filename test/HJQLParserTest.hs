module HJQLParserTest where

import Data.Map qualified as M
import HJQL
import HJQLParser
import JSONObject
import JSONParserTest
import Parser qualified as P
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck

instance (Arbitrary a, Eq a) => Arbitrary (QueryTree a) where
  arbitrary :: Gen (QueryTree a)
  arbitrary = sized gen
    where
      strNoQuoteGen :: Gen String
      strNoQuoteGen = suchThat arbitrary (notElem '"')
      gen :: Int -> Gen (QueryTree a)
      gen n =
        frequency
          [ (1, QueryLeaf <$> strNoQuoteGen <*> arbitrary),
            ( n,
              oneof
                [ QueryTwig <$> strNoQuoteGen <*> scale (`div` 2) arbitrary,
                  QueryBranch <$> suchThat (scale (`div` 2) arbitrary) (not . null),
                  QueryList
                    <$> strNoQuoteGen
                    <*> suchThat
                      (arbitrary :: Gen JSONObj)
                      (all (notElem '"') . M.keys)
                    <*> scale (`div` 2) arbitrary
                ]
            )
          ]

  shrink :: (Arbitrary a) => QueryTree a -> [QueryTree a]
  shrink q = filter (q /=) (shrinkHelper q)
    where
      shrinkHelper (QueryLeaf k a) = [QueryLeaf k' a' | k' <- shrink k, a' <- shrink a]
      shrinkHelper (QueryTwig k t) = t : [QueryTwig k' t' | k' <- shrink k, t' <- shrink t]
      shrinkHelper (QueryList k m t) =
        t : [QueryList k' m' t' | k' <- shrink k, m' <- shrink m, t' <- shrink t]
      shrinkHelper (QueryBranch ts) = map QueryBranch $ filter (not . null) $ shrink ts

instance Arbitrary Query where
  arbitrary :: Gen Query
  arbitrary =
    oneof
      [ Read <$> arbitrary,
        Write <$> arbitrary,
        Delete <$> arbitrary
      ]

  shrink :: Query -> [Query]
  shrink (Read t) = map Read (shrink t)
  shrink (Write t) = map Write (shrink t)
  shrink (Delete t) = map Delete (shrink t)

prop_roundtripHJQL :: Query -> Property
prop_roundtripHJQL q =
  let p = P.doParse hjqlP (showQuery q)
   in case p of
        Just (parsed, "") ->
          counterexample
            (show parsed ++ show (reduceQuery parsed) ++ show (reduceQuery q))
            (reduceQuery parsed == reduceQuery q)
        _ -> counterexample (show p) False
  where
    -- Removes unnecessary QueryBranch
    reduce :: QueryTree a -> QueryTree a
    reduce (QueryTwig k t) = QueryTwig k (reduce t)
    reduce (QueryList k m t) = QueryList k m (reduce t)
    reduce (QueryBranch l@(t : ts)) =
      if null ts then reduce t else QueryBranch (reduceNestedBranch indivReduced)
      where
        indivReduced = map reduce l
        reduceNestedBranch :: [QueryTree a] -> [QueryTree a]
        reduceNestedBranch [] = []
        reduceNestedBranch (QueryBranch b : ts) = b ++ reduceNestedBranch ts
        reduceNestedBranch (t : ts) = t : reduceNestedBranch ts
    reduce t = t
    reduceQuery :: Query -> Query
    reduceQuery (Write t) = Write (reduce t)
    reduceQuery (Read t) = Read (reduce t)
    reduceQuery (Delete t) = Delete (reduce t)

runHJQLParser :: String -> Either P.ParseError Query
runHJQLParser = P.parse hjqlP

test_parseHJQLWPair :: Test
test_parseHJQLWPair =
  "parse write instructions" ~:
    TestList
      [ "empty" ~:
          runHJQLParser
            "write {}"
            ~?= Right (Write (QueryBranch [])),
        "simple" ~:
          runHJQLParser
            " write {\n\
            \    \"key\": \"value\",\n\
            \}\
            \"
            ~?= Right (Write $ QueryBranch [QueryLeaf "key" (JSONStr "value")]),
        "nested" ~:
          runHJQLParser
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
        "multiple, not nested" ~:
          runHJQLParser
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
        "multiple, nested, fancy JSON" ~:
          runHJQLParser
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
              ),
        "simple list" ~:
          runHJQLParser
            "write {\n\
            \    \"key\" [\n\
            \        \"field\": true\n\
            \    ]\n\
            \}"
            ~?= Right
              ( Write $
                  QueryBranch
                    [ QueryList
                        "key"
                        M.empty
                        ( QueryBranch [QueryLeaf "field" $ JSONBool True]
                        )
                    ]
              ),
        "nested list/object" ~:
          runHJQLParser
            "write {\n\
            \    \"key\" [\n\
            \        \"field\": []\n,\
            \        \"data\" [\n\
            \            \"name\": \"Jacob\",\n\
            \            \"boss\" {\n\
            \                \"id\": 2\n\
            \            }\n\
            \        ]\n\
            \    ]\n\
            \}"
            ~?= Right
              ( Write $
                  QueryBranch
                    [ QueryList "key" M.empty $
                        QueryBranch
                          [ QueryLeaf "field" $ JSONList [],
                            QueryList "data" M.empty $
                              QueryBranch
                                [ QueryLeaf "name" $ JSONStr "Jacob",
                                  QueryTwig "boss" $
                                    QueryBranch [QueryLeaf "id" $ JSONNum 2]
                                ]
                          ]
                    ]
              ),
        "list filters" ~:
          runHJQLParser
            "write {\n\
            \    \"key\" [\n\
            \        \"field\": null\n,\
            \        \"data\" |\
            \ \"id\"  ==   300    && \"boss\" ==\
            \{\"id\": 100, \"name\": \"Boris\"} [\n\
            \            \"name\": \"Edward\",\n\
            \            \"boss\" {\n\
            \                \"id\": 552\n\
            \            }\n\
            \        ]\n\
            \    ]\n\
            \}"
            ~?= Right
              ( Write $
                  QueryBranch
                    [ QueryList "key" M.empty $
                        QueryBranch
                          [ QueryLeaf "field" JSONNull,
                            QueryList
                              "data"
                              ( M.fromList
                                  [ ("id", JSONNum 300),
                                    ( "boss",
                                      JSONObj $
                                        M.fromList
                                          [ ("id", JSONNum 100),
                                            ("name", JSONStr "Boris")
                                          ]
                                    )
                                  ]
                              )
                              ( QueryBranch
                                  [ QueryLeaf "name" $ JSONStr "Edward",
                                    QueryTwig "boss" $
                                      QueryBranch
                                        [ QueryLeaf "id" $
                                            JSONNum 552
                                        ]
                                  ]
                              )
                          ]
                    ]
              )
      ]

test_parseHJQLNoPair :: Test
test_parseHJQLNoPair =
  "parse read/delete instructions" ~:
    TestList
      [ "empty" ~:
          runHJQLParser
            "read {}"
            ~?= Right (Read (QueryBranch [])),
        "simple" ~:
          runHJQLParser
            " delete {\n\
            \    \"key\",\n\
            \}\
            \"
            ~?= Right (Delete $ QueryBranch [QueryLeaf "key" ()]),
        "nested" ~:
          runHJQLParser
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
        "multiple, not nested" ~:
          runHJQLParser
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
        "multiple, nested" ~:
          runHJQLParser
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
              ),
        "simple list" ~:
          runHJQLParser
            "delete {\n\
            \    \"key\" [\n\
            \        \"field\"\n\
            \    ]\n\
            \}"
            ~?= Right
              ( Delete $
                  QueryBranch
                    [ QueryList
                        "key"
                        M.empty
                        ( QueryBranch [QueryLeaf "field" ()]
                        )
                    ]
              ),
        "nested list/object" ~:
          runHJQLParser
            "read {\n\
            \    \"key\" [\n\
            \        \"field\"\n,\
            \        \"data\" [\n\
            \            \"name\",\n\
            \            \"boss\" {\n\
            \                \"id\"\n\
            \            }\n\
            \        ]\n\
            \    ]\n\
            \}"
            ~?= Right
              ( Read $
                  QueryBranch
                    [ QueryList "key" M.empty $
                        QueryBranch
                          [ QueryLeaf "field" (),
                            QueryList "data" M.empty $
                              QueryBranch
                                [ QueryLeaf "name" (),
                                  QueryTwig "boss" $
                                    QueryBranch [QueryLeaf "id" ()]
                                ]
                          ]
                    ]
              ),
        "list filters" ~:
          runHJQLParser
            "read {\n\
            \    \"key\" [\n\
            \        \"field\"\n,\
            \        \"data\" |\
            \ \"id\"  ==   300    && \"boss\" ==\
            \{\"id\": 100, \"name\": \"Boris\"} [\n\
            \            \"name\",\n\
            \            \"boss\" {\n\
            \                \"id\"\n\
            \            }\n\
            \        ]\n\
            \    ]\n\
            \}"
            ~?= Right
              ( Read $
                  QueryBranch
                    [ QueryList "key" M.empty $
                        QueryBranch
                          [ QueryLeaf "field" (),
                            QueryList
                              "data"
                              ( M.fromList
                                  [ ("id", JSONNum 300),
                                    ( "boss",
                                      JSONObj $
                                        M.fromList
                                          [ ("id", JSONNum 100),
                                            ("name", JSONStr "Boris")
                                          ]
                                    )
                                  ]
                              )
                              ( QueryBranch
                                  [ QueryLeaf "name" (),
                                    QueryTwig "boss" $
                                      QueryBranch [QueryLeaf "id" ()]
                                  ]
                              )
                          ]
                    ]
              )
      ]

-- >>> runTestTT test_parseHJQLWPair
-- Counts {cases = 8, tried = 8, errors = 0, failures = 0}

-- >>> runTestTT test_parseHJQLNoPair
-- Counts {cases = 8, tried = 8, errors = 0, failures = 0}
