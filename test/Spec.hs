import HJQLParserTest
import HJQLTest
import JSONParserTest
import Test.HUnit
import Test.QuickCheck (quickCheck)

main :: IO ()
main = do
  putStrLn "Testing JSON Parsing"
  quickCheck prop_roundtripJSON
  runTestTT testJSONParsePrimitives
  runTestTT testJSONParseObject
  runTestTT testJSONParseList
  putStrLn "Testing HJQL Parsing"
  quickCheck prop_roundtripHJQL
  runTestTT test_parseHJQLNoPair
  runTestTT test_parseHJQLWPair
  putStrLn "Testing Running Queries"
  runTestTT test_read
  runTestTT test_write
  runTestTTAndExit test_delete
