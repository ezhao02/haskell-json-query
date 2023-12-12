import HJQLParserTest
import HJQLTest
import JSONParserTest
import Test.HUnit
import Test.QuickCheck

main :: IO ()
main = do
  putStrLn "Testing JSON Parsing"
  runTestTT testJSONParsePrimitives
  runTestTT testJSONParseObject
  runTestTT testJSONParseList
  putStrLn "Testing HJQL Parsing"
  runTestTT test_parseHJQLNoPair
  runTestTT test_parseHJQLWPair
  putStrLn "Testing Running Queries"
  runTestTT test_read
  runTestTT test_write
  runTestTTAndExit test_delete
