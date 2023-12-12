module Main where

import Data.List (foldl')
import HJQL
import HJQLParser (parseHJQLFile)
import JSONObject
import JSONParser (parseJSONFile)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [jsonFile, hjqlFile] -> do
      json <- parseJSONFile jsonFile
      hjqlInstructions <- parseHJQLFile hjqlFile
      case (json, hjqlInstructions) of
        (Left e, _) -> do
          putStr "could not parse JSON file: "
          error e
        (_, Left e) -> do
          putStr "could not parse HJQL file: "
          error e
        (Right (JSONObj obj), Right instrs) -> do
          foldl' (evalInstrs jsonFile) (return obj) instrs
          return ()
        _ -> error "JSON file must store a JSON object"
    _ -> error "usage: first argument is JSON, second argument is query"
  where
    evalInstrs :: String -> IO JSONObj -> Query -> IO JSONObj
    evalInstrs jsonFile jsonIO query = do
      json <- jsonIO
      case (runQuery query json, query) of
        (Left e, _) -> error e
        (Right obj, Read _) -> do
          print $ JSONObj obj
          return json
        (Right obj, _) -> do
          -- Other instructions mutate
          print $ JSONObj obj
          writeFile jsonFile $ show $ JSONObj obj
          return obj
