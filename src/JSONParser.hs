module JSONParser (parseJSONFile, jsonP) where

-- imports
import Control.Applicative
import Data.Char qualified as Char
import Data.Map (Map)
import Data.Map qualified as Map
import JSONObject
import Parser (Parser)
import Parser qualified as P

-- data JSON
--   = JSONNum Float
--   | JSONStr String
--   | JSONObj (Map String JSON)
--   | JSONBool Bool
--   | JSONNull
--   | JSONList [JSON]

-- parsing the file as a string into a JSON Datatype
parseJSONFile :: String -> IO (Either P.ParseError JSON)
parseJSONFile = P.parseFromFile jsonP

-- The parser to get the JSON datatype
jsonP :: Parser JSON
jsonP =
  P.wsP $
    (JSONNum <$> P.numP)
      <|> (JSONStr <$> P.strP)
      <|> (JSONBool <$> P.boolP)
      <|> (JSONNull <$ nullP)
      <|> (JSONObj <$> objP)
      <|> (JSONList <$> listP)

-- | Parser for getting a null value for JSONNull
nullP :: Parser String
nullP = P.string "null"

-- | Parser for getting a mapping from String to JSON
objP :: Parser JSONObj
objP = Map.fromList <$> P.braces (P.wsP $ P.sepByHanging kvPairP $ P.char ',')

-- | Parser for getting a key value pair
kvPairP :: Parser (String, JSON)
kvPairP = (,) <$> P.wsP P.strP <* P.char ':' <*> jsonP

-- | Parser for getting a list of JSON
listP :: Parser [JSON]
listP = P.brackets $ P.wsP $ P.sepByHanging jsonP $ P.char ','
