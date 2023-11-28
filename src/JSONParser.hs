module JSONParser where

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
parseJSONFile = undefined

-- The parser to get the JSON datatype
jsonP :: Parser JSON
jsonP =
  (JSONNum <$> numP)
    <|> (JSONStr <$> strP)
    <|> (JSONBool <$> boolP)
    <|> (JSONNull <$ nullP)
    <|> (JSONObj <$> objP)
    <|> (JSONList <$> listP)

-- Parser for getting a floating point value for JSONNum
numP :: Parser Float
numP = undefined

-- Parser for getting a string value for JSONStr
strP :: Parser String
strP = undefined

-- Parser for getting a boolean value for JSONBool
boolP :: Parser Bool
boolP = undefined

-- | Parser for getting a null value for JSONNull
nullP :: Parser String
nullP = P.string "null"

-- | Parser for getting a mapping from String to JSON
objP :: Parser (Map String JSON)
objP = Map.fromList <$> P.braces (P.sepBy kvPairP $ P.char ',')

-- | Parser for getting a key value pair
kvPairP :: Parser (String, JSON)
kvPairP = undefined

-- | Parser for getting a list of JSON
listP :: Parser [JSON]
listP = P.brackets (P.sepBy jsonP (P.char ','))
