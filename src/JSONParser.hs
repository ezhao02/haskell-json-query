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
parseJSONFile = P.parseFromFile jsonP

-- The parser to get the JSON datatype
jsonP :: Parser JSON
jsonP =
  P.wsP $
    (JSONNum <$> numP)
      <|> (JSONStr <$> strP)
      <|> (JSONBool <$> boolP)
      <|> (JSONNull <$ nullP)
      <|> (JSONObj <$> objP)
      <|> (JSONList <$> listP)

-- Parser for getting a floating point value for JSONNum
numP :: Parser Float
numP =
  let nonNegSignificandParser =
        ( ((++) <$> some P.digit <*> ((:) <$> P.char '.' <*> some P.digit))
            <|> some P.digit
        )
      exponentParser =
        (:)
          <$> P.char 'e'
          <*> ( (:) <$> P.char '-' <*> some P.digit
                  <|> some P.digit
              )
      nonNegFloatParser =
        (++) <$> nonNegSignificandParser <*> exponentParser
          <|> nonNegSignificandParser
   in (read :: String -> Float)
        <$> (((:) <$> P.char '-' <*> nonNegFloatParser) <|> nonNegFloatParser)

-- Parser for getting a string value for JSONStr
strP :: Parser String
strP = P.between (P.char '"') (many (P.filter ('"' /=) P.get)) (P.char '"')

-- Parser for getting a boolean value for JSONBool
boolP :: Parser Bool
boolP = (True <$ P.string "true") <|> (False <$ P.string "false")

-- | Parser for getting a null value for JSONNull
nullP :: Parser String
nullP = P.string "null"

-- | Parser for getting a mapping from String to JSON
objP :: Parser JSONObj
objP = Map.fromList <$> P.braces (P.wsP $ P.sepByHanging kvPairP $ P.char ',')

-- | Parser for getting a key value pair
kvPairP :: Parser (String, JSON)
kvPairP = (,) <$> P.wsP strP <* P.char ':' <*> jsonP

-- | Parser for getting a list of JSON
listP :: Parser [JSON]
listP = P.brackets $ P.wsP $ P.sepByHanging jsonP $ P.char ','
