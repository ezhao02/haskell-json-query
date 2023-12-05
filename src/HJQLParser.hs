module HJQLParser where

import Control.Applicative
import Data.Char qualified as Char
import Data.Map (Map)
import Data.Map qualified as Map
import JSONObject
import JSONParser qualified as JP
import Parser (Parser)
import Parser qualified as P

type Instruction = (Int, Either JSON [String])

-- | Parse a HJQL file
parserHJQLFile :: String -> IO (Either String [Instruction])
parserHJQLFile = undefined

-- | types for the different query parser
type DeleteQuery = [String]

type ReadQuery = [String]

type UpdateQuery = JSON

type CreateQuery = JSON

-- | Parse a HJQL query for the create operation
parseCreate :: Parser CreateQuery
parseCreate = undefined

-- | Parse a HJQL query for the read operation
parseRead :: Parser ReadQuery
parseRead = undefined

-- | Parse a HJQL query for the update operation
parseUpdate :: Parser UpdateQuery
parseUpdate = undefined

-- | Parse a HJQL query for the delete operation
parseDelete :: Parser DeleteQuery
parseDelete = undefined
