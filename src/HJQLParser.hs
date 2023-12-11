module HJQLParser where

import Control.Applicative
import Data.Char qualified as Char
import Data.Map (Map)
import Data.Map qualified as Map
import HJQL
import JSONObject
import JSONParser qualified as JP
import Parser (Parser)
import Parser qualified as P

-- | Parse a HJQL file
parserHJQLFile :: String -> IO (Either String [Query])
parserHJQLFile = P.parseFromFile hjqlListP

-- | Parses a list of instructions
hjqlListP :: Parser [Query]
hjqlListP = P.sepByHanging hjqlP $ some $ P.char '\n'

hjqlP :: Parser Query
hjqlP =
  P.wsP $
    Write <$ P.string "write" <* many P.space <*> queryTreeWValueP
      <|> Read <$ P.string "read" <* many P.space <*> queryTreeKeyOnlyP
      <|> Delete <$ P.string "delete" <* many P.space <*> queryTreeKeyOnlyP

-- | Parser for query path for read and delete queries
queryTreeKeyOnlyP :: Parser (QueryTree ())
queryTreeKeyOnlyP =
  QueryBranch
    <$> P.braces (P.wsP $ P.sepByHanging queryTreeKeyOnlyNoBranchP $ P.char ',')

queryTreeKeyOnlyNoBranchP :: Parser (QueryTree ())
queryTreeKeyOnlyNoBranchP =
  QueryTwig <$> P.wsP JP.strP <*> queryTreeKeyOnlyP <* many P.space
    <|> QueryLeaf <$> P.wsP JP.strP <*> pure ()

-- | Parser for query path for write queries
queryTreeWValueP :: Parser (QueryTree JSON)
queryTreeWValueP =
  QueryBranch
    <$> P.braces (P.wsP $ P.sepByHanging queryTreeWValueNoBranchP $ P.char ',')

queryTreeWValueNoBranchP :: Parser (QueryTree JSON)
queryTreeWValueNoBranchP =
  QueryTwig <$> P.wsP JP.strP <*> queryTreeWValueP <* many P.space
    <|> QueryLeaf <$> P.wsP JP.strP <* P.char ':' <*> JP.jsonP
