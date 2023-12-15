module HJQLParser where

import Control.Applicative
import Data.Char qualified as Char
import Data.Map (Map)
import Data.Map qualified as M
import HJQL
import JSONObject
import JSONParser qualified as JP
import Parser (Parser)
import Parser qualified as P

-- | Parse a HJQL file
parseHJQLFile :: String -> IO (Either String [Query])
parseHJQLFile = P.parseFromFile hjqlListP

-- | Parses a list of instructions
hjqlListP :: Parser [Query]
hjqlListP = P.sepByHanging hjqlP $ some $ P.char '\n'

hjqlP :: Parser Query
hjqlP =
  many P.space
    *> ( Write <$ P.string "write" <* many P.space <*> P.braces queryTreeWValueP
           <|> Read
             <$ P.string "read"
             <* many P.space
             <*> P.braces queryTreeKeyOnlyP
           <|> Delete
             <$ P.string "delete"
             <* many P.space
             <*> P.braces queryTreeKeyOnlyP
       )
    <* many (P.filter (/= '\n') P.space)

-- | Parser for query path for read and delete queries
queryTreeKeyOnlyP :: Parser (QueryTree ())
queryTreeKeyOnlyP =
  QueryBranch <$> P.wsP (P.sepByHanging queryTreeKeyOnlyNoBranchP $ P.char ',')

queryTreeKeyOnlyNoBranchP :: Parser (QueryTree ())
queryTreeKeyOnlyNoBranchP =
  QueryList <$> P.wsP JP.strP <*> pure M.empty <*> undefined <*> P.brackets queryTreeKeyOnlyP <* many P.space
    <|> undefined -- TODO: QueryList with filters
    <|> QueryTwig <$> P.wsP JP.strP <*> P.braces queryTreeKeyOnlyP <* many P.space
    <|> QueryLeaf <$> P.wsP JP.strP <*> pure ()

-- | Parser for query path for write queries
queryTreeWValueP :: Parser (QueryTree JSON)
queryTreeWValueP =
  QueryBranch <$> P.wsP (P.sepByHanging queryTreeWValueNoBranchP $ P.char ',')

queryTreeWValueNoBranchP :: Parser (QueryTree JSON)
queryTreeWValueNoBranchP =
  QueryList <$> P.wsP JP.strP <*> pure M.empty <*> undefined <*> P.brackets queryTreeWValueP <* many P.space
    <|> undefined -- TODO: QueryList with filters
    <|> QueryTwig <$> P.wsP JP.strP <*> P.braces queryTreeWValueP <* many P.space
    <|> QueryLeaf <$> P.wsP JP.strP <* P.char ':' <*> JP.jsonP
