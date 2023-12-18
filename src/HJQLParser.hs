module HJQLParser
  ( parseHJQLFile,
    hjqlListP,
    hjqlP,
  )
where

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

-- | Parser for filters on list queries
queryListFilterP :: Parser JSONObj
queryListFilterP =
  M.fromList <$> P.wsP (P.sepBy1 eqConstraintP (P.string "&&"))
  where
    eqConstraintP :: Parser (Key, JSON)
    eqConstraintP = (,) <$> P.wsP P.strP <* P.string "==" <*> JP.jsonP

-- | Parser for filters on list queries that defaults to empty if none found
queryListFilterOrNoneP :: Parser JSONObj
queryListFilterOrNoneP = P.char '|' *> queryListFilterP <|> pure M.empty

-- | Parser for list queries that applies the given parser to the inner elements
queryListP :: Parser (QueryTree a) -> Parser (QueryTree a)
queryListP treeP =
  QueryList
    <$> P.wsP P.strP
    <*> queryListFilterOrNoneP
    <*> P.brackets treeP
    <* many P.space

-- | Parser for query twigs that applies the given parser to the inner elements
queryTwigP :: Parser (QueryTree a) -> Parser (QueryTree a)
queryTwigP treeP =
  QueryTwig <$> P.wsP P.strP <*> P.braces treeP <* many P.space

-- | Parser for query path for read and delete queries
queryTreeKeyOnlyP :: Parser (QueryTree ())
queryTreeKeyOnlyP =
  QueryBranch <$> P.wsP (P.sepByHanging queryTreeKeyOnlyNoBranchP (P.char ','))

queryTreeKeyOnlyNoBranchP :: Parser (QueryTree ())
queryTreeKeyOnlyNoBranchP =
  queryListP queryTreeKeyOnlyP
    <|> queryTwigP queryTreeKeyOnlyP
    <|> QueryLeaf <$> P.wsP P.strP <*> pure ()

-- | Parser for query path for write queries
queryTreeWValueP :: Parser (QueryTree JSON)
queryTreeWValueP =
  QueryBranch <$> P.wsP (P.sepByHanging queryTreeWValueNoBranchP (P.char ','))

queryTreeWValueNoBranchP :: Parser (QueryTree JSON)
queryTreeWValueNoBranchP =
  queryListP queryTreeWValueP
    <|> queryTwigP queryTreeWValueP
    <|> QueryLeaf <$> P.wsP P.strP <* P.char ':' <*> JP.jsonP
