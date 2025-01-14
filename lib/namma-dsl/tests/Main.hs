{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

--import NammaDSL.DSL.Parser.Storage (SQL_MANIPULATION, sqlCleanedLineParser,  storageParser)
-- Temporary import. Will be deleted

import Data.Default -- Temporary import. Will be deleted
import NammaDSL.App
import NammaDSL.DSL.Parser.Storage (sqlCleanedLineParser, storageParser)
import NammaDSL.DSL.Syntax.Storage --(queries, whereClause, qpName, QueryParam) -- Temporary import. Will be deleted
--import Data.List (isSubsequenceOf, sort) -- Temporary import. Will be deleted
import Prelude

storageYamlFilePath :: FilePath
storageYamlFilePath = "./tests/storage.yaml"

apiYamlFilePath :: FilePath
apiYamlFilePath = "./tests/api.yaml"

dashboardApiYamlFilePath :: FilePath
dashboardApiYamlFilePath = "./tests/dashboard-api.yaml"

generateAllExample :: IO ()
generateAllExample = do
  runStorageGenerator "./tests/dsl-config.dhall" storageYamlFilePath
  runApiGenerator "./tests/dsl-config.dhall" apiYamlFilePath
  runApiGenerator "./tests/dsl-config.dhall" dashboardApiYamlFilePath

sql :: String -> SQL_MANIPULATION -- Just for quick testing
sql = sqlCleanedLineParser

main :: IO ()
main = do
  -- pure () -- generateAllExample
  content <- storageParser def "./tests/storageTest.yaml"
  mapM_ processingWhereClause content
  putStrLn "///////////////////////////////"

-- Temporary functions. Will be deleted
-- TableDef fields :: [FieldDef], ->  beamFields :: [BeamField], ->  bConstraints :: [FieldConstraint],

processingWhereClause :: TableDef -> IO ()
processingWhereClause tableDef = do
  let whereClauseLs = whereClause <$> queries tableDef
  --     primaryKeyLs = primaryKey tableDef
  --     secondaryKeyLs = secondaryKey tableDef
  --     caseWhereClause whereClause' =
  --       case whereClause' of
  --         EmptyWhere -> putStrLn "Going on"
  --         Leaf (queryParam, _) -> processingWhereClauseLeaf queryParam primaryKeyLs secondaryKeyLs
  --         _ -> putStrLn "Another result"
  -- mapM_ caseWhereClause whereClauseLs
  print $ map getColumnsFromWhereClause whereClauseLs

processingWhereClauseLeaf :: QueryParam -> [String] -> [String] -> IO ()
processingWhereClauseLeaf queryParam primaryKeyLs secondaryKeyLs = do
  let fildName = qpName queryParam
  if fildName `elem` secondaryKeyLs || fildName `elem` primaryKeyLs
    then putStrLn "Going on"
    else putStrLn "Huston, we have a problem"

getColumnsFromWhereClause :: WhereClause -> [String]
getColumnsFromWhereClause where_ =
  case where_ of
    EmptyWhere -> []
    Leaf (queryParam, _) -> [qpName queryParam]
    Query (operator, whereLs_) ->
      case operator of
        Or -> []
        _ -> concatMap getColumnsFromWhereClause whereLs_

-- data WhereClause = EmptyWhere | Leaf (QueryParam, Maybe Operator) | Query (Operator, [WhereClause]) deriving (Show)

-- data Operator = And | Or | In | Eq | GreaterThan | LessThan | GreaterThanOrEq | LessThanOrEq | Not Operator deriving (Show, Eq)
