module NammaDSL.DSL.Parser.Storage.KVRestrictions (checkKVRestrictions) where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.Either.Validation
import Data.Functor ((<&>))
import Data.List (find, intercalate)
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import NammaDSL.DSL.Syntax.Common
import qualified NammaDSL.DSL.Syntax.Storage as Storage
import NammaDSL.Utils
import Prelude

checkKVRestrictions :: FilePath -> [Storage.TableDef] -> IO ()
checkKVRestrictions yamlPath tableDefs = do
  case for tableDefs validateKVRestrictions of
    Failure (info :: [KVRestrictionInfo]) -> do
      putStrLnRed $
        "KV Restriction failed: "
          <> yamlPath
          <> "\nAll primary keys or at least one secondary key should be found in non empty where clause:\n"
          <> intercalate ";\n" (("  " <>) . show <$> info)
          <> ".\nGeneration failed"
      throwIO $ KVRestrictionError
    Success (info :: [[KVRestrictionInfo]]) -> do
      -- TODO Just for debug purpose, remove later
      putStrLnGreen $
        "KV Restriction applied successfully:\n"
          <> intercalate ";\n" (("  " <>) . show <$> concat info)
      let skippedTables = map (.tableNameHaskell) $ filter (.disableKVQueryRestrictions) tableDefs
      unless (null skippedTables) $
        putStrLnYellow $
          "WARNING: skipped KV Restriction for following tables: " <> intercalate ", " skippedTables
      pure ()

data KVRestrictionInfo = KVRestrictionInfo
  { tableName :: String,
    queryName :: String,
    whereColumns :: [String],
    primaryKeys :: [String],
    secondaryKeys :: [String]
  }
  deriving (Show)

validateKVRestrictions :: Storage.TableDef -> Validation [KVRestrictionInfo] [KVRestrictionInfo]
validateKVRestrictions tableDef = do
  if tableDef.disableKVQueryRestrictions
    then Success []
    else do
      let filteredQueries = filter (\query -> query.kvFunction `notElem` excludedDbFunctions) tableDef.queries
      for filteredQueries $ \query -> do
        let whereColumns = getBeamColumnsFromWhereClause tableDef.fields query.whereClause
        let info =
              KVRestrictionInfo
                { tableName = tableDef.tableNameHaskell,
                  queryName = query.queryName,
                  whereColumns,
                  primaryKeys = tableDef.primaryKey,
                  secondaryKeys = tableDef.secondaryKey
                }
        if (kvQueryRestrictions tableDef (isEmptyWhere query.whereClause) whereColumns) then Success info else Failure [info]

kvQueryRestrictions :: Storage.TableDef -> Bool -> [String] -> Bool
kvQueryRestrictions table isEmpty whereColumns = do
  isEmpty
    || all (`elem` whereColumns) table.primaryKey
    || any (`elem` whereColumns) table.secondaryKey

isEmptyWhere :: Storage.WhereClause -> Bool
isEmptyWhere Storage.EmptyWhere = True
isEmptyWhere _ = False

getBeamColumnsFromWhereClause :: [Storage.FieldDef] -> Storage.WhereClause -> [String]
getBeamColumnsFromWhereClause fields whereClause = do
  let haskellFields = getColumnsFromWhereClause whereClause
  flip concatMap haskellFields $ \haskellField -> do
    (<&> (.bFieldName)) . fromMaybe [] . (<&> (.beamFields)) . find (\field -> field.fieldName == haskellField) $ fields

getColumnsFromWhereClause :: Storage.WhereClause -> [String]
getColumnsFromWhereClause = \case
  Storage.EmptyWhere -> []
  Storage.Leaf (queryParam, Just Storage.And) -> [queryParam.qpName]
  Storage.Leaf (queryParam, Just Storage.Eq) -> [queryParam.qpName]
  Storage.Leaf (queryParam, Nothing) -> [queryParam.qpName]
  Storage.Leaf (_queryParam, Just _) -> []
  Storage.Query (Storage.And, whereList) -> concatMap getColumnsFromWhereClause whereList
  Storage.Query (Storage.Eq, whereList) -> concatMap getColumnsFromWhereClause whereList
  Storage.Query (_, _whereList) -> []

excludedDbFunctions :: [String]
excludedDbFunctions =
  [ "findOneWithDb",
    "findAllWithDb",
    "findAllWithOptionsDb",
    "updateWithDb",
    "deleteWithDb"
  ]
