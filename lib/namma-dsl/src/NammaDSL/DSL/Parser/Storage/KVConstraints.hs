module NammaDSL.DSL.Parser.Storage.KVConstraints (checkKVConstraints) where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.Either.Validation
import Data.List (intercalate)
import Data.Traversable (for)
import NammaDSL.DSL.Syntax.Common
import qualified NammaDSL.DSL.Syntax.Storage as Storage
import NammaDSL.Utils
import Prelude

checkKVConstraints :: [Storage.TableDef] -> IO ()
checkKVConstraints tableDefs = do
  case for tableDefs validateKVConstraints of
    Failure (info :: [KVConstraintInfo]) -> do
      putStrLnRed $
        "KV constraint failed. All primary keys or at least one secondary key should be found in non empty where clause:\n"
          <> intercalate ";\n" (("  " <>) . show <$> info)
          <> ".\nGeneration failed"
      throwIO $ KVConstraintError
    Success (info :: [[KVConstraintInfo]]) -> do
      -- TODO Just for debug purpose, remove later
      putStrLnGreen $
        "KV constraint applied successfully:\n"
          <> intercalate ";\n" (("  " <>) . show <$> concat info)
      let skippedTables = map (.tableNameHaskell) $ filter (.disableKVQueryConstraints) tableDefs
      unless (null skippedTables) $
        putStrLnYellow $
          "WARNING: skipped KV constraint for following tables: " <> intercalate ", " skippedTables
      pure ()

data KVConstraintInfo = KVConstraintInfo
  { tableName :: String,
    queryName :: String,
    whereColumns :: [String],
    primaryKeys :: [String],
    secondaryKeys :: [String]
  }
  deriving (Show)

validateKVConstraints :: Storage.TableDef -> Validation [KVConstraintInfo] [KVConstraintInfo]
validateKVConstraints tableDef = do
  if tableDef.disableKVQueryConstraints
    then Success []
    else for tableDef.queries $ \query -> do
      -- unless (kvQueryConstraints (concat $ beamFields <$> fields table) (whereClause query)) $ do
      let whereColumns = getColumnsFromWhereClause query.whereClause
      let info =
            KVConstraintInfo
              { tableName = tableDef.tableNameHaskell,
                queryName = query.queryName,
                whereColumns,
                primaryKeys = tableDef.primaryKey,
                secondaryKeys = tableDef.secondaryKey
              }
      if (kvQueryConstraints tableDef (isEmptyWhere query.whereClause) whereColumns) then Success info else Failure [info]

kvQueryConstraints :: Storage.TableDef -> Bool -> [String] -> Bool
kvQueryConstraints table isEmpty whereColumns = do
  isEmpty
    || all (`elem` whereColumns) table.primaryKey
    || any (`elem` whereColumns) table.secondaryKey

isEmptyWhere :: Storage.WhereClause -> Bool
isEmptyWhere Storage.EmptyWhere = True
isEmptyWhere _ = False

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
