module NammaDSL.DSL.Parser.Storage.KVConstraints where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.Either.Validation
import Data.Foldable (for_)
import Data.List (intercalate)
import NammaDSL.DSL.Syntax.Common
import qualified NammaDSL.DSL.Syntax.Storage as Storage
import Prelude

checkKVConstraints :: [Storage.TableDef] -> IO ()
checkKVConstraints tableDefs = do
  case for_ tableDefs validateKVConstraints of
    Failure info -> do
      throwIO $ KVConstraintError $ "Neither primary nor secondary keys were found in where clause:\n" <> intercalate ";\n" (show <$> info)
    Success () -> pure ()

data KVConstraintInfo = KVConstraintInfo
  { tableName :: String,
    queryName :: String,
    whereColumns :: [String],
    primaryKeys :: [String],
    secondaryKeys :: [String]
  }
  deriving (Show)

validateKVConstraints :: Storage.TableDef -> Validation [KVConstraintInfo] ()
validateKVConstraints tableDef = do
  unless tableDef.disableKVQueryConstraints $ do
    for_ tableDef.queries $ \query -> do
      -- unless (kvQueryConstraints (concat $ beamFields <$> fields table) (whereClause query)) $ do
      let whereColumns = getColumnsFromWhereClause query.whereClause
      unless (kvQueryConstraints tableDef whereColumns) $ do
        let info =
              KVConstraintInfo
                { tableName = tableDef.tableNameHaskell,
                  queryName = query.queryName,
                  whereColumns,
                  primaryKeys = tableDef.primaryKey,
                  secondaryKeys = tableDef.secondaryKey
                }
        Failure [info]

kvQueryConstraints :: Storage.TableDef -> [String] -> Bool
kvQueryConstraints table whereColumns = do
  null whereColumns
    || all (`elem` whereColumns) table.primaryKey
    || any (`elem` whereColumns) table.secondaryKey

getColumnsFromWhereClause :: Storage.WhereClause -> [String]
getColumnsFromWhereClause _where_ = ["foo", "bar"] -- error "TODO"

-- kvQueryPrimaryKeyConstraint :: [String] -> WhereClause -> Bool
-- kvQueryPrimaryKeyConstraint _keys EmptyWhere = True
-- kvQueryPrimaryKeyConstraint _keys (Leaf (_qp, Nothing)) = False -- error "TODO"
-- kvQueryPrimaryKeyConstraint _keys (Leaf (_qp, (Just _op))) = False -- error "TODO"
-- kvQueryPrimaryKeyConstraint _keys (Query (_op, _ws)) = False -- error "TODO"

-- kvQuerySecondaryKeyConstraint :: [String] -> WhereClause -> Bool
-- kvQuerySecondaryKeyConstraint _keys EmptyWhere = True
-- kvQuerySecondaryKeyConstraint _keys _where_ = False -- error "TODO"

-- fetchPrimaryKeys :: [BeamField] -> [String]
-- fetchPrimaryKeys _fields = error "TODO"

-- fetchSecondaryKeys :: [BeamField] -> [String]
-- fetchSecondaryKeys = error "TODO"
