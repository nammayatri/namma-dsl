module NammaDSL.DSL.Parser.Storage.KVConstraints (checkKVConstraints) where

import Control.Exception (throwIO)
import Data.Either.Validation
import Data.List (intercalate)
import Data.Traversable (for)
import NammaDSL.DSL.Syntax.Common
import qualified NammaDSL.DSL.Syntax.Storage as Storage
import Prelude

putStrLn' :: String -> String -> IO ()
putStrLn' colorCode text = putStrLn $ "\x1b[" ++ colorCode ++ "m" ++ text ++ "\x1b[0m"

checkKVConstraints :: [Storage.TableDef] -> IO ()
checkKVConstraints tableDefs = do
  case for tableDefs validateKVConstraints of
    Failure info -> do
      throwIO $
        KVConstraintError $
          "Neither primary nor secondary keys were found in where clause:\n"
            <> intercalate ";\n" (show <$> info)
            <> ".\nGeneration failed"
    Success info -> do
      -- TODO Just for debug purpose, remove later
      putStrLn' "32" $
        "KV constraint applied successfully:\n"
          <> intercalate ";\n" (show <$> info)
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
  Storage.Leaf (_queryParam, Just Storage.Or) -> []
  Storage.Leaf (queryParam, _) -> [queryParam.qpName]
  Storage.Query (Storage.Or, _whereList) -> []
  Storage.Query (_op, whereList) -> concatMap getColumnsFromWhereClause whereList

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
