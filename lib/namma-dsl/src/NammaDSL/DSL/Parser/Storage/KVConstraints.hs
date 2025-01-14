module NammaDSL.DSL.Parser.Storage.KVConstraints where

import Control.Exception (throwIO)
import Control.Monad (forM_, unless, when)
import NammaDSL.DSL.Syntax.Common
import NammaDSL.DSL.Syntax.Storage
import Prelude

checkKVConstraints :: TableDef -> IO ()
checkKVConstraints tableDef = do
  when (disableKVQueryConstraints tableDef) $ do
    forM_ (queries tableDef) $ \query -> do
      -- unless (kvQueryConstraints (concat $ beamFields <$> fields table) (whereClause query)) $ do
      unless (kvQueryConstraints tableDef (whereClause query)) $ do
        throwIO (KVConstraintError $ "Neither primary nor secondary keys were found in where clause: " <> queryName query <> "; table: " <> tableNameHaskell tableDef)

kvQueryConstraints :: TableDef -> WhereClause -> Bool
kvQueryConstraints table where_ =
  kvQueryPrimaryKeyConstraint (primaryKey table) where_
    || kvQuerySecondaryKeyConstraint (secondaryKey table) where_

kvQueryPrimaryKeyConstraint :: [String] -> WhereClause -> Bool
kvQueryPrimaryKeyConstraint _keys EmptyWhere = True
kvQueryPrimaryKeyConstraint _keys (Leaf (_qp, Nothing)) = error "TODO"
kvQueryPrimaryKeyConstraint _keys (Leaf (_qp, (Just _op))) = error "TODO"
kvQueryPrimaryKeyConstraint _keys (Query (_op, _ws)) = error "TODO"

kvQuerySecondaryKeyConstraint :: [String] -> WhereClause -> Bool
kvQuerySecondaryKeyConstraint = error "TODO"

-- fetchPrimaryKeys :: [BeamField] -> [String]
-- fetchPrimaryKeys _fields = error "TODO"

-- fetchSecondaryKeys :: [BeamField] -> [String]
-- fetchSecondaryKeys = error "TODO"
