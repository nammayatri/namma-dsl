{-# LANGUAGE BangPatterns #-}

module NammaDSL.Generator.SQL.Table (generateSQL) where

import Control.Monad (foldM)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (isJust, isNothing, mapMaybe)
import qualified Data.Set as DS
import NammaDSL.DSL.Syntax.Common
import NammaDSL.DSL.Syntax.Storage
import NammaDSL.Utils (removeBeamFieldsWRTRelation)
import Text.Casing (quietSnake)
import Prelude

mkSnake :: BeamField -> String
mkSnake = quietSnake . bFieldName

-- Generates SQL for creating a table and altering it to add columns
generateSQL :: Database -> Maybe MigrationFile -> TableDef -> Either SQL_ERROR String
generateSQL database (Just oldSqlFile) tableDef = do
  SqlReportedChanges {..} <- getUpdatesAndRest oldSqlFile tableDef
  let tableName = tableNameSql tableDef
      anyChanges = not (null (updatedFields <> newFields <> deletedFields))
      anyIndexChanges = not (null (newIndexes <> toBeDeletedIndexes))
      updateQueries = generateUpdateSQL database tableName updatedFields
      pkChangeQuery = if isPkChanged then "ALTER TABLE " <> database <> "." <> tableName <> " DROP CONSTRAINT " <> tableName <> "_pkey;\n" <> addKeySQL database tableDef else ""
      upsertIndexes = upsertIndexesSQL database tableName toBeDeletedIndexes newIndexes
  newColumnQueries <- addColumnSQL True database tableName newFields
  let deleteQueries = generateDeleteSQL database tableName deletedFields
  if anyChanges || isPkChanged || anyIndexChanges
    then do
      let sqlContent = filter (not . null) [updateQueries, newColumnQueries, pkChangeQuery, deleteQueries, upsertIndexes]
      if not $ null sqlContent
        then Right $ oldSqlFile.rawLastSqlFile ++ updateStamp ++ intercalate "\n" sqlContent
        else Right $ oldSqlFile.rawLastSqlFile
    else Right $ oldSqlFile.rawLastSqlFile
generateSQL database Nothing tableDef = do
  altrStmts <- alterTableSQL database tableDef
  Right $ intercalate "\n" [createTableSQL database tableDef, altrStmts, addKeySQL database tableDef, upsertIndexesSQL database (tableNameSql tableDef) [] (indexes tableDef)]

upsertIndexesSQL :: Database -> String -> [IndexDef] -> [IndexDef] -> String
upsertIndexesSQL database tableNameSql delIndexes newIndexes =
  intercalate "\n" (delIndexesStr <> newIndexesStr)
  where
    delIndexesStr =
      map
        ( \indexDef ->
            if indexUnique indexDef
              then "ALTER TABLE " <> tableName <> " DROP CONSTRAINT " <> indexName indexDef <> ";"
              else "DROP INDEX " <> indexName indexDef <> ";"
        )
        delIndexes
    newIndexesStr =
      map
        ( \indexDef ->
            if indexUnique indexDef
              then "ALTER TABLE " <> tableName <> " ADD CONSTRAINT " <> indexName indexDef <> " UNIQUE (" <> indexColsStr indexDef <> ");"
              else "CREATE INDEX " <> indexName indexDef <> " ON " <> tableName <> " USING btree (" <> indexColsStr indexDef <> ");"
        )
        newIndexes
    tableName = database <> "." <> tableNameSql
    indexColsStr = intercalate ", " . map (wrapWithQuotes . quietSnake) . DS.toList . indexColumns

updateStamp :: String
updateStamp = "\n\n\n------- SQL updates -------\n\n"

generateDeleteSQL :: Database -> String -> [BeamField] -> String
generateDeleteSQL database tableName beamFields =
  let beamFieldsWithNotNull = filter (\field -> NotNull `elem` bConstraints field) beamFields
      dropStmts = intercalate "\n" . (flip map) beamFieldsWithNotNull $ \beamField -> do
        ("ALTER TABLE " <> database <> ".") <> tableName <> " ALTER COLUMN " <> mkSnake beamField <> " DROP NOT NULL;"
   in if null beamFieldsWithNotNull
        then mempty
        else
          "\n--- Now DSL don't allow dropping tables instead we will drop not null constraint if any .Please be careful while running ---\n"
            ++ dropStmts
            ++ "\n--- Drop section ends. Please check before running ---\n"

generateUpdateSQL :: Database -> String -> [BeamField] -> String
generateUpdateSQL database tableName beamFields = intercalate "\n" . (flip map) beamFields $ \beamField -> intercalate "\n" . filter (not . null) . (flip map) (bFieldUpdates beamField) $ \fieldUpdates -> case fieldUpdates of
  DropDefault -> ("ALTER TABLE " <> database <> ".") <> tableName <> " ALTER COLUMN " <> (mkSnake beamField) <> " DROP DEFAULT;"
  AddDefault _ -> maybe "" (\dv -> ("ALTER TABLE " <> database <> ".") <> tableName <> " ALTER COLUMN " <> (mkSnake beamField) <> " SET DEFAULT " <> dv <> ";") (bDefaultVal beamField)
  TypeChange -> ("ALTER TABLE " <> database <> ".") <> tableName <> " ALTER COLUMN " <> (mkSnake beamField) <> " TYPE " <> (bSqlType beamField) <> ";"
  DropNotNull -> ("ALTER TABLE " <> database <> ".") <> tableName <> " ALTER COLUMN " <> (mkSnake beamField) <> " DROP NOT NULL;"
  AddNotNull -> ("ALTER TABLE " <> database <> ".") <> tableName <> " ALTER COLUMN " <> (mkSnake beamField) <> " SET NOT NULL;"
  DropColumn -> ""

whichChanges :: BeamField -> BeamField -> Either SQL_ERROR [SqlFieldUpdates]
whichChanges oldField newField = do
  let nCs = DS.fromList $ bConstraints newField
  let oCs = DS.fromList $ bConstraints oldField
  let addedConstraints = DS.difference nCs oCs
  let removedConstraints = DS.difference oCs nCs
  -- check before --
  if DS.member NotNull removedConstraints && (isNothing $ bDefaultVal newField)
    then Left ("Cannot drop not null constraint without default value for column " <> bFieldName newField)
    else
      if DS.member NotNull addedConstraints && (isNothing $ bDefaultVal newField)
        then Left ("Cannot add not null constraint without default value for column " <> bFieldName newField)
        else
          let isChangeApplicable change =
                case change of
                  DropColumn -> False
                  DropDefault -> isNothing (bDefaultVal newField) && isJust (bDefaultVal oldField)
                  AddDefault _ -> isJust (bDefaultVal newField) && isNothing (bDefaultVal oldField) || bDefaultVal oldField /= bDefaultVal newField
                  TypeChange -> bSqlType newField /= bSqlType oldField
                  DropNotNull -> DS.member NotNull removedConstraints
                  AddNotNull -> DS.member NotNull addedConstraints
           in Right $ filter isChangeApplicable [AddDefault "Not_Required_Here", DropNotNull, AddNotNull, DropDefault, TypeChange]

getUpdatesAndRest :: MigrationFile -> TableDef -> Either SQL_ERROR SqlReportedChanges
getUpdatesAndRest oldSqlFile tableDef = do
  let newSqlFields = M.fromList . map (\beamField -> (mkSnake beamField, beamField)) $ concatMap (\field -> field.beamFields) (fields tableDef)
  let oldSqlFields = M.fromList . map (\beamField -> (mkSnake beamField, beamField)) $ concatMap (\field -> field.beamFields) (fields_ oldSqlFile)
  let newKeyIds = DS.fromList $ tableDef.primaryKey -- <> tableDef.secondaryKey -- As secondary key should not the included in the primary keys
  let oldKeyIds = DS.fromList $ oldSqlFile.primaryKeys -- <> oldSqlFile.secondaryKeys
  let isPkChanged = newKeyIds /= oldKeyIds
  let processChanges acc (columnName, newField) =
        case M.lookup columnName oldSqlFields of
          Just oldField -> do
            changesResult <- whichChanges oldField newField
            return $
              if null changesResult
                then acc
                else (newField {bFieldUpdates = changesResult} : acc)
          Nothing -> Right acc
  updatedFields <- foldM processChanges [] (M.toList newSqlFields)
  -- fst $
  --   M.mapAccumWithKey
  --     ( \acc columnName newField ->
  --         case M.lookup columnName oldSqlFields of
  --           Just oldField -> do
  --             let changes = whichChanges oldField newField
  --             if null changes
  --               then (acc, Nothing)
  --               else ((newField {bFieldUpdates = changes}) : acc, Just newField)
  --           Nothing -> (acc, Nothing)
  --     )
  --     []
  --     newSqlFields
  let newFields =
        fst $
          M.mapAccumWithKey
            ( \acc columnName newField ->
                case M.lookup columnName oldSqlFields of
                  Just _ -> (acc, Nothing)
                  Nothing -> (newField : acc, Just newField)
            )
            []
            newSqlFields
  let deletedFields =
        fst $
          M.mapAccumWithKey
            ( \acc columnName oldField ->
                case M.lookup columnName newSqlFields of
                  Just _ -> (acc, Nothing)
                  Nothing -> (oldField : acc, Just oldField)
            )
            []
            oldSqlFields
  let currIndexes = DS.fromList (indexes tableDef)
  let newIndexes = DS.toList (DS.difference currIndexes (pastIndexes oldSqlFile))
  let toBeDeletedIndexes = DS.toList (DS.difference (pastIndexes oldSqlFile) currIndexes)
  return SqlReportedChanges {..}

-- SQL for creating an empty table
createTableSQL :: Database -> TableDef -> String
createTableSQL database tableDef =
  "CREATE TABLE " <> database <> "." ++ tableNameSql tableDef ++ " ();\n"

-- SQL for altering the table to add each column
alterTableSQL :: Database -> TableDef -> Either SQL_ERROR String
alterTableSQL database tableDef = do
  altrStmts <- sequence $ map (addColumnSQL False database (tableNameSql tableDef) . beamFields) $ filter (removeBeamFieldsWRTRelation . relation) (fields tableDef)
  return (intercalate "\n" altrStmts)

-- SQL for adding a single column with constraints
addColumnSQL :: Bool -> Database -> String -> [BeamField] -> Either SQL_ERROR String
addColumnSQL hasOldMigration database tableName beamFields = do
  mappedAddColStmts <-
    sequence $
      map
        ( \fieldDef ->
            generateAlterColumnSQL database (mkSnake fieldDef) (bSqlType fieldDef) fieldDef
        )
        beamFields
  return (intercalate "\n" mappedAddColStmts)
  where
    generateAlterColumnSQL :: Database -> String -> String -> BeamField -> Either SQL_ERROR String
    generateAlterColumnSQL database' fieldName_ sqlType_ beamField =
      if hasOldMigration && NotNull `elem` bConstraints beamField
        then Left ("Error: Not allowed to add new columns with Not Null constraint. Please make the column " <> bFieldName beamField <> " nullable for backwards compatibility")
        else
          Right $
            ("ALTER TABLE " <> database' <> ".") ++ tableName ++ " ADD COLUMN " ++ intercalate " " (filter (not . null) [wrapWithQuotes fieldName_, sqlType_]) ++ " "
              ++ unwords (mapMaybe constraintToSQL (bConstraints beamField))
              ++ maybe "" (" default " ++) (bDefaultVal beamField)
              ++ ";"

addKeySQL :: Database -> TableDef -> String
addKeySQL database tableDef =
  let keys = map (wrapWithQuotes . quietSnake) $ primaryKey tableDef
   in ("ALTER TABLE " <> database <> ".") ++ tableNameSql tableDef ++ " ADD PRIMARY KEY ( "
        ++ intercalate ", " keys
        ++ ");"

-- ALTER TABLE tablename ADD PRIMARY KEY (column1, column2, ...);

-- Converts a FieldConstraint to SQL
constraintToSQL :: FieldConstraint -> Maybe String
constraintToSQL NotNull = Just "NOT NULL"
constraintToSQL _ = Nothing

wrapWithQuotes :: String -> String
wrapWithQuotes columnName
  | columnName `elem` sqlKeywords = "\"" ++ columnName ++ "\""
  | otherwise = columnName
  where
    sqlKeywords = ["group", "order", "inner", "left", "right", "full", "union", "insert", "values", "update", "set", "delete", "create", "alter", "drop", "truncate", "index", "constraint", "primary", "foreign", "default", "not", "distinct", "like", "between", "in", "exists", "case", "then", "else", "end", "null", "is", "count", "avg", "sum", "max", "min", "any", "all", "as"]
