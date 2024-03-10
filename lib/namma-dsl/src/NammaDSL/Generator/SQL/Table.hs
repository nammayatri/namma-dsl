{-# LANGUAGE BangPatterns #-}

module NammaDSL.Generator.SQL.Table (generateSQL) where

import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (isJust, isNothing, mapMaybe)
import qualified Data.Set as DS
import NammaDSL.DSL.Syntax.Storage
import NammaDSL.Utils (removeBeamFieldsWRTRelation)
import Text.Casing (quietSnake)
import Prelude

mkSnake :: BeamField -> String
mkSnake = quietSnake . bFieldName

-- Generates SQL for creating a table and altering it to add columns
generateSQL :: Database -> Maybe MigrationFile -> TableDef -> String
generateSQL database (Just oldSqlFile) tableDef = do
  let (updatedFields, newFields, deletedFields, pkChanged) = getUpdatesAndRest oldSqlFile tableDef
      tableName = tableNameSql tableDef
      anyChanges = not (null (updatedFields <> newFields <> deletedFields))
      updateQueries = generateUpdateSQL database tableName updatedFields
      pkChangeQuery = if pkChanged then "ALTER TABLE " <> database <> "." <> tableName <> " DROP CONSTRAINT " <> tableName <> "_pkey;\n" <> addKeySQL database tableDef else ""
      newColumnQueries = addColumnSQL database tableName newFields
      deleteQueries = generateDeleteSQL database tableName deletedFields
  if anyChanges || pkChanged
    then oldSqlFile.rawLastSqlFile ++ updateStamp ++ intercalate "\n" (filter (not . null) [updateQueries, newColumnQueries, deleteQueries, pkChangeQuery])
    else oldSqlFile.rawLastSqlFile
generateSQL database Nothing tableDef =
  createTableSQL database tableDef ++ "\n" ++ alterTableSQL database tableDef ++ "\n" ++ addKeySQL database tableDef

updateStamp :: String
updateStamp = "\n\n\n------- SQL updates -------\n\n"

generateDeleteSQL :: Database -> String -> [BeamField] -> String
generateDeleteSQL database tableName beamFields = intercalate "\n" . (flip map) beamFields $ \beamField -> do
  ("ALTER TABLE " <> database <> ".") ++ tableName ++ " DROP COLUMN " ++ mkSnake beamField ++ ";"

generateUpdateSQL :: Database -> String -> [BeamField] -> String
generateUpdateSQL database tableName beamFields = intercalate "\n" . (flip map) beamFields $ \beamField -> intercalate "\n" . filter (not . null) . (flip map) (bFieldUpdates beamField) $ \fieldUpdates -> case fieldUpdates of
  DropDefault -> ("ALTER TABLE " <> database <> ".") <> tableName <> " ALTER COLUMN " <> (mkSnake beamField) <> " DROP DEFAULT;"
  AddDefault _ -> maybe "" (\dv -> ("ALTER TABLE " <> database <> ".") <> tableName <> " ALTER COLUMN " <> (mkSnake beamField) <> " SET DEFAULT " <> dv <> ";") (bDefaultVal beamField)
  TypeChange -> ("ALTER TABLE " <> database <> ".") <> tableName <> " ALTER COLUMN " <> (mkSnake beamField) <> " TYPE " <> (bSqlType beamField) <> ";"
  DropNotNull -> ("ALTER TABLE " <> database <> ".") <> tableName <> " ALTER COLUMN " <> (mkSnake beamField) <> " DROP NOT NULL;"
  AddNotNull -> ("ALTER TABLE " <> database <> ".") <> tableName <> " ALTER COLUMN " <> (mkSnake beamField) <> " SET NOT NULL;"
  DropColumn -> ""

whichChanges :: BeamField -> BeamField -> [SqlFieldUpdates]
whichChanges oldField newField = do
  let nCs = DS.fromList $ bConstraints newField
  let oCs = DS.fromList $ bConstraints oldField
  let addedConstraints = DS.difference nCs oCs
  let removedConstraints = DS.difference oCs nCs
  let isChangeApplicable change =
        case change of
          DropColumn -> False
          DropDefault -> isNothing (bDefaultVal newField) && isJust (bDefaultVal oldField)
          AddDefault _ -> isJust (bDefaultVal newField) && isNothing (bDefaultVal oldField) || bDefaultVal oldField /= bDefaultVal newField
          TypeChange -> bSqlType newField /= bSqlType oldField
          DropNotNull -> DS.member NotNull removedConstraints
          AddNotNull -> DS.member NotNull addedConstraints
  filter isChangeApplicable [DropNotNull, DropDefault, AddNotNull, AddDefault "Not_Required_Here", TypeChange]

-- Add a hash and encrypted field for encrypted fields
mkBeamFieldsForEncryptedTypes :: [BeamField] -> [BeamField]
mkBeamFieldsForEncryptedTypes = concatMap (\field -> if bIsEncrypted field then [field {bFieldName = bFieldName field ++ "Hash", bSqlType = "bytea"}, field {bFieldName = bFieldName field ++ "Encrypted", bSqlType = "character varying(255)"}] else [field])

getUpdatesAndRest :: MigrationFile -> TableDef -> ([BeamField], [BeamField], [BeamField], Bool)
getUpdatesAndRest oldSqlFile tableDef = do
  let newSqlFields = M.fromList . map (\beamField -> (mkSnake beamField, beamField)) $ mkBeamFieldsForEncryptedTypes $ concatMap (\field -> field.beamFields) (fields tableDef)
  let oldSqlFields = M.fromList . map (\beamField -> (mkSnake beamField, beamField)) $ concatMap (\field -> field.beamFields) (fields_ oldSqlFile)
  let newKeyIds = DS.fromList $ tableDef.primaryKey -- <> tableDef.secondaryKey -- As secondary key should not the included in the primary keys
  let oldKeyIds = DS.fromList $ oldSqlFile.primaryKeys -- <> oldSqlFile.secondaryKeys
  let isPkChanged = newKeyIds /= oldKeyIds
  let updatedFields =
        fst $
          M.mapAccumWithKey
            ( \acc columnName newField ->
                case M.lookup columnName oldSqlFields of
                  Just oldField -> do
                    let changes = whichChanges oldField newField
                    if null changes
                      then (acc, Nothing)
                      else ((newField {bFieldUpdates = changes}) : acc, Just newField)
                  Nothing -> (acc, Nothing)
            )
            []
            newSqlFields
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
  (updatedFields, newFields, deletedFields, isPkChanged)

-- SQL for creating an empty table
createTableSQL :: Database -> TableDef -> String
createTableSQL database tableDef =
  "CREATE TABLE " <> database <> "." ++ tableNameSql tableDef ++ " ();\n"

-- SQL for altering the table to add each column
alterTableSQL :: Database -> TableDef -> String
alterTableSQL database tableDef =
  intercalate "\n" $ map (addColumnSQL database (tableNameSql tableDef) . beamFields) $ filter (removeBeamFieldsWRTRelation . relation) (fields tableDef)

-- SQL for adding a single column with constraints
addColumnSQL :: Database -> String -> [BeamField] -> String
addColumnSQL database tableName beamFields =
  intercalate "\n" $
    map
      ( \fieldDef ->
          generateAlterColumnSQL database (mkSnake fieldDef) (bSqlType fieldDef) fieldDef
      )
      beamFields
  where
    generateAlterColumnSQL :: Database -> String -> String -> BeamField -> String
    generateAlterColumnSQL database' fieldName_ sqlType_ beamField =
      ("ALTER TABLE " <> database' <> ".") ++ tableName ++ " ADD COLUMN " ++ intercalate " " (filter (not . null) [fieldName_, sqlType_]) ++ " "
        ++ unwords (mapMaybe constraintToSQL (bConstraints beamField))
        ++ maybe "" (" default " ++) (bDefaultVal beamField)
        ++ ";"

addKeySQL :: Database -> TableDef -> String
addKeySQL database tableDef =
  let keys = map quietSnake $ primaryKey tableDef
   in ("ALTER TABLE " <> database <> ".") ++ tableNameSql tableDef ++ " ADD PRIMARY KEY ( "
        ++ intercalate ", " keys
        ++ ");"

-- ALTER TABLE tablename ADD PRIMARY KEY (column1, column2, ...);

-- Converts a FieldConstraint to SQL
constraintToSQL :: FieldConstraint -> Maybe String
constraintToSQL NotNull = Just "NOT NULL"
constraintToSQL _ = Nothing
