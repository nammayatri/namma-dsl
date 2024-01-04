{-# LANGUAGE BangPatterns #-}

module NammaDSL.Generator.SQL.Table (generateSQL) where

import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Set as DS
-- import qualified Debug.Trace as DT
import Kernel.Prelude
import NammaDSL.DSL.Syntax.Storage
import Text.Casing (quietSnake)

mkSnake :: BeamField -> String
mkSnake = quietSnake . bFieldName

-- Generates SQL for creating a table and altering it to add columns
generateSQL :: Maybe MigrationFile -> TableDef -> String
generateSQL (Just oldSqlFile) tableDef = do
  let (updatedFields, newFields, deletedFields, pkChanged) = getUpdatesAndRest oldSqlFile tableDef
      tableName = tableNameSql tableDef
      anyChanges = length (updatedFields <> newFields <> deletedFields) > 0
      updateQueries = generateUpdateSQL tableName updatedFields
      pkChangeQuery = if pkChanged then "ALTER TABLE atlas_app." <> tableName <> " DROP CONSTRAINT " <> tableName <> "_pkey;\n" <> addKeySQL tableDef else ""
      newColumnQueries = addColumnSQL tableName newFields
      deleteQueries = generateDeleteSQL tableName deletedFields
  if anyChanges || pkChanged
    then oldSqlFile.rawLastSqlFile ++ updateStamp ++ intercalate "\n" (filter (not . null) [updateQueries, newColumnQueries, deleteQueries, pkChangeQuery])
    else oldSqlFile.rawLastSqlFile
generateSQL Nothing tableDef =
  createTableSQL tableDef ++ "\n" ++ alterTableSQL tableDef ++ "\n" ++ addKeySQL tableDef

updateStamp :: String
updateStamp = "\n\n\n------- SQL updates -------\n\n"

generateDeleteSQL :: String -> [BeamField] -> String
generateDeleteSQL tableName beamFields = intercalate "\n" . (flip map) beamFields $ \beamField -> do
  "ALTER TABLE atlas_app." ++ tableName ++ " DROP COLUMN " ++ (mkSnake beamField) ++ ";"

generateUpdateSQL :: String -> [BeamField] -> String
generateUpdateSQL tableName beamFields = intercalate "\n" . (flip map) beamFields $ \beamField -> intercalate "\n" . filter (not . null) . (flip map) (bFieldUpdates beamField) $ \fieldUpdaes -> case fieldUpdaes of
  DropDefault -> "ALTER TABLE atlas_app." <> tableName <> " ALTER COLUMN " <> (mkSnake beamField) <> " DROP DEFAULT;"
  AddDefault _ -> maybe "" (\dv -> "ALTER TABLE atlas_app." <> tableName <> " ALTER COLUMN " <> (mkSnake beamField) <> " SET DEFAULT " <> dv <> ";") (bDefaultVal beamField)
  TypeChange -> "ALTER TABLE atlas_app." <> tableName <> " ALTER COLUMN " <> (mkSnake beamField) <> " TYPE " <> (bSqlType beamField) <> ";"
  DropNotNull -> "ALTER TABLE atlas_app." <> tableName <> " ALTER COLUMN " <> (mkSnake beamField) <> " DROP NOT NULL;"
  AddNotNull -> "ALTER TABLE atlas_app." <> tableName <> " ALTER COLUMN " <> (mkSnake beamField) <> " SET NOT NULL;"
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

getUpdatesAndRest :: MigrationFile -> TableDef -> ([BeamField], [BeamField], [BeamField], Bool)
getUpdatesAndRest oldSqlFile tableDef = do
  let newSqlFields = M.fromList . map (\beamField -> (beamField.bFieldName, beamField)) $ concatMap (\field -> field.beamFields) (fields tableDef)
  let oldSqlFields = M.fromList . map (\beamField -> (beamField.bFieldName, beamField)) $ concatMap (\field -> field.beamFields) (fields_ oldSqlFile)
  let newKeyIds = DS.fromList $ tableDef.primaryKey <> tableDef.secondaryKey
  let oldKeyIds = DS.fromList $ oldSqlFile.primaryKeys <> oldSqlFile.secondaryKeys
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
createTableSQL :: TableDef -> String
createTableSQL tableDef =
  "CREATE TABLE atlas_app." ++ tableNameSql tableDef ++ " ();\n"

-- SQL for altering the table to add each column
alterTableSQL :: TableDef -> String
alterTableSQL tableDef =
  intercalate "\n" $ map (addColumnSQL (tableNameSql tableDef) . beamFields) $ filter (isNothing . relation) (fields tableDef)

-- SQL for adding a single column with constraints
addColumnSQL :: String -> [BeamField] -> String
addColumnSQL tableName beamFields =
  intercalate "\n" $
    map
      ( \fieldDef ->
          if bIsEncrypted fieldDef
            then
              generateAlterColumnSQL (mkSnake fieldDef ++ "_hash") "bytea" fieldDef
                ++ "\n"
                ++ generateAlterColumnSQL (mkSnake fieldDef ++ "_encrypted") "character varying(255)" fieldDef
            else generateAlterColumnSQL (mkSnake fieldDef) (bSqlType fieldDef) fieldDef
      )
      beamFields
  where
    generateAlterColumnSQL :: String -> String -> BeamField -> String
    generateAlterColumnSQL fieldName_ sqlType_ beamField =
      "ALTER TABLE atlas_app." ++ tableName ++ " ADD COLUMN " ++ intercalate " " (filter (not . null) [fieldName_, sqlType_]) ++ " "
        ++ unwords (mapMaybe constraintToSQL (bConstraints beamField))
        ++ maybe "" (" default " ++) (bDefaultVal beamField)
        ++ ";"

addKeySQL :: TableDef -> String
addKeySQL tableDef =
  let keys = map quietSnake $ primaryKey tableDef <> secondaryKey tableDef
   in "ALTER TABLE atlas_app." ++ (tableNameSql tableDef) ++ " ADD PRIMARY KEY ( "
        ++ intercalate ", " keys
        ++ ");"

-- ALTER TABLE tablename ADD PRIMARY KEY (column1, column2, ...);

-- Converts a FieldConstraint to SQL
constraintToSQL :: FieldConstraint -> Maybe String
constraintToSQL NotNull = Just "NOT NULL"
constraintToSQL _ = Nothing
