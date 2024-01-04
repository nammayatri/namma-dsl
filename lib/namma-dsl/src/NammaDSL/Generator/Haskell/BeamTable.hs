{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module NammaDSL.Generator.Haskell.BeamTable (generateBeamTable) where

import Data.List (intercalate, isInfixOf)
import Data.String.Interpolate (i, __i)
import qualified Data.Text as T
import Kernel.Prelude hiding (replicateM)
import NammaDSL.DSL.Syntax.Storage
import NammaDSL.GeneratorCore
import NammaDSL.Utils

generateBeamTable :: TableDef -> Code
generateBeamTable tableDef =
  generateCode generatorInput
  where
    generatorInput :: GeneratorInput
    generatorInput =
      GeneratorInput
        { _ghcOptions = ["-Wno-unused-imports"],
          _extensions = ["DerivingStrategies", "TemplateHaskell"],
          _moduleNm = "Storage.Beam." <> (capitalize $ tableNameHaskell tableDef),
          _simpleImports = ["Kernel.Prelude", "Tools.Beam.UtilsTH", "Kernel.External.Encryption"],
          _qualifiedImports = ["Database.Beam as B"] <> imports tableDef,
          _codeBody = generateCodeBody mkCodeBody tableDef
        }

mkCodeBody :: StorageM ()
mkCodeBody = do
  beamDataType
  primaryKeyToBeam
  tableInstancesToBeam

beamDataType :: StorageM ()
beamDataType = do
  dataSignature
  dataFields
  derivingInstances

primaryKeyToBeam :: StorageM ()
primaryKeyToBeam = do
  def <- ask
  let tableName = tellM (tableNameHaskell def)
  onNewLine $ do
    tellM "instance B.Table "
    withSpace tableName
    tellM "T where"
    onNewLine $
      withSpace $ withinSpaces $ tellM "data PrimaryKey"
    tableName
    tellM "T f ="
    withSpace tableName
    fetchPrimaryKey def
    derivingInstances
    onNewLine $ withSpace $ withinSpaces $ tellM "primaryKey ="
    tableName
    "Id "
      `followedBy` (tellM $ fromMaybe (error $ T.pack ("Primary Key not found for " ++ tableNameHaskell def)) (getPrimaryKeys (primaryKey def)))
  where
    fetchPrimaryKey tableDef = tellM $ fromMaybe (error $ T.pack ("Primary Key not found for " ++ tableNameHaskell tableDef)) (generateKeyTypes (filter (\f -> fieldName f `elem` primaryKey tableDef) $ fields tableDef))
    getPrimaryKeys [] = Nothing
    getPrimaryKeys [xs] = Just $ ". " <> xs
    getPrimaryKeys xs = Just $ foldl' handleAccPrimary "<$> " xs
    handleAccPrimary acc x = if acc == "<$> " then (acc ++ x) else acc ++ " <*> " ++ x

    getBeamTypeOfPrimaryKey :: FieldDef -> String
    getBeamTypeOfPrimaryKey field = case beamFields field of
      [beamField] -> bFieldType beamField
      _ -> error "Primary key should have only one beam field"

    formatType :: String -> String
    formatType t = if " " `isInfixOf` t then "(" ++ t ++ ")" else t

    generateKeyTypes :: [FieldDef] -> Maybe String
    generateKeyTypes [] = Nothing
    generateKeyTypes xs = Just $ foldl' handleAcc "Id" xs
      where
        handleAcc acc x = acc ++ " (B.C f " ++ formatType (getBeamTypeOfPrimaryKey x) ++ ")"

dataSignature :: StorageM ()
dataSignature = do
  def <- ask
  let tableName = tableNameHaskell def

  tellM $
    [__i|
    data #{tableName}T f = #{tableName}T\n
  |]

dataFields :: StorageM ()
dataFields = do
  def <- ask
  tellM $
    [i|  { #{intercalate ",\n    " (map fieldDefToBeam $ filter (isNothing . relation) (fields def))}
  }|]

derivingInstances :: StorageM ()
derivingInstances =
  onNewLine $
    [i|    deriving (#{intercalate ", " derives})|]
  where
    derives = ["Generic", "B.Beamable"]

tableInstancesToBeam :: StorageM ()
tableInstancesToBeam = do
  def <- ask
  let tableName = tableNameHaskell def
  onNewLine $
    [__i|
    type #{tableName} = #{tableName}T Identity

    $(enableKVPG ''#{tableName}T ['#{intercalate "', '" (primaryKey def)}] #{if null (secondaryKey def) then "[]" else "[['" <> intercalate "', '" (secondaryKey def) <> "]]"})

    $(mkTableInstances ''#{tableName}T "#{tableNameSql def}")
  |]

fieldDefToBeam :: FieldDef -> String
fieldDefToBeam hfield = do
  intercalate "\n" $
    map
      ( \field ->
          let bfName = (bFieldName field)
              isEncrypted = bIsEncrypted field
              encBlock =
                if isEncrypted
                  then
                    [__i|
                    #{bfName}Encrypted :: B.C f #{wrapMaybe "Text" field},
                    #{bfName}Hash :: B.C f #{wrapMaybe "DbHash" field}
                  |]
                  else
                    [__i|
                    #{bfName} :: B.C f #{formatType (bFieldType field)}
                  |]
           in encBlock
      )
      (beamFields hfield)
  where
    formatType :: String -> String
    formatType t = if " " `isInfixOf` t then [__i|(#{t})|] else t

    wrapMaybe :: String -> BeamField -> String
    wrapMaybe beamType field =
      if isMaybeType (bFieldType field)
        then [__i|(Maybe #{beamType})|]
        else beamType
