{-# LANGUAGE OverloadedStrings #-}

module NammaDSL.Generator.Haskell.BeamTable (generateBeamTable) where

import Control.Monad (forM_)
import Control.Monad.Reader (ask)
import Data.Bool (bool)
import Data.Data (Proxy (..))
import Data.Functor ((<&>))
import Data.List (isInfixOf)
import qualified Data.List.Extra as L
import Data.List.NonEmpty (NonEmpty ((:|)), fromList)
import Data.Maybe (fromMaybe)
import NammaDSL.Config (DefaultImports (..))
import NammaDSL.DSL.Syntax.Storage
import NammaDSL.Generator.Haskell.Common (checkForPackageOverrides)
import NammaDSL.GeneratorCore
import NammaDSL.Lib hiding (Q, Writer)
import qualified NammaDSL.Lib.TH as TH
import qualified NammaDSL.Lib.Types as TH
import NammaDSL.Utils
import Prelude

type Writer w = TH.Writer TableDef w

type Q w = TH.Q TableDef w

generateBeamTable :: DefaultImports -> StorageRead -> TableDef -> Code
generateBeamTable (DefaultImports qualifiedImp simpleImp _) storageRead tableDef =
  generateCode generatorInput
  where
    codeBody' = generateCodeBody mkCodeBody tableDef
    beamTypeModulePrefix = storageRead.beamTypeModulePrefix <> "."
    packageOverride :: [String] -> [String]
    packageOverride = checkForPackageOverrides (importPackageOverrides tableDef)

    generatorInput :: GeneratorInput
    generatorInput =
      GeneratorInput
        { _ghcOptions = ["-Wno-unused-imports"],
          _extensions = ["DerivingStrategies", "TemplateHaskell", "StandaloneDeriving"],
          _moduleNm = beamTypeModulePrefix <> capitalize (tableNameHaskell tableDef),
          _simpleImports = packageOverride simpleImp,
          _qualifiedImports = packageOverride $ removeUnusedQualifiedImports codeBody' (imports tableDef <> qualifiedImp),
          _codeBody = codeBody'
        }

mkCodeBody :: StorageM ()
mkCodeBody = do
  def <- ask
  tellM . fromMaybe mempty $
    interpreter def $ do
      beamDataType
      primaryKeyToBeam
      tableInstancesToBeam

beamDataType :: Writer CodeUnit
beamDataType = do
  def <- ask
  TH.dataDW
    (TH.mkName $ tableNameHaskell def <> "T")
    [TH.PlainTV (TH.mkName "f") ()]
    do
      recCW (TH.mkName $ tableNameHaskell def <> "T") $
        fieldDefToBeam `mapM_` filter (removeBeamFieldsWRTRelation . (.relation)) (fields def)
    (TH.derivClauseW Nothing $ TH.ConT . TH.mkName <$> ["Generic", "B.Beamable"])

fieldDefToBeam :: FieldDef -> Writer TH.FieldDec
fieldDefToBeam hfield = forM_ (beamFields hfield) $ \field -> do
  let bfName = TH.mkName $ bFieldName field
  let wrapMaybe beamType =
        if isMaybeType (bFieldType field)
          then cT "Maybe" ~~ beamType
          else beamType
  let wrapBeam t = cT "B.C" ~~ cT "f" ~~ t
  let formattedType = TH.appendT . fromList $ cT <$> words (bFieldType field)
  if bIsEncrypted field
    then do
      TH.fieldDecW bfName (wrapBeam $ wrapMaybe formattedType)
    else do
      TH.fieldDecW bfName (wrapBeam formattedType)

primaryKeyToBeam :: Writer CodeUnit
primaryKeyToBeam = do
  def <- ask
  let _TableT = tableNameHaskell def <> "T"
  TH.instanceDW (pure []) (cT "B.Table" ~~ cT _TableT) $ do
    dataInstDW
      (cT "PrimaryKey" ~~ cT _TableT ~~ vT "f")
      do
        let keyTypes =
              fromMaybe (error ("Primary Key not found for " ++ tableNameHaskell def)) $
                generateKeyTypes (filter (\f -> fieldName f `elem` primaryKey def) $ fields def)
        TH.normalCW (TH.mkName $ _TableId def) keyTypes
      (TH.derivClauseW Nothing $ TH.ConT <$> ["Generic", "B.Beamable"])
    TH.funDW "primaryKey" $ do
      let primaryKeyNames = primaryKey def
      let primaryKeysExp = fromMaybe (error ("Primary Key not found for " ++ tableNameHaskell def)) (getPrimaryKeysExps def primaryKeyNames)
      TH.clauseW [] $ TH.normalB primaryKeysExp
  where
    _TableId def = tableNameHaskell def <> "Id"
    getPrimaryKeysExps _ [] = Nothing
    getPrimaryKeysExps def [x] = Just $ cE (_TableId def) ~. vE x
    getPrimaryKeysExps def (x : xs) = do
      let fmapExp = cE (_TableId def) ~<$> vE x
      Just $ appendInfixE (vE "<*>") $ fmapExp :| (vE <$> xs)

    formatType :: String -> Q TH.Type
    formatType t =
      if " " `isInfixOf` t -- TODO move to parsing
        then TH.appendT . fromList $ cT <$> words t
        else cT t

    getBeamTypeOfPrimaryKey :: FieldDef -> String
    getBeamTypeOfPrimaryKey field = case beamFields field of
      [beamField] -> bFieldType beamField
      _ -> error "Primary key should have only one beam field"

    generateKeyTypes :: [FieldDef] -> Maybe [Q TH.Type]
    generateKeyTypes [] = Nothing
    generateKeyTypes xs = Just $ (\x -> TH.appendT $ cT "B.C" :| [vT "f", formatType (getBeamTypeOfPrimaryKey x)]) <$> xs

tableInstancesToBeam :: Writer CodeUnit
tableInstancesToBeam = do
  def <- ask
  let tableName = tableNameHaskell def
  tySynDW (TH.mkName tableName) [] (cT (tableName <> "T") ~~ cT "Identity")

  let thTableName = "''" <> tableName <> "T"
  spliceW . pure $ do
    TH.VarE "enableKVPG"
      `TH.AppE` TH.VarE (TH.mkName thTableName)
      `TH.AppE` TH.ListE (TH.VarE . TH.mkName . ("'" <>) <$> primaryKey def)
      `TH.AppE` TH.ListE (secondaryKey def <&> (\k -> TH.ListE [TH.VarE . TH.mkName . ("'" <>) $ k]))
  mapM_
    ( \instanceDef -> do
        let (instanceName, extraInstanceParam, isCustomInstance) = case instanceDef of
              MakeTableInstances -> ("mkTableInstances", Nothing, False)
              MakeTableInstancesGenericSchema -> ("mkTableInstancesGenericSchema", Nothing, False)
              MakeTableInstancesWithTModifier prm -> ("mkTableInstancesWithTModifier", Just prm, False)
              Custom name prm -> (name, bool (Just prm) Nothing (null prm), True)
        spliceW $ do
          TH.appendE . fromList $
            [ vE instanceName,
              vE thTableName
            ]
              <> [strE $ tableNameSql def | not isCustomInstance]
              <> maybe [] (\prm -> bool [pure $ readExpUnsafe (Proxy @[(String, String)]) prm] (rawE <$> (map L.trim (filter (not . null) $ L.splitOn " " prm))) isCustomInstance) extraInstanceParam
    )
    (beamTableInstance def)
