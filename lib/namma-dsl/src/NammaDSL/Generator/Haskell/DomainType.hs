{-# LANGUAGE TemplateHaskell #-}

module NammaDSL.Generator.Haskell.DomainType where

import Control.Monad.Reader (ask)
import Control.Monad.Writer hiding (Writer)
import Data.Bool (bool)
import Data.Data (Proxy (..))
import Data.Foldable
import Data.Functor
import qualified Data.List as L
import qualified Data.List.Extra as L
import Data.List.NonEmpty (fromList)
import Data.Maybe
import NammaDSL.Config (DefaultImports (..), GenerationType (DOMAIN_TYPE))
import NammaDSL.DSL.Syntax.Common
import NammaDSL.DSL.Syntax.Storage
import NammaDSL.Generator.Haskell.Common (checkForPackageOverrides)
import NammaDSL.GeneratorCore
import NammaDSL.Lib hiding (Q, Writer)
import qualified NammaDSL.Lib.TH as TH
import qualified NammaDSL.Lib.Types as TH
import NammaDSL.Utils (isMaybeType, removeUnusedQualifiedImports)
import Prelude

type Writer w = TH.Writer TableDef w

type Q w = TH.Q TableDef w

data DomainTypeCode = DomainTypeCode
  { domainTypeDefaultCode :: Code,
    domainTypeExtraCode :: Maybe Code
  }

generateDomainType :: DefaultImports -> StorageRead -> TableDef -> DomainTypeCode
generateDomainType (DefaultImports qualifiedImp simpleImp _packageImports _) storageRead tableDef =
  DomainTypeCode defaultCode extraDomainCode
  where
    generationType = DOMAIN_TYPE
    isExtraCode = EXTRA_DOMAIN_TYPE_FILE `elem` (extraOperations tableDef)
    defaultCode = generateCode generatorInput
    extraDomainCode = bool Nothing (Just $ generateCode extraFileGeneratorInput) isExtraCode
    domainTypeModulePrefix = storageRead.domainTypeModulePrefix
    packageOverride :: [String] -> [String]
    packageOverride = checkForPackageOverrides generationType (storagePackageMapping storageRead) (importPackageOverrides tableDef)

    moduleName' = bool (domainTypeModulePrefix ++ "." ++ tableNameHaskell tableDef) (domainTypeModulePrefix ++ "." ++ tableNameHaskell tableDef ++ " (module " ++ (domainTypeModulePrefix ++ "." ++ tableNameHaskell tableDef) ++ ", module ReExport)") isExtraCode
    extraFileModuleName = domainTypeModulePrefix ++ ".Extra." ++ tableNameHaskell tableDef

    allSimpleImports :: [String]
    allSimpleImports = createDefaultImports tableDef <> simpleImp

    allQualifiedImports :: [String]
    allQualifiedImports = removeDefaultImports allSimpleImports moduleName' $ (imports tableDef) <> qualifiedImp

    codeBody' = generateCodeBody mkCodeBody tableDef

    preventSameModuleImports :: [String] -> [String]
    preventSameModuleImports = filter (\x -> not ((domainTypeModulePrefix ++ "." ++ tableNameHaskell tableDef) == L.trim x))

    generatorInput :: GeneratorInput
    generatorInput =
      GeneratorInput
        { _ghcOptions = ["-Wno-unused-imports"] <> ["-Wno-dodgy-exports" | isExtraCode],
          _extensions = ["ApplicativeDo"],
          _moduleNm = moduleName',
          _moduleExports = Nothing,
          _simpleImports = packageOverride $ preventSameModuleImports $ allSimpleImports <> [(extraFileModuleName ++ " as ReExport") | isExtraCode],
          _qualifiedImports = packageOverride $ preventSameModuleImports $ (removeUnusedQualifiedImports codeBody' allQualifiedImports),
          _packageImports,
          _codeBody = codeBody'
        }
    extraFileGeneratorInput :: GeneratorInput
    extraFileGeneratorInput =
      GeneratorInput
        { _ghcOptions = ["-Wno-unused-imports", "-Wno-dodgy-exports"],
          _extensions = ["ApplicativeDo"],
          _moduleNm = extraFileModuleName,
          _moduleExports = Nothing,
          _simpleImports = packageOverride $ allSimpleImports,
          _qualifiedImports = [],
          _packageImports = [],
          _codeBody = generateCodeBody extraFileCodeBody tableDef
        }

extraFileCodeBody :: StorageM ()
extraFileCodeBody = do
  onNewLine $ tellM "-- Extra code goes here -- "

createDefaultImports :: TableDef -> [String]
createDefaultImports tableDef =
  ["Kernel.Prelude"] -- <> ["Tools.Beam.UtilsTH" | shouldImportUtilsTH (fromMaybe [] $ types tableDef)]
    <> ["Kernel.Utils.TH" | isHttpInstanceImportRequired (fromMaybe [] $ types tableDef)]
    <> ["Data.Aeson" | isHttpInstanceImportRequired (fromMaybe [] $ types tableDef)]
    <> ["Kernel.External.Encryption" | tableDef.containsEncryptedField]
    <> ["Domain.Types.Common (UsageSafety (..))" | isUsageSafetyRequired tableDef.derives]

removeDefaultImports :: [String] -> String -> [String] -> [String]
removeDefaultImports defaultImports moduleName = filter (moduleName /=) . filter (`notElem` defaultImports)

mkCodeBody :: StorageM ()
mkCodeBody = do
  def <- ask
  tellM . fromMaybe mempty $
    interpreter def $ do
      genTableType
      when (def.containsEncryptedField) $ generateEncryptionInstance
      forM_ (types def) generateHaskellTypes
      domaintableInstances
      when (isUsageSafetyRequired def.derives) usageSafetyInstance

genTableType :: Writer CodeUnit
genTableType = do
  def <- ask
  decW . pure $ do
    let derives' = case derives def of
          Nothing -> derivingInstances $ containsEncryptedField def
          Just deriveList -> do
            let derivesStrList = filter (\x -> not $ L.isPrefixOf "'" x) $ getInstanceToDerive <$> deriveList
            TH.DerivClause Nothing $ TH.ConT . TH.mkName <$> derivesStrList
        isUsageSafetyRequired' = isUsageSafetyRequired (derives def)
    let (typeName, typeVars) =
          if def.containsEncryptedField
            then (TH.mkName $ tableNameHaskell def <> "E", [TH.PlainTV (TH.mkName "e") ()])
            else
              if isUsageSafetyRequired'
                then (TH.mkName $ tableNameHaskell def <> "D", [TH.KindedTV (TH.mkName "s") () (TH.ConT (TH.mkName "UsageSafety"))])
                else (TH.mkName $ tableNameHaskell def, [])
    TH.DataD [] typeName typeVars Nothing [TH.RecC (TH.mkName $ tableNameHaskell def) (fields def <&> \field -> (TH.mkName field.fieldName, defaultBang, TH.ConT $ TH.mkName field.haskellType))] [derives']

derivingInstances :: Bool -> TH.DerivClause
derivingInstances containsEncryptedField =
  if containsEncryptedField
    then TH.DerivClause Nothing $ TH.ConT <$> ["Generic"]
    else TH.DerivClause Nothing $ TH.ConT <$> ["Generic", "Show", "ToJSON", "FromJSON", "ToSchema"]

usageSafetyInstance :: Writer CodeUnit
usageSafetyInstance = do
  def <- ask
  let tableName = tableNameHaskell def
  tySynDW (TH.mkName tableName) [] (cT (tableName <> "D") ~~ cT "'Safe")
  TH.instanceDW (pure []) (cT "FromJSON" ~~ cT ("(" ++ tableName ++ "D 'Unsafe)")) (pure ())
  TH.instanceDW (pure []) (cT "ToJSON" ~~ cT ("(" ++ tableName ++ "D 'Unsafe)")) (pure ())
  TH.instanceDW (pure []) (cT "FromJSON" ~~ cT ("(" ++ tableName ++ "D 'Safe)")) (pure ())
  TH.instanceDW (pure []) (cT "ToJSON" ~~ cT ("(" ++ tableName ++ "D 'Safe)")) (pure ())

-- didn't find how we can use record wild cards for TH, so using simple records
generateEncryptionInstance :: Writer CodeUnit
generateEncryptionInstance = do
  tableDef <- ask

  let _Table = tableNameHaskell tableDef
      _TableE = _Table <> "E"
      _DecryptedTable = "Decrypted" <> _Table
      entityP = TH.vP "entity"
      entityE = TH.vE "entity"
      saltP = TH.vP "salt"
      saltE = TH.vE "salt"

      mkTHVars f = do
        let fieldE = TH.vE f.fieldName
            fieldUpd = f.fieldName <> "_"
            fieldUpdE = TH.vE fieldUpd
            fieldUpdP = TH.vP fieldUpd
        (fieldE, fieldUpdP, fieldUpdE)

      updEntityExp :: Q TH.Exp
      updEntityExp = do
        TH.recConEW (TH.mkName _Table) $
          forM_ (fields tableDef) $ \(f :: FieldDef) -> do
            let (fieldE, _, fieldUpdE) = mkTHVars f
            if f.isEncrypted
              then fieldExpW (TH.mkName f.fieldName) fieldUpdE
              else fieldExpW (TH.mkName f.fieldName) (fieldE ~* entityE)

  TH.tySynDW (TH.mkName _Table) [] $ TH.cT _TableE ~~ TH.cT "'AsEncrypted"
  TH.tySynDW (TH.mkName _DecryptedTable) [] $ TH.cT _TableE ~~ TH.cT "'AsUnencrypted"

  TH.instanceDW (pure []) (cT "EncryptedItem" ~~ TH.cT _Table) $ do
    TH.tySynInstDW $
      TH.tySynEqn Nothing (TH.cT "Unencrypted" ~~ TH.cT _Table) $
        TH.tupleT 2 ~~ cT _DecryptedTable ~~ cT "HashSalt"

    TH.funDW "encryptItem" $ do
      TH.clauseW [TH.tupP [entityP, saltP]] $
        TH.normalB $
          TH.doEW $ do
            forM_ (fields tableDef) $ \(f :: FieldDef) -> do
              let (fieldE, fieldUpdP, _) = mkTHVars f
              when f.isEncrypted $
                if isMaybeType f.haskellType
                  then fieldUpdP <-- (vE "encryptItem" ~$ TH.tupE [Nothing, Just saltE] ~<$> fieldE ~* entityE)
                  else fieldUpdP <-- (vE "encryptItem" ~* TH.tupE [Just $ fieldE ~* entityE, Just saltE])
            TH.noBindSW $ vE "pure" ~* updEntityExp

    TH.funDW "decryptItem" $ do
      TH.clauseW [entityP] $ do
        TH.normalB $
          TH.doEW $ do
            forM_ (fields tableDef) $ \(f :: FieldDef) -> do
              let (fieldE, fieldUpdP, _) = mkTHVars f
              when f.isEncrypted $
                if isMaybeType f.haskellType
                  then fieldUpdP <-- (vE "fmap" ~* vE "fst" ~<$> vE "decryptItem" ~* (fieldE ~* entityE))
                  else fieldUpdP <-- (vE "fst" ~<$> vE "decryptItem" ~* (fieldE ~* entityE))
            TH.noBindSW $
              (vE "pure" ~*) $ do
                TH.tupE [Just updEntityExp, Just $ strE ""]

  TH.instanceDW (pure []) (TH.cT "EncryptedItem'" ~~ TH.cT _Table) $ do
    TH.tySynInstDW $ TH.tySynEqn Nothing (cT "UnencryptedItem" ~~ cT _Table) (TH.cT _DecryptedTable)
    TH.funDW "toUnencrypted" $ do
      TH.clauseW [TH.vP "a", saltP] $
        TH.normalB (TH.tupE [Just $ TH.vE "a", Just saltE])
    TH.funDW "fromUnencrypted" $ do
      TH.clauseW [] $
        TH.normalB (vE "fst")

isHttpInstanceDerived' :: [TypeObject] -> TypeName -> Bool
isHttpInstanceDerived' typeObj tpName =
  any (\case TypeObject _ nm _ derive _ -> (tpName == nm) && InstanceToDerive "HttpInstance" `elem` derive) typeObj

isHttpInstanceImportRequired :: [TypeObject] -> Bool
isHttpInstanceImportRequired = any (\case TypeObject _ _ _ derive _ -> InstanceToDerive "HttpInstance" `elem` derive)

isListInstanceDerived :: [TypeObject] -> TypeName -> Bool
isListInstanceDerived typeObj tpName =
  any (\case TypeObject _ nm _ derive _ -> (tpName == nm) && InstanceToDerive "'ListInstance" `elem` derive) typeObj

isJsonInstanceDerived :: [TypeObject] -> TypeName -> Bool
isJsonInstanceDerived typeObj tpName =
  any (\case TypeObject _ nm _ derive _ -> (tpName == nm) && InstanceToDerive "'JsonInstance" `elem` derive) typeObj

isUsageSafetyRequired :: Maybe [InstanceToDerive] -> Bool
isUsageSafetyRequired derives = fromMaybe False (("'UsageSafety" `elem`) <$> map getInstanceToDerive <$> derives)

isEnum :: [(FieldName, FieldType)] -> Bool
isEnum [(FieldName "enum", _)] = True
isEnum _ = False

generateHaskellTypes :: [TypeObject] -> Writer CodeUnit
generateHaskellTypes typeObj = traverse_ processType typeObj
  where
    processType :: TypeObject -> Writer CodeUnit
    processType (TypeObject recType typeName fields _ domainInstanceOverriden)
      | isEnum fields = generateEnum recType typeName fields domainInstanceOverriden
      | otherwise = generateDataStructure recType typeName fields

    generateEnum :: RecordType -> TypeName -> [(FieldName, FieldType)] -> Bool -> Writer CodeUnit
    generateEnum recType typeName [(FieldName "enum", values)] isOverrideDomainInstance = do
      let enumValues = L.splitOn "," values.getFieldType -- TODO move to parsing
      let _thTypeName = vE $ "''" <> typeName.getTypeName
      TH.decW . pure $ do
        let restDerivations = addRestDerivations (concatMap (\(TypeObject _ tname _ d _) -> if tname == typeName then d else []) typeObj)
        let overrideDerives = maybe False (\(TypeObject _ _ _ _ od) -> od) $ find (\(TypeObject _ tname _ _ _) -> tname == typeName) typeObj
        let derives = TH.DerivClause Nothing (TH.ConT <$> (bool (["Eq", "Ord", "Show", "Read", "Generic", "ToJSON", "FromJSON", "ToSchema"] <> restDerivations) restDerivations overrideDerives))
        case recType of
          NewType -> do
            let (newTypeCons, internalType) = case enumValues of
                  [enumValue] -> case words enumValue of
                    [a, b] -> (a, b)
                    _ -> error "Newtype enum value should have exactly one field"
                  _ -> error "Newtype enum value should have exactly one constructor"
            TH.NewtypeD [] (TH.mkName typeName.getTypeName) [] Nothing (TH.NormalC (TH.mkName newTypeCons) [(defaultBang, TH.ConT $ TH.mkName internalType)]) [derives]
          --error "Generate haskell domain enum types: expected Data but got NewType"
          Data -> TH.DataD [] (TH.mkName typeName.getTypeName) [] Nothing (enumValues <&> (\enumValue -> TH.NormalC (TH.mkName enumValue) [])) [derives]
          Type -> TH.TySynD (TH.mkName typeName.getTypeName) [] (foldl1 TH.AppT $ map (TH.VarT . TH.mkName) enumValues)
      unless isOverrideDomainInstance $ do
        TH.spliceW $ vE "Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList" ~* _thTypeName
        when (isHttpInstanceDerived' typeObj typeName) $
          TH.spliceW $ vE "mkHttpInstancesForEnum" ~* _thTypeName
        when (isJsonInstanceDerived typeObj typeName) $
          TH.spliceW $ vE "Tools.Beam.UtilsTH.mkBeamInstancesForJSON" ~* _thTypeName
    generateEnum _ _ _ _ = error "Invalid enum definition"

    addRestDerivations :: [InstanceToDerive] -> [TH.Name]
    addRestDerivations derivations = map (TH.mkName . toInstanceName) $ filter (\x -> not $ L.isPrefixOf "'" x) $ getInstanceToDerive <$> derivations

    toInstanceName = \case
      "HttpInstance" -> "ToParamSchema"
      val -> val

    generateDataStructure :: RecordType -> TypeName -> [(FieldName, FieldType)] -> Writer CodeUnit
    generateDataStructure recType typeName fields = do
      def <- ask
      let isOverrideDomainInstance = not $ null (domainTableInstance def)
      TH.decW . pure $ do
        let restDerivations = addRestDerivations (concatMap (\(TypeObject _ tname _ d _) -> if tname == typeName then d else []) typeObj)
        let overrideDerives = maybe False (\(TypeObject _ _ _ _ od) -> od) $ find (\(TypeObject _ tname _ _ _) -> tname == typeName) typeObj
        let derives = TH.DerivClause Nothing (TH.ConT <$> (bool (["Generic", "Show", "ToJSON", "FromJSON", "ToSchema"] <> restDerivations) restDerivations overrideDerives))
        let thTypeName = TH.mkName typeName.getTypeName
        case recType of
          NewType -> do
            let (f, t) = case fields of
                  [field] -> field
                  _ -> error $ "Generate data structure: expected exactly one record for NewType but got: " <> show fields

            TH.NewtypeD [] thTypeName [] Nothing (TH.RecC thTypeName [(TH.mkName f.getFieldName, defaultBang, TH.ConT $ TH.mkName t.getFieldType)]) [derives]
          Data -> TH.DataD [] thTypeName [] Nothing [TH.RecC thTypeName (fields <&> \(f, t) -> (TH.mkName f.getFieldName, defaultBang, TH.ConT $ TH.mkName t.getFieldType))] [derives]
          Type -> error "Generate data structure: expected Data but got Type"
      unless isOverrideDomainInstance $ do
        let spliceTypeName = pure . TH.VarE . TH.mkName $ "''" <> typeName.getTypeName
        when (isListInstanceDerived typeObj typeName) $
          TH.spliceW $ TH.vE "Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList" ~* spliceTypeName
        when (isJsonInstanceDerived typeObj typeName) $
          TH.spliceW $ TH.vE "Tools.Beam.UtilsTH.mkBeamInstancesForJSON" ~* spliceTypeName

domaintableInstances :: Writer CodeUnit
domaintableInstances = do
  def <- ask
  let thTableName = "''" <> (tableNameHaskell def) <> "T"
  mapM_
    ( \instanceDef -> do
        let (instanceName, dName, extraInstanceParam, isCustomInstance) = case instanceDef of
              Custom name dataName prm -> (name, dataName, bool (Just prm) Nothing (null prm), True)
              _ -> error "Please add custom instance in domain type"
        spliceW $ do
          TH.appendE . fromList $
            [ vE instanceName,
              maybe (vE thTableName) (vE . ("''" ++)) dName
            ]
              <> maybe [] (\prm -> bool [pure $ readExpUnsafe (Proxy @[(String, String)]) prm] (vE <$> (map L.trim (filter (not . null) $ L.splitOn " " prm))) isCustomInstance) extraInstanceParam
    )
    (domainTableInstance def)
