{-# LANGUAGE TemplateHaskell #-}

module NammaDSL.Generator.Haskell.DomainType where

import Control.Monad.Reader (ask)
import Control.Monad.Writer hiding (Writer)
import Data.Foldable
import Data.Functor
import qualified Data.List as L
import qualified Data.List.Split as L
import Data.Maybe
import qualified Data.Text as T
import qualified Language.Haskell.TH as TH
import NammaDSL.DSL.Syntax.Common
import NammaDSL.DSL.Syntax.Storage
import NammaDSL.Generator.Haskell.Common (checkForPackageOverrides)
import NammaDSL.GeneratorCore
import NammaDSL.Lib
import qualified NammaDSL.Lib.TH as TH
import NammaDSL.Utils (isMaybeType)
import Prelude

generateDomainType :: TableDef -> Code
generateDomainType tableDef =
  generateCode generatorInput
  where
    packageOverride :: [String] -> [String]
    packageOverride = checkForPackageOverrides (importPackageOverrides tableDef)

    moduleName' = "Domain.Types." ++ tableNameHaskell tableDef

    allSimpleImports :: [String]
    allSimpleImports = createDefaultImports tableDef

    allQualifiedImports :: [String]
    allQualifiedImports = removeDefaultImports allSimpleImports moduleName' $ (imports tableDef) <> ["Tools.Beam.UtilsTH"]

    generatorInput :: GeneratorInput
    generatorInput =
      GeneratorInput
        { _ghcOptions = ["-Wno-unused-imports"],
          _extensions = ["ApplicativeDo", "TemplateHaskell"],
          _moduleNm = moduleName',
          _simpleImports = packageOverride allSimpleImports,
          _qualifiedImports = packageOverride allQualifiedImports,
          _codeBody = generateCodeBody (mkCodeBody (allSimpleImports <> allQualifiedImports)) tableDef
        }

createDefaultImports :: TableDef -> [String]
createDefaultImports tableDef =
  ["Kernel.Prelude"] -- <> ["Tools.Beam.UtilsTH" | shouldImportUtilsTH (fromMaybe [] $ types tableDef)]
    <> ["Kernel.Utils.TH" | isHttpInstanceDerived (fromMaybe [] $ types tableDef)]
    <> ["Data.Aeson" | isHttpInstanceDerived (fromMaybe [] $ types tableDef)]
    <> ["Kernel.External.Encryption" | tableDef.containsEncryptedField]

removeDefaultImports :: [String] -> String -> [String] -> [String]
removeDefaultImports defaultImports moduleName = filter (moduleName /=) . filter (`notElem` defaultImports)

-- shouldImportUtilsTH :: [TypeObject] -> Bool
-- shouldImportUtilsTH typeObj =
--   any
--     ( \case
--         TypeObject (_, (fields, _)) -> isEnum fields
--     )
--     typeObj

mkCodeBody :: [String] -> StorageM ()
mkCodeBody allImports = do
  def <- ask
  tellM . interpreter allImports $ do
    genTableType def
    when (def.containsEncryptedField) $ generateEncryptionInstance def
    forM_ (types def) generateHaskellTypes

genTableType :: TableDef -> Writer CodeUnit
genTableType def = decW . pure $ do
  let bang = TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness

  let derives' = case derives def of
        Nothing -> derivingInstances $ containsEncryptedField def
        Just derivesStr -> do
          let derivesStrList = map T.unpack (T.split (== ',') (T.pack derivesStr)) -- FIXME move to parsing
          TH.DerivClause Nothing $ TH.ConT . TH.mkName <$> derivesStrList
  let (typeName, typeVars) =
        if def.containsEncryptedField
          then (TH.mkName $ tableNameHaskell def ++ "E", [TH.PlainTV (TH.mkName "e") ()])
          else (TH.mkName $ tableNameHaskell def, [])
  TH.DataD [] typeName typeVars Nothing [TH.RecC (TH.mkName $ tableNameHaskell def) (fields def <&> \field -> (TH.mkName field.fieldName, bang, TH.ConT $ TH.mkName field.haskellType))] [derives']

derivingInstances :: Bool -> TH.DerivClause
derivingInstances containsEncryptedField =
  if containsEncryptedField
    then TH.DerivClause Nothing $ TH.ConT <$> ["Generic"]
    else TH.DerivClause Nothing $ TH.ConT <$> ["Generic", "Show", "ToJSON", "FromJSON", "ToSchema"]

-- didn't find how we can use record wild cards for TH, so using simple records
generateEncryptionInstance :: TableDef -> Writer CodeUnit
generateEncryptionInstance tableDef = do
  TH.tySynDW _Table [] (TH.conT _TableE `TH.appT` TH.conT "'AsEncrypted") -- _Table :: Name
  TH.tySynDW _DecryptedTable [] (TH.conT _TableE `TH.appT` TH.conT "'AsUnencrypted")

  TH.instanceDW (pure []) (TH.conT "EncryptedItem" `TH.appT` TH.conT _Table) $ do
    TH.tySynInstDW $ TH.tySynEqn Nothing (TH.conT "Unencrypted" `TH.appT` TH.conT _Table) [t|($(TH.conT _DecryptedTable), $(TH.conT "HashSalt"))|]
    TH.funDW "encryptItem" $ do
      -- IsString Name
      TH.clauseW [[p|($entityP, $saltP)|]] $
        TH.normalB $
          TH.doEW $ do
            forM_ (fields tableDef) $ \(f :: FieldDef) -> do
              let (fieldE, fieldUpdP, _) = mkTHVars f
              when f.isEncrypted $
                if isMaybeType f.haskellType
                  then fieldUpdP <-- [|encryptItem ((,$saltE) <$> $fieldE $entityE)|]
                  else fieldUpdP <-- [|encryptItem ($fieldE $entityE, $saltE)|]
            TH.pureW updEntityExp

    TH.funDW "decryptItem" $ do
      TH.clauseW [entityP] $ do
        TH.normalB $
          TH.doEW $ do
            forM_ (fields tableDef) $ \(f :: FieldDef) -> do
              let (fieldE, fieldUpdP, _) = mkTHVars f
              when f.isEncrypted $
                if isMaybeType f.haskellType
                  then fieldUpdP <-- [|fmap fst <$> decryptItem ($fieldE $entityE)|]
                  else fieldUpdP <-- [|fst <$> decryptItem ($fieldE $entityE)|]
            TH.pureW [|($updEntityExp, "")|]

  TH.instanceDW (pure []) (TH.conT "EncryptedItem'" `TH.appT` TH.conT _Table) $ do
    TH.tySynInstDW $ TH.tySynEqn Nothing (TH.conT "UnencryptedItem" `TH.appT` TH.conT _Table) (TH.conT _DecryptedTable)
    TH.funDW "toUnencrypted" $ do
      TH.clauseW [TH.varP "a", saltP] $
        TH.normalB [|($(TH.varE "a"), $saltE)|]
    TH.funDW "fromUnencrypted" $ do
      TH.clauseW [] $
        TH.normalB [|fst|]
  where
    updEntityExp :: TH.Q TH.Exp
    updEntityExp = do
      TH.recConEW _Table $
        forM_ (fields tableDef) $ \(f :: FieldDef) -> do
          let (fieldE, _, fieldUpdE) = mkTHVars f
          if f.isEncrypted
            then fieldExpW (TH.mkName f.fieldName) fieldUpdE
            else fieldExpW (TH.mkName f.fieldName) [|$fieldE $entityE|]

    _Table = TH.mkName $ tableNameHaskell tableDef
    _TableE = _Table <> "E"
    _DecryptedTable = "Decrypted" <> _Table
    entityP = TH.varP "entity"
    entityE = TH.varE "entity"
    saltP = TH.varP "salt"
    saltE = TH.varE "salt"

    mkTHVars f = do
      let fieldE :: TH.Q TH.Exp = TH.varE $ TH.mkName f.fieldName
          fieldUpd = f.fieldName <> "_"
          fieldUpdE :: TH.Q TH.Exp = TH.varE $ TH.mkName fieldUpd
          fieldUpdP :: TH.Q TH.Pat = TH.varP $ TH.mkName fieldUpd
      (fieldE, fieldUpdP, fieldUpdE)

isHttpInstanceDerived :: [TypeObject] -> Bool
isHttpInstanceDerived = any (\case TypeObject _ (_, (_, derive)) -> "HttpInstance" `elem` derive)

isListInstanceDerived :: [TypeObject] -> String -> Bool
isListInstanceDerived typeObj tpName =
  any (\case TypeObject _ (nm, (_, derive)) -> (tpName == nm) && "'ListInstance" `elem` derive) typeObj

isJsonInstanceDerived :: [TypeObject] -> String -> Bool
isJsonInstanceDerived typeObj tpName =
  any (\case TypeObject _ (nm, (_, derive)) -> (tpName == nm) && "'JsonInstance" `elem` derive) typeObj

isEnum :: [(String, String)] -> Bool
isEnum [("enum", _)] = True
isEnum _ = False

generateHaskellTypes :: [TypeObject] -> Writer CodeUnit
generateHaskellTypes typeObj = traverse_ processType typeObj -- (both concat . unzip . map (both L.unlines . processType)) typeObj
  where
    processType :: TypeObject -> Writer CodeUnit
    processType (TypeObject recType (typeName, (fields, _)))
      | isEnum fields = generateEnum recType typeName fields
      | otherwise = generateDataStructure recType typeName fields

    generateEnum :: RecordType -> String -> [(String, String)] -> Writer CodeUnit
    generateEnum recType typeName [("enum", values)] = do
      let enumValues = L.splitOn "," values
      let _thTypeName = TH.varE . TH.mkName $ "''" <> typeName

      TH.decW . pure $ do
        let restDerivations = addRestDerivations (concatMap (\(TypeObject _ (tname, (_, d))) -> if tname == typeName then d else []) typeObj)
        let derives = TH.DerivClause Nothing (TH.ConT <$> ["Eq", "Ord", "Show", "Read", "Generic", "ToJSON", "FromJSON", "ToSchema"] <> restDerivations)
        case recType of
          NewType -> error "Generate haskell domain types: expected Data but got NewType" -- can be newtype here?
          Data -> TH.DataD [] (TH.mkName typeName) [] Nothing (enumValues <&> (\enumValue -> TH.NormalC (TH.mkName enumValue) [])) [derives]
          Type -> error "Generate haskell domain types: expected Data but got Type"
      TH.spliceW $ TH.varE "Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList" `TH.appE` _thTypeName
      when (isHttpInstanceDerived typeObj) $
        TH.spliceW $ TH.varE "mkHttpInstancesForEnum" `TH.appE` _thTypeName
      when (isJsonInstanceDerived typeObj typeName) $
        TH.spliceW $ TH.varE "Tools.Beam.UtilsTH.mkBeamInstancesForJSON" `TH.appE` _thTypeName
    generateEnum _ _ _ = error "Invalid enum definition"

    addRestDerivations :: [String] -> [TH.Name]
    addRestDerivations derivations = map (TH.mkName . toInstanceName) $ filter (\x -> not $ L.isPrefixOf "'" x) derivations

    toInstanceName = \case
      "HttpInstance" -> "ToParamSchema"
      val -> val

    generateDataStructure :: RecordType -> String -> [(String, String)] -> Writer CodeUnit
    generateDataStructure recType typeName fields = do
      TH.decW . pure $ do
        let bang = TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness
        let restDerivations = addRestDerivations (concatMap (\(TypeObject _ (tname, (_, d))) -> if tname == typeName then d else []) typeObj)
        let derives = TH.DerivClause Nothing (TH.ConT <$> ["Generic", "Show", "ToJSON", "FromJSON", "ToSchema"] <> restDerivations)
        case recType of
          NewType -> do
            let (f, t) = case fields of
                  [field] -> field
                  _ -> error $ "Generate data structure: expected exactly one record for NewType but got: " <> show fields
            TH.NewtypeD [] (TH.mkName typeName) [] Nothing (TH.RecC (TH.mkName typeName) [(TH.mkName f, bang, TH.ConT $ TH.mkName t)]) [derives]
          Data -> TH.DataD [] (TH.mkName typeName) [] Nothing [TH.RecC (TH.mkName typeName) (fields <&> \(f, t) -> (TH.mkName f, bang, TH.ConT $ TH.mkName t))] [derives]
          Type -> error "Generate data structure: expected Data but got Type"

      let _thTypeName = pure . TH.VarE . TH.mkName $ "''" <> typeName
      when (isListInstanceDerived typeObj typeName) $
        TH.spliceW $ TH.varE "Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList" `TH.appE` _thTypeName
      when (isJsonInstanceDerived typeObj typeName) $
        TH.spliceW $ TH.varE "Tools.Beam.UtilsTH.mkBeamInstancesForJSON" `TH.appE` _thTypeName
