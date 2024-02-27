{-# LANGUAGE TemplateHaskell #-}

module NammaDSL.Generator.Haskell.DomainType where

import Control.Monad.Reader (ask)
import qualified Data.List as L
import qualified Data.List.Split as L
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Tuple.Extra (both)
import NammaDSL.DSL.Syntax.Common
import Control.Monad.Writer hiding (Writer)
import qualified Data.List as L
import qualified Data.List.Split as L
import qualified Data.Text as T
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Language.Haskell.TH as TH
import NammaDSL.DSL.Syntax.Storage
import NammaDSL.Generator.Haskell.Common (checkForPackageOverrides, getRecordType)
import NammaDSL.GeneratorCore
import NammaDSL.Lib
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
    whenJust (types def) generateHaskellTypes

genTableType :: TableDef -> Writer CodeUnit
genTableType def = dec_ . pure $ do
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
    then TH.DerivClause Nothing $ TH.ConT <$> [''Generic]
    else TH.DerivClause Nothing $ TH.ConT <$> [''Generic, ''Show, ''ToJSON, ''FromJSON, ''ToSchema]

-- didn't find how we can use record wild cards for TH, so using simple records
generateEncryptionInstance :: TableDef -> Writer CodeUnit
generateEncryptionInstance tableDef = do
  type_ _Table [t|$_TableET 'AsEncrypted|]
  type_ _DecryptedTable [t|$_TableET 'AsUnencrypted|]

  decs_
    [d|
      instance EncryptedItem $_TableT where
        type Unencrypted $_TableT = ($_DecryptedTableT, HashSalt)
        encryptItem ($entityP, $saltP) =
          $( do_ $
               do
                 forM_ (fields tableDef) $ \(f :: FieldDef) -> do
                   let (fieldE, fieldUpdP, _) = mkTHVars f
                   when f.isEncrypted $
                     if isMaybeType f.haskellType
                       then fieldUpdP <-- [|encryptItem ((,$saltE) <$> $fieldE $entityE)|]
                       else fieldUpdP <-- [|encryptItem ($fieldE $entityE, $saltE)|]
                 pure_ updEntityExp
           )
        decryptItem $entityP =
          $( do_ $
               do
                 forM_ (fields tableDef) $ \(f :: FieldDef) -> do
                   let (fieldE, fieldUpdP, _) = mkTHVars f
                   when f.isEncrypted $
                     if isMaybeType f.haskellType
                       then fieldUpdP <-- [|fmap fst <$> decryptItem ($fieldE $entityE)|]
                       else fieldUpdP <-- [|fst <$> decryptItem ($fieldE $entityE)|]
                 pure_ [|($updEntityExp, "")|]
           )

      instance EncryptedItem' $_TableT where
        type UnencryptedItem $_TableT = $_DecryptedTableT
        toUnencrypted $(varP "a") $saltP = ($(varE "a"), $saltE)
        fromUnencrypted = fst
      |]
  where
    updEntityExp :: TH.Q TH.Exp
    updEntityExp = do
      recCon_ _Table $
        forM_ (fields tableDef) $ \(f :: FieldDef) -> do
          let (fieldE, _, fieldUpdE) = mkTHVars f
          if f.isEncrypted
            then fieldExp_ f.fieldName fieldUpdE
            else fieldExp_ f.fieldName [|$fieldE $entityE|]

    _Table = tableNameHaskell tableDef
    _TableT = conT _Table
    _TableET = conT $ _Table <> "E"
    _DecryptedTable = "Decrypted" <> _Table
    _DecryptedTableT = conT _DecryptedTable
    entityP = varP "entity"
    entityE = varE "entity"
    saltP = varP "salt"
    saltE = varE "salt"

    mkTHVars f = do
      let fieldE = varE f.fieldName
          fieldUpd = f.fieldName <> "_"
          fieldUpdE = varE fieldUpd
          fieldUpdP = varP fieldUpd
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
      let _thTypeName = pure . TH.VarE . TH.mkName $ "''" <> typeName
      let recordType = getRecordType recType -- TODO use in generation
      dec_ . pure $ do
        let restDerivations = addRestDerivations (concatMap (\(TypeObject _ (tname, (_, d))) -> if tname == typeName then d else []) typeObj)
        let derives = TH.DerivClause Nothing (TH.ConT <$> [''Eq, ''Ord, ''Show, ''Read, ''Generic, ''ToJSON, ''FromJSON, ''ToSchema] <> restDerivations)
        TH.DataD [] (TH.mkName typeName) [] Nothing (enumValues <&> (\enumValue -> TH.NormalC (TH.mkName enumValue) [])) [derives]
      splice_ [e|Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList $_thTypeName|]
      when (isHttpInstanceDerived typeObj) $
        splice_ [e|mkHttpInstancesForEnum $_thTypeName|]
      when (isJsonInstanceDerived typeObj typeName) $
        splice_ [e|Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForJSON $_thTypeName|]
    generateEnum _ _ = error "Invalid enum definition"

    addRestDerivations :: [String] -> [TH.Name]
    addRestDerivations derivations = map (TH.mkName . toInstanceName) $ filter (\x -> not $ L.isPrefixOf "'" x) derivations

    toInstanceName = \case
      "HttpInstance" -> "ToParamSchema"
      val -> val

    generateDataStructure :: RecordType -> String -> [(String, String)] -> Writer CodeUnit
    generateDataStructure recType typeName fields = do
      dec_ . pure $ do
        let recordType = getRecordType recType -- TODO use in generation
        let bang = TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness
        let restDerivations = addRestDerivations (concatMap (\(TypeObject _ (tname, (_, d))) -> if tname == typeName then d else []) typeObj)
        let derives = TH.DerivClause Nothing (TH.ConT <$> [''Generic, ''Show, ''ToJSON, ''FromJSON, ''ToSchema] <> restDerivations)
        TH.DataD [] (TH.mkName typeName) [] Nothing [TH.RecC (TH.mkName typeName) (fields <&> \(f, t) -> (TH.mkName f, bang, TH.ConT $ TH.mkName t))] [derives]
      let _thTypeName = pure . TH.VarE . TH.mkName $ "''" <> typeName
      when (isListInstanceDerived typeObj typeName) $
        splice_ [e|Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList $_thTypeName|]
      when (isJsonInstanceDerived typeObj typeName) $
        splice_ [e|Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForJSON $_thTypeName|]
