module NammaDSL.Generator.Haskell.DomainType where

import qualified Data.List as L
import qualified Data.List.Split as L
import Data.Tuple.Extra (both)
import Kernel.Prelude
import NammaDSL.DSL.Syntax.Storage
import NammaDSL.Generator.Haskell.Common (checkForPackageOverrides)
import NammaDSL.GeneratorCore
import NammaDSL.Utils (isMaybeType)

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
          _codeBody = generateCodeBody mkCodeBody tableDef
        }

mkCodeBody :: StorageM ()
mkCodeBody = do
  def <- ask
  let derivations = fromMaybe (L.intercalate "," (derivingInstances $ containsEncryptedField def)) (derives def)
  let seperator = onNewLine $ tellM $ "  , "
  onNewLine $
    tellM $
      "data " <> tableNameHaskellType def <> " = " <> tableNameHaskell def
  onNewLine $
    withSomeSpaces 2 $
      withinCurls $
        lineSpace $
          withinSpaces $ intercalateA seperator (map fieldDefToHaskell (fields def))
  onNewLine $ tellM $ "  deriving (" ++ derivations ++ ")\n\n"
  if def.containsEncryptedField then generateEncryptionInstance def else pure ()
  onNewLine $ tellM $ maybe "" (uncurry (++) . generateHaskellTypes) (types def)

tableNameHaskellType :: TableDef -> String
tableNameHaskellType tableDef = do
  if tableDef.containsEncryptedField
    then tableNameHaskell tableDef ++ "E e"
    else tableNameHaskell tableDef

derivingInstances :: Bool -> [String]
derivingInstances containsEncryptedField =
  if containsEncryptedField
    then ["Generic"]
    else ["Generic", "Show", "ToJSON", "FromJSON", "ToSchema"]

generateEncryptionInstance :: TableDef -> StorageM ()
generateEncryptionInstance tableDef =
  onNewLine $
    tellM $
      unlines $
        [ "type " ++ baseType ++ " = " ++ encryptedType ++ "\n",
          "type " ++ decryptBaseType ++ " = " ++ decryptedType ++ "\n",
          "instance EncryptedItem " ++ baseType ++ " where",
          "  type Unencrypted " ++ baseType ++ " = (" ++ decryptBaseType ++ ", HashSalt)",
          "  encryptItem (" ++ baseType ++ " {..}, salt) = do",
          unlines ((catMaybes $ map encryptField (fields tableDef)) ++ ["    return " ++ baseType ++ " {" ++ L.intercalate "," (catMaybes $ map mapFields (fields tableDef)) ++ ", ..}"]),
          "  decryptItem " ++ baseType ++ " {..} = do",
          unlines ((catMaybes $ map decryptField (fields tableDef)) ++ ["    return (" ++ baseType ++ " {" ++ L.intercalate "," (catMaybes $ map mapFields (fields tableDef)) ++ ", ..}, \"\")\n"]),
          "instance EncryptedItem' " ++ baseType ++ " where",
          "  type UnencryptedItem " ++ baseType ++ " = " ++ decryptBaseType,
          "  toUnencrypted a salt = (a, salt)",
          "  fromUnencrypted = fst\n\n"
        ]
  where
    baseType = tableNameHaskell tableDef
    decryptBaseType = "Decrypted" ++ tableNameHaskell tableDef
    encryptedType = baseType ++ "E 'AsEncrypted"
    decryptedType = baseType ++ "E 'AsUnencrypted"
    encryptField field = do
      if field.isEncrypted
        then
          if isMaybeType field.haskellType
            then Just $ "    " ++ field.fieldName ++ "_ <- encryptItem $ (,salt) <$> " ++ field.fieldName
            else Just $ "    " ++ field.fieldName ++ "_ <- encryptItem $ (" ++ field.fieldName ++ ",salt)"
        else Nothing
    decryptField field = do
      if field.isEncrypted
        then
          if isMaybeType field.haskellType
            then Just $ "    " ++ field.fieldName ++ "_ <- fmap fst <$> decryptItem " ++ field.fieldName
            else Just $ "    " ++ field.fieldName ++ "_ <- fst <$> decryptItem " ++ field.fieldName
        else Nothing

    mapFields field = do
      if field.isEncrypted
        then Just $ field.fieldName ++ " = " ++ field.fieldName ++ "_"
        else Nothing

removeDefaultImports :: [String] -> String -> [String] -> [String]
removeDefaultImports defaultImports moduleName = filter ((/=) moduleName) . filter (`notElem` defaultImports)

-- Convert FieldDef to Haskell field
fieldDefToHaskell :: FieldDef -> StorageM ()
fieldDefToHaskell fieldDef =
  tellM $
    fieldDef.fieldName ++ " :: " ++ fieldDef.haskellType

createDefaultImports :: TableDef -> [String]
createDefaultImports tableDef =
  ["Kernel.Prelude"] -- <> ["Tools.Beam.UtilsTH" | shouldImportUtilsTH (fromMaybe [] $ types tableDef)]
    <> ["Kernel.Utils.TH" | isHttpInstanceDerived (fromMaybe [] $ types tableDef)]
    <> ["Data.Aeson" | isHttpInstanceDerived (fromMaybe [] $ types tableDef)]
    <> ["Kernel.External.Encryption" | tableDef.containsEncryptedField]

-- shouldImportUtilsTH :: [TypeObject] -> Bool
-- shouldImportUtilsTH typeObj =
--   any
--     ( \case
--         TypeObject (_, (fields, _)) -> isEnum fields
--     )
--     typeObj

isHttpInstanceDerived :: [TypeObject] -> Bool
isHttpInstanceDerived typeObj =
  any (\case TypeObject (_, (_, derive)) -> "HttpInstance" `elem` derive) typeObj

isListInstanceDerived :: [TypeObject] -> String -> Bool
isListInstanceDerived typeObj tpName =
  any (\case TypeObject (nm, (_, derive)) -> (tpName == nm) && "'ListInstance" `elem` derive) typeObj

isJsonInstanceDerived :: [TypeObject] -> String -> Bool
isJsonInstanceDerived typeObj tpName =
  any (\case TypeObject (nm, (_, derive)) -> (tpName == nm) && "'JsonInstance" `elem` derive) typeObj

isEnum :: [(String, String)] -> Bool
isEnum [("enum", _)] = True
isEnum _ = False

generateHaskellTypes :: [TypeObject] -> (String, String)
generateHaskellTypes typeObj = (both concat . unzip . map (both L.unlines . processType)) typeObj
  where
    processType :: TypeObject -> ([String], [String])
    processType (TypeObject (typeName, (fields, _)))
      | isEnum fields = generateEnum typeName fields
      | otherwise = generateDataStructure typeName fields

    generateEnum :: String -> [(String, String)] -> ([String], [String])
    generateEnum typeName [("enum", values)] =
      let enumValues = L.splitOn "," values
       in ( ("data " <> typeName <> " = " <> L.intercalate " | " enumValues) :
            ["  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema" <> addRestDerivations (concatMap (\(TypeObject (tname, (_, d))) -> if tname == typeName then d else []) typeObj) <> ")\n\n"],
            ("$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''" <> typeName <> ")\n\n") :
            ( ["$(mkHttpInstancesForEnum ''" <> typeName <> ")\n" | isHttpInstanceDerived typeObj]
                ++ ["$(Tools.Beam.UtilsTH.mkBeamInstancesForJson ''" <> typeName <> ")\n" | isJsonInstanceDerived typeObj typeName]
            )
          )
    generateEnum _ _ = error "Invalid enum definition"

    addRestDerivations :: [String] -> String
    addRestDerivations [] = ""
    addRestDerivations derivations = if length derives > 0 then ", " <> derives else ""
      where
        derives = L.intercalate ", " (map toInstanceName $ filter (\x -> not $ L.isPrefixOf "'" x) derivations)

    toInstanceName = \case
      "HttpInstance" -> "ToParamSchema"
      val -> val

    generateDataStructure :: String -> [(String, String)] -> ([String], [String])
    generateDataStructure typeName fields =
      ( ["data " <> typeName <> " = " <> typeName]
          ++ ["  { " <> L.intercalate ",\n    " (map formatField fields) <> "\n  }"]
          ++ ["  deriving (Generic, Show, ToJSON, FromJSON, ToSchema" <> addRestDerivations (concatMap (\(TypeObject (tname, (_, d))) -> if tname == typeName then d else []) typeObj) <> ")\n"],
        ["$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''" <> typeName <> ")\n" | isListInstanceDerived typeObj typeName]
          ++ ["$(Tools.Beam.UtilsTH.mkBeamInstancesForJson ''" <> typeName <> ")\n" | isJsonInstanceDerived typeObj typeName]
      )

    formatField :: (String, String) -> String
    formatField (fieldName, fieldType) = fieldName ++ " :: " ++ fieldType
