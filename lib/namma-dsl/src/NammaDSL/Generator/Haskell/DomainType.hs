{-# LANGUAGE QuasiQuotes #-}

module NammaDSL.Generator.Haskell.DomainType where

import qualified Data.List as L
import qualified Data.List.Split as L
import Data.String.Interpolate (i, __i)
import Data.Text (pack)
import Data.Tuple.Extra (both)
import Kernel.Prelude
import NammaDSL.DSL.Syntax.Storage
import NammaDSL.GeneratorCore
import NammaDSL.Utils (isMaybeType)

generateDomainType :: TableDef -> Code
generateDomainType tableDef =
  generateCode generatorInput
  where
    moduleName' = "Domain.Types." ++ tableNameHaskell tableDef

    allSimpleImports :: [String]
    allSimpleImports = createDefaultImports tableDef

    allQualifiedImports :: [String]
    allQualifiedImports = removeDefaultImports allSimpleImports moduleName' (imports tableDef)

    generatorInput :: GeneratorInput
    generatorInput =
      GeneratorInput
        { _ghcOptions = [],
          _extensions = ["ApplicativeDo", "TemplateHaskell"],
          _moduleNm = moduleName',
          _simpleImports = allSimpleImports,
          _qualifiedImports = allQualifiedImports,
          _codeBody = generateCodeBody mkCodeBody tableDef
        }

mkCodeBody :: StorageM ()
mkCodeBody = do
  def <- ask
  -- let seperator = ((*>) "\n") $ "  , "
  tellM $
    [__i|
    data #{tableNameHaskellType def} = #{tableNameHaskell def}
      { #{L.intercalate ",\n    " (map fieldDefToHaskell (fields def))}
      }
      deriving (#{L.intercalate ", " (derivingInstances $ containsEncryptedField def)})
  |]
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
generateEncryptionInstance tableDef = do
  let baseType = tableNameHaskell tableDef
      decryptBaseType = "Decrypted" ++ baseType
      encryptedType = baseType ++ "E 'AsEncrypted"
      decryptedType = baseType ++ "E 'AsUnencrypted"

  onNewLine $
    tellM
      [__i|
    type #{baseType} = #{encryptedType}

    type #{decryptBaseType} = #{decryptedType}

    instance EncryptedItem #{baseType} where
      type Unencrypted #{baseType} = (#{decryptBaseType}, HashSalt)
      encryptItem (#{baseType} {..}, salt) = do
        #{unlines $ catMaybes $ map encryptField (fields tableDef)}
        return #{baseType} {#{L.intercalate ", " $ catMaybes $ map mapFields (fields tableDef)}, ..}
      decryptItem #{baseType} {..} = do
        #{unlines $ catMaybes $ map decryptField (fields tableDef)}
        return (#{baseType} {#{L.intercalate ", " $ catMaybes $ map mapFields (fields tableDef)}, ..}, "")

    instance EncryptedItem' #{baseType} where
      type UnencryptedItem #{baseType} = #{decryptBaseType}
      toUnencrypted a salt = (a, salt)
      fromUnencrypted = fst
  |]
  where
    encryptField :: FieldDef -> Maybe String
    encryptField field =
      if field.isEncrypted
        then
          Just $
            if isMaybeType (field.haskellType)
              then [i|    #{fieldName field}_ <- encryptItem $ (, salt) <$> #{fieldName field}|]
              else [i|    #{fieldName field}_ <- encryptItem $ (#{fieldName field}, salt)|]
        else Nothing

    decryptField :: FieldDef -> Maybe String
    decryptField field =
      if field.isEncrypted
        then
          Just $
            if isMaybeType (field.haskellType)
              then [i|    #{fieldName field}_ <- fmap fst <$> decryptItem #{fieldName field}|]
              else [i|    #{fieldName field}_ <- fst <$> decryptItem #{fieldName field}|]
        else Nothing

    mapFields :: FieldDef -> Maybe String
    mapFields field =
      if field.isEncrypted
        then Just $ [i|#{fieldName field} = #{fieldName field}_|]
        else Nothing

removeDefaultImports :: [String] -> String -> [String] -> [String]
removeDefaultImports defaultImports moduleName = filter ((/=) moduleName) . filter (`notElem` defaultImports)

-- Convert FieldDef to Haskell field
fieldDefToHaskell :: FieldDef -> String
fieldDefToHaskell fieldDef =
  fieldDef.fieldName ++ " :: " ++ fieldDef.haskellType

createDefaultImports :: TableDef -> [String]
createDefaultImports tableDef =
  ["Kernel.Prelude"] <> ["Tools.Beam.UtilsTH" | shouldImportUtilsTH (fromMaybe [] $ types tableDef)]
    <> ["Kernel.Utils.TH" | isHttpInstanceDerived (fromMaybe [] $ types tableDef)]
    <> ["Data.Aeson" | isHttpInstanceDerived (fromMaybe [] $ types tableDef)]
    <> ["Kernel.External.Encryption" | tableDef.containsEncryptedField]

shouldImportUtilsTH :: [TypeObject] -> Bool
shouldImportUtilsTH typeObj =
  any
    ( \case
        TypeObject (_, (fields, _)) -> isEnum fields
    )
    typeObj

isHttpInstanceDerived :: [TypeObject] -> Bool
isHttpInstanceDerived typeObj =
  any (\case TypeObject (_, (_, derive)) -> "HttpInstance" `elem` derive) typeObj

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
       in ( ([__i|data #{typeName} = #{L.intercalate " | " enumValues}|]) :
            [[i|  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema#{addRestDerivations'})\n\n|]],
            [__i|$(mkBeamInstancesForEnum ''#{typeName})|] :
              [[__i|$(mkHttpInstancesForEnum ''#{typeName})|] | isHttpInstanceDerived typeObj]
          )
    generateEnum _ _ = error "Invalid enum definition"

    addRestDerivations' = addRestDerivations (concatMap (\case TypeObject (_, (_, d)) -> d) typeObj)

    addRestDerivations :: [String] -> String
    addRestDerivations [] = ""
    addRestDerivations derivations = [__i|, #{L.intercalate ", " (map toInstanceName derivations)}|]

    toInstanceName :: String -> String
    toInstanceName "HttpInstance" = "ToParamSchema"
    toInstanceName val = error $ pack $ ("Invalid instance derivation specified: " ++ val)

    generateDataStructure :: String -> [(String, String)] -> ([String], [String])
    generateDataStructure typeName fields =
      ( [ [__i|
          data #{typeName} = #{typeName}
            {#{L.intercalate ",\n    " (map formatField fields)}
            }
            deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
        |]
        ],
        []
      )

    formatField :: (String, String) -> String
    formatField (fieldName, fieldType) = fieldName ++ " :: " ++ fieldType
