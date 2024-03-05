module NammaDSL.Generator.Haskell.ApiTypes where

import Control.Lens ((^.))
import Control.Monad.Reader (ask)
import Data.List (isInfixOf, nub)
import Data.Text (Text)
import qualified Data.Text as T
import NammaDSL.Config (DefaultImports (..))
import NammaDSL.DSL.Syntax.API
import NammaDSL.DSL.Syntax.Common
import NammaDSL.Generator.Haskell.Common (checkForPackageOverrides, getRecordType)
import NammaDSL.GeneratorCore
import Prelude

generateApiTypes :: DefaultImports -> ApiRead -> Apis -> Code
generateApiTypes (DefaultImports qualifiedImp simpleImp _) apiRead input = generateCode generatorInput
  where
    apiTypesModulePrefix = apiTypesImportPrefix apiRead ++ "."
    packageOverride :: [String] -> [String]
    packageOverride = checkForPackageOverrides (input ^. importPackageOverrides)

    generatorInput :: GeneratorInput
    generatorInput =
      GeneratorInput
        { _ghcOptions = ["-Wno-orphans", "-Wno-unused-imports"],
          _extensions = [],
          _moduleNm = apiTypesModulePrefix <> T.unpack (_moduleName input),
          _simpleImports = packageOverride simpleImp,
          _qualifiedImports = packageOverride allQualifiedImports,
          _codeBody = generateCodeBody mkCodeBody input
        }
    qualifiedModuleName = T.unpack ((T.pack apiTypesModulePrefix) <> _moduleName input)

    allQualifiedImports :: [String]
    allQualifiedImports =
      nub $
        preventSameModuleImports $
          (T.unpack <$> input ^. apiTypes . typeImports)
            <> qualifiedImp

    preventSameModuleImports :: [String] -> [String]
    preventSameModuleImports = filter (\x -> not (qualifiedModuleName `isInfixOf` x))

mkCodeBody :: ApisM ()
mkCodeBody = do
  input <- ask
  generateHaskellTypes (input ^. apiTypes . types)

generateHaskellTypes :: [TypeObject] -> ApisM ()
generateHaskellTypes typeObj =
  onNewLine $
    tellM $
      T.unpack $
        T.unlines (concatMap processType typeObj)
  where
    processType :: TypeObject -> [Text]
    processType (TypeObject rectype (typeName, (fields, _)))
      | isEnum fields = generateEnum rectype typeName fields
      | otherwise = generateDataStructure rectype typeName fields

    isEnum :: [(Text, Text)] -> Bool
    isEnum [("enum", _)] = True
    isEnum _ = False

    generateEnum :: RecordType -> Text -> [(Text, Text)] -> [Text]
    generateEnum recType typeName [("enum", values)] =
      let enumValues = T.splitOn "," values
       in ((T.pack $ getRecordType recType) <> " " <> typeName <> " = " <> T.intercalate " | " enumValues) :
          ["  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema" <> addRestDerivations (concatMap (\(TypeObject _ (tname, (_, d))) -> if tname == typeName then d else []) typeObj) <> ")\n"]
    generateEnum _ _ _ = error "Invalid enum definition"

    addRestDerivations :: [Text] -> Text
    addRestDerivations [] = ""
    addRestDerivations derivations = if T.length derives > 0 then ", " <> derives else ""
      where
        derives = T.intercalate ", " (filter (\x -> not $ T.isPrefixOf "'" x) derivations)

    generateDataStructure :: RecordType -> Text -> [(Text, Text)] -> [Text]
    generateDataStructure recType typeName fields =
      [(T.pack $ getRecordType recType) <> " " <> typeName <> " = " <> typeName]
        ++ ["  { " <> T.intercalate ",\n    " (map formatField fields) <> "\n  }"]
        ++ ["  deriving (Generic, ToJSON, FromJSON, ToSchema" <> addRestDerivations (concatMap (\(TypeObject _ (tname, (_, d))) -> if tname == typeName then d else []) typeObj) <> ")\n"]

    formatField :: (Text, Text) -> Text
    formatField (fieldName, fieldType) = fieldName <> " :: " <> fieldType
