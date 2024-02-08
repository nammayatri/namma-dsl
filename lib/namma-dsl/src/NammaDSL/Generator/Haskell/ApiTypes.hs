module NammaDSL.Generator.Haskell.ApiTypes where

import Control.Lens ((^.))
import Data.List (isInfixOf, nub)
import qualified Data.Text as T
import Kernel.Prelude hiding (replicateM)
import NammaDSL.DSL.Syntax.API
import NammaDSL.Generator.Haskell.Common (checkForPackageOverrides)
import NammaDSL.GeneratorCore

generateApiTypes :: Apis -> Code
generateApiTypes input = generateCode generatorInput
  where
    packageOverride :: [String] -> [String]
    packageOverride = checkForPackageOverrides (input ^. importPackageOverrides)

    generatorInput :: GeneratorInput
    generatorInput =
      GeneratorInput
        { _ghcOptions = ["-Wno-orphans", "-Wno-unused-imports"],
          _extensions = [],
          _moduleNm = "API.Types.UI." <> T.unpack (_moduleName input),
          _simpleImports = packageOverride allSimpleImports,
          _qualifiedImports = packageOverride allQualifiedImports,
          _codeBody = generateCodeBody mkCodeBody input
        }
    qualifiedModuleName = T.unpack ("API.Types.UI." <> _moduleName input)

    allSimpleImports :: [String]
    allSimpleImports =
      [ "EulerHS.Prelude hiding (id)",
        "Servant",
        "Tools.Auth",
        "Data.OpenApi (ToSchema)"
      ]

    allQualifiedImports :: [String]
    allQualifiedImports =
      nub $
        preventSameModuleImports $
          (T.unpack <$> input ^. apiTypes . typeImports)
            <> defaultQualifiedImport

    preventSameModuleImports :: [String] -> [String]
    preventSameModuleImports = filter (\x -> not (qualifiedModuleName `isInfixOf` x))

    defaultQualifiedImport :: [String]
    defaultQualifiedImport = ["Kernel.Prelude", "Domain.Types.Person", "Domain.Types.Merchant", "Environment", "Kernel.Types.Id"]

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
    processType (TypeObject (typeName, (fields, _)))
      | isEnum fields = generateEnum typeName fields
      | otherwise = generateDataStructure typeName fields

    isEnum :: [(Text, Text)] -> Bool
    isEnum [("enum", _)] = True
    isEnum _ = False

    generateEnum :: Text -> [(Text, Text)] -> [Text]
    generateEnum typeName [("enum", values)] =
      let enumValues = T.splitOn "," values
       in ("data " <> typeName <> " = " <> T.intercalate " | " enumValues) :
          ["  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema" <> addRestDerivations (concatMap (\(TypeObject (tname, (_, d))) -> if tname == typeName then d else []) typeObj) <> ")\n"]
    generateEnum _ _ = error "Invalid enum definition"

    addRestDerivations :: [Text] -> Text
    addRestDerivations [] = ""
    addRestDerivations derivations = if T.length derives > 0 then ", " <> derives else ""
      where
        derives = T.intercalate ", " (filter (\x -> not $ T.isPrefixOf "'" x) derivations)

    generateDataStructure :: Text -> [(Text, Text)] -> [Text]
    generateDataStructure typeName fields =
      ["data " <> typeName <> " = " <> typeName]
        ++ ["  { " <> T.intercalate ",\n    " (map formatField fields) <> "\n  }"]
        ++ ["  deriving (Generic, ToJSON, FromJSON, ToSchema" <> addRestDerivations (concatMap (\(TypeObject (tname, (_, d))) -> if tname == typeName then d else []) typeObj) <> ")\n"]

    formatField :: (Text, Text) -> Text
    formatField (fieldName, fieldType) = fieldName <> " :: " <> fieldType
