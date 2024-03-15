module NammaDSL.Generator.Haskell.ApiTypes where

import Control.Lens ((^.))
import Control.Monad.Reader (ask)
import Data.Foldable
import Data.Functor ((<&>))
import Data.List (isInfixOf, nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import NammaDSL.Config (DefaultImports (..))
import NammaDSL.DSL.Syntax.API
import NammaDSL.DSL.Syntax.Common
import NammaDSL.Generator.Haskell.Common (checkForPackageOverrides)
import NammaDSL.GeneratorCore
import NammaDSL.Lib hiding (Q, Writer)
import qualified NammaDSL.Lib.TH as TH
import qualified NammaDSL.Lib.Types as TH
import NammaDSL.Utils (removeUnusedQualifiedImports)
import Prelude

type Writer w = TH.Writer Apis w

type Q w = TH.Q Apis w

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
          _qualifiedImports = packageOverride $ removeUnusedQualifiedImports codeBody' allQualifiedImports,
          _codeBody = codeBody'
        }
    codeBody' = generateCodeBody mkCodeBody input
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
  tellM . fromMaybe mempty $
    interpreter input $ do
      generateHaskellTypes (input ^. apiTypes . types)

generateHaskellTypes :: [TypeObject] -> Writer CodeUnit
generateHaskellTypes typeObj = traverse_ processType typeObj
  where
    processType :: TypeObject -> Writer CodeUnit
    processType (TypeObject recType (typeName, (fields, _)))
      | isEnum fields = generateEnum recType typeName fields
      | otherwise = generateDataStructure recType typeName fields

    isEnum :: [(Text, Text)] -> Bool
    isEnum [("enum", _)] = True
    isEnum _ = False

    generateEnum :: RecordType -> Text -> [(Text, Text)] -> Writer CodeUnit
    generateEnum recType typeName [("enum", values)] = do
      let enumValues = T.splitOn "," values -- TODO move to parsing
      let _thTypeName = vE $ "''" <> T.unpack typeName
      TH.decW . pure $ do
        let restDerivations = addRestDerivations (concatMap (\(TypeObject _ (tname, (_, d))) -> if tname == typeName then d else []) typeObj)
        let derives = TH.DerivClause Nothing (TH.ConT <$> ["Eq", "Show", "Generic", "ToJSON", "FromJSON", "ToSchema"] <> restDerivations)
        case recType of
          NewType -> error "Generate haskell domain types: expected Data but got NewType" -- can be newtype here?
          Data -> TH.DataD [] (mkNameT typeName) [] Nothing (enumValues <&> (\enumValue -> TH.NormalC (mkNameT enumValue) [])) [derives]
          Type -> error "Generate haskell domain types: expected Data but got Type"
    generateEnum _ _ _ = error "Invalid enum definition"

    addRestDerivations :: [Text] -> [TH.Name]
    addRestDerivations derivations = map mkNameT $ filter (\x -> not $ T.isPrefixOf "'" x) $ derivations

    generateDataStructure :: RecordType -> Text -> [(Text, Text)] -> Writer CodeUnit
    generateDataStructure recType typeName fields =
      TH.decW . pure $ do
        let restDerivations = addRestDerivations (concatMap (\(TypeObject _ (tname, (_, d))) -> if tname == typeName then d else []) typeObj)
        let derives = TH.DerivClause Nothing (TH.ConT <$> ["Generic", "ToJSON", "FromJSON", "ToSchema"] <> restDerivations)

        let thTypeName = mkNameT typeName
        case recType of
          NewType -> do
            let (f, t) = case fields of
                  [field] -> field
                  _ -> error $ "Generate data structure: expected exactly one record for NewType but got: " <> show fields

            TH.NewtypeD [] thTypeName [] Nothing (TH.RecC thTypeName [(mkNameT f, defaultBang, TH.ConT $ mkNameT t)]) [derives]
          Data -> TH.DataD [] thTypeName [] Nothing [TH.RecC thTypeName (fields <&> \(f, t) -> (mkNameT f, defaultBang, TH.ConT $ mkNameT t))] [derives]
          Type -> error "Generate data structure: expected Data but got Type"
