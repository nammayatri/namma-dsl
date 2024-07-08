module NammaDSL.Generator.Haskell.ApiTypes (generateApiTypes) where

import Control.Lens ((^.))
import Control.Monad (when)
import Control.Monad.Reader (ask)
import Data.Foldable
import Data.Functor ((<&>))
import Data.List (isInfixOf, nub)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import NammaDSL.Config (ApiKind (DASHBOARD), DefaultImports (..), GenerationType (API_TYPES))
import NammaDSL.DSL.Syntax.API
import NammaDSL.DSL.Syntax.Common
import NammaDSL.Generator.Haskell.Common (apiTTToText, checkForPackageOverrides, generateAPIType, handlerFunctionText, handlerSignature, mkApiName)
import NammaDSL.GeneratorCore
import NammaDSL.Lib hiding (Q, Writer)
import qualified NammaDSL.Lib.TH as TH
import qualified NammaDSL.Lib.Types as TH
import NammaDSL.Utils
import Prelude

type Writer w = TH.Writer Apis w

-- type Q w = TH.Q Apis w

generateApiTypes :: DefaultImports -> ApiRead -> Apis -> Code
generateApiTypes (DefaultImports qualifiedImp simpleImp _packageImports _) apiRead input = generateCode generatorInput
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
          _moduleExports = Nothing,
          _simpleImports = packageOverride simpleImp,
          _qualifiedImports = packageOverride $ removeUnusedQualifiedImports codeBody' allQualifiedImports,
          _packageImports,
          _codeBody = codeBody'
        }
    codeBody' = generateCodeBody (mkCodeBody apiRead) input
    qualifiedModuleName = T.unpack ((T.pack apiTypesModulePrefix) <> _moduleName input)

    allQualifiedImports :: [String]
    allQualifiedImports =
      nub $
        preventSameModuleImports $
          (T.unpack <$> input ^. apiTypes . typeImports)
            <> (if apiReadKind apiRead == DASHBOARD then figureOutImports (T.unpack <$> concatMap handlerSignature (_apis input)) else [])
            <> qualifiedImp

    preventSameModuleImports :: [String] -> [String]
    preventSameModuleImports = filter (\x -> not (qualifiedModuleName `isInfixOf` x))

mkCodeBody :: ApiRead -> ApisM ()
mkCodeBody apiRead = do
  input <- ask
  tellM . fromMaybe mempty $
    interpreter input $ do
      generateHaskellTypes (input ^. apiTypes . types)
      when (apiReadKind apiRead == DASHBOARD) $ do
        generateAPIType API_TYPES apiRead
        generateServantApiType `mapM_` (input ^. apis)
        generateClientsType
        generateMkClientsType

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
          Type -> case fields of -- FIXME refactor this in more type safe way
            [("type", t)] -> TH.TySynD (mkNameT typeName) [] (TH.ConT $ mkNameT t)
            _ -> error "Generate data structure: Type synonym definition should contain single \"type\" field"

-- used in dashboard common apis
generateServantApiType :: ApiTT -> Writer CodeUnit
generateServantApiType api = do
  tySynDW (TH.mkNameT $ mkApiName api) [] $ do
    apiTTToText API_TYPES api

generateClientsType :: Writer CodeUnit
generateClientsType = do
  input <- ask
  let name = TH.mkNameT (input ^. moduleName) <> "APIs"
  case input ^. apis of
    [] -> pure ()
    apiTTs -> do
      TH.dataOrNewtypeDW
        name
        []
        do
          recCW name $ do
            forM_ apiTTs $ \apiT -> do
              let allTypes = handlerSignature apiT
                  showType = cT . T.unpack <$> filter (/= T.empty) (init allTypes)
                  handlerName = TH.mkNameT $ handlerFunctionText apiT
                  handlerType = showType <> [cT "EulerHS.Types.EulerClient" ~~ cT (T.unpack $ last allTypes)]
              TH.fieldDecW handlerName $ TH.appendArrow $ NE.fromList handlerType
        (pure ())

generateMkClientsType :: Writer CodeUnit
generateMkClientsType = do
  input <- ask
  let modName = input ^. moduleName
  let funcName = TH.mkNameT $ "mk" <> modName <> "APIs"
  let typeName = T.unpack modName <> "APIs"
  let clientFuncName = T.unpack (headToLower modName) <> "Client"
  case input ^. apis of
    [] -> pure ()
    (apiTT : apiTTs) -> do
      TH.decsW $ do
        TH.sigDW funcName $
          cT "Client" ~~ cT "EulerHS.Types.EulerClient" ~~ cT "API" --> cT typeName
        TH.funDW funcName $
          TH.clauseWhereW
            [vP clientFuncName]
            (TH.normalB $ wildRecordsE typeName)
            do
              let handlerPats = vP . T.unpack . handlerFunctionText <$> (apiTT NE.:| apiTTs)
              valDW (appendInfixP ":<|>" handlerPats) (normalB $ vE clientFuncName)
