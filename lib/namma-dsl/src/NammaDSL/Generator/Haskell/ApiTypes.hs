module NammaDSL.Generator.Haskell.ApiTypes (generateApiTypes) where

import Control.Lens ((^.))
import Control.Monad (when)
import Control.Monad.Extra (whenJust)
import Control.Monad.Reader (ask)
import Data.Bool (bool)
import Data.Foldable
import Data.Functor ((<&>))
import Data.List (isInfixOf, nub)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import NammaDSL.Config (ApiKind (..), DefaultImports (..), GenerationType (API_TYPES))
import NammaDSL.DSL.Syntax.API
import NammaDSL.DSL.Syntax.Common
import NammaDSL.Generator.Haskell.Common as Common
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
          _extensions = ["StandaloneKindSignatures" | apiReadKind apiRead == DASHBOARD],
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
            <> figureOutImports allHandlersSignatures
            <> qualifiedImp
            <> multipartImports
            <> hideSecretsImports
            <> endpointImports

    allHandlersSignatures :: [String]
    allHandlersSignatures = case apiReadKind apiRead of
      UI -> []
      DASHBOARD -> T.unpack <$> (concatMap handlerSignature (_apis input) <> concatMap handlerSignatureHelper (_apis input))

    preventSameModuleImports :: [String] -> [String]
    preventSameModuleImports = filter (\x -> not (qualifiedModuleName `isInfixOf` x))

    multipartImports :: [String]
    multipartImports = do
      if apiReadKind apiRead == DASHBOARD && any (isJust . (^. apiMultipartType)) (input ^. apis)
        then ["Kernel.ServantMultipart", "Data.ByteString.Lazy"]
        else []

    endpointImports :: [String]
    endpointImports = do
      (["Data.Aeson" | apiReadKind apiRead == DASHBOARD && length (input ^. apis) == 1])

    hideSecretsImports :: [String]
    hideSecretsImports = do
      ["Kernel.Types.HideSecrets" | apiReadKind apiRead == DASHBOARD && any isHideSecretsInstanceDerived (input ^. apiTypes ^. types)]

mkCodeBody :: ApiRead -> ApisM ()
mkCodeBody apiRead = do
  input <- ask
  tellM . fromMaybe mempty $
    interpreter input $ do
      generateHaskellTypes (input ^. apiTypes . types)
      when (apiReadKind apiRead == DASHBOARD) $ do
        generateAPITypeHelper API_TYPES apiRead
        generateServantApiType apiRead `mapM_` (input ^. apis)
        generateClientsType
        generateMkClientsType
        generateUserActionType

isHideSecretsInstanceDerived :: TypeObject -> Bool
isHideSecretsInstanceDerived (TypeObject _ (_, (_, derive)) _) = "'HideSecrets" `elem` derive

getDerivingStrategy :: Text -> TH.DerivStrategy
getDerivingStrategy derive = do
  let stockDerives = ["Eq", "Ord", "Show", "Read", "Enum", "Bounded", "Ix"]
  let stockDerivesWithExtension = ["Functor", "Foldable", "Traversable", "Generic", "Generic1", "Data", "Lift"] -- TODO generate extension also
  if any (\stockDerive -> stockDerive == derive || ("." <> stockDerive) `T.isSuffixOf` derive) $ stockDerives <> stockDerivesWithExtension
    then TH.StockStrategy
    else TH.AnyclassStrategy

generateHaskellTypes :: [TypeObject] -> Writer CodeUnit
generateHaskellTypes = traverse_ processType
  where
    processType :: TypeObject -> Writer CodeUnit
    processType typeObj@(TypeObject _ (_, (fields, _)) _) = case fields of
      [("enum", values)] -> generateEnum typeObj values
      _ -> generateDataStructure typeObj

    generateEnum :: TypeObject -> Text -> Writer CodeUnit
    generateEnum typeObj@(TypeObject recType (typeName, _) overrideDefaultDerive) values = do
      let enumValues = T.splitOn "," values -- TODO move to parsing
      let _thTypeName = vE $ "''" <> T.unpack typeName
      TH.decW . pure $ do
        let enumWithNestedTypes = any ((> 1) . length . T.words) enumValues
        let defaultStockDerives = bool (if enumWithNestedTypes then ["Generic"] else ["Eq", "Show", "Generic"]) [] overrideDefaultDerive
        let stockDerives = mkDerivClause TH.StockStrategy $ defaultStockDerives <> addRestDerivations TH.StockStrategy typeObj
        let defaultAnyclassDerives = bool ["ToJSON", "FromJSON", "ToSchema"] [] overrideDefaultDerive
        let anyclassDerives = mkDerivClause AnyclassStrategy $ defaultAnyclassDerives <> addRestDerivations TH.AnyclassStrategy typeObj
        case recType of
          NewType -> error "Generate haskell domain types: expected Data but got NewType" -- can be newtype here?
          Data -> TH.DataD [] (mkNameT typeName) [] Nothing (enumValues <&> (\enumValue -> TH.NormalC (mkNameT enumValue) [])) $ catMaybes [stockDerives, anyclassDerives]
          Type -> error "Generate haskell domain types: expected Data but got Type"
      generateHideSecretsDefaultInstance typeObj

    mkDerivClause :: TH.DerivStrategy -> [TH.Name] -> Maybe TH.DerivClause
    mkDerivClause _ [] = Nothing
    mkDerivClause strategy names = Just $ TH.DerivClause (Just strategy) (TH.ConT <$> names)

    addRestDerivations :: TH.DerivStrategy -> TypeObject -> [TH.Name]
    addRestDerivations strategy (TypeObject _ (_, (_, derivations)) _) =
      map mkNameT $
        filter ((== strategy) . getDerivingStrategy) $
          filter (not . T.isPrefixOf "'") derivations

    generateDataStructure :: TypeObject -> Writer CodeUnit
    generateDataStructure typeObj@(TypeObject recType (typeName, (fields, _)) overrideDefaultDerive) = do
      TH.decW . pure $ do
        let defaultStockDerives = bool ["Generic"] [] overrideDefaultDerive
        let stockDerives = mkDerivClause TH.StockStrategy $ defaultStockDerives <> addRestDerivations TH.StockStrategy typeObj
        let defaultAnyclassDerives = bool ["ToJSON", "FromJSON", "ToSchema"] [] overrideDefaultDerive
        let anyclassDerives = mkDerivClause TH.AnyclassStrategy $ defaultAnyclassDerives <> addRestDerivations TH.AnyclassStrategy typeObj

        let thTypeName = mkNameT typeName
        case recType of
          NewType -> do
            let (f, t) = case fields of
                  [field] -> field
                  _ -> error $ "Generate data structure: expected exactly one record for NewType but got: " <> show fields

            TH.NewtypeD [] thTypeName [] Nothing (TH.RecC thTypeName [(mkNameT f, defaultBang, TH.ConT $ mkNameT t)]) $ catMaybes [stockDerives, anyclassDerives]
          Data -> TH.DataD [] thTypeName [] Nothing [TH.RecC thTypeName (fields <&> \(f, t) -> (mkNameT f, defaultBang, TH.ConT $ mkNameT t))] $ catMaybes [stockDerives, anyclassDerives]
          Type -> case fields of -- FIXME refactor this in more type safe way
            [("type", t)] -> TH.TySynD (mkNameT typeName) [] (TH.ConT $ mkNameT t)
            _ -> error "Generate data structure: Type synonym definition should contain single \"type\" field"
      generateHideSecretsDefaultInstance typeObj

    generateHideSecretsDefaultInstance :: TypeObject -> Writer CodeUnit
    generateHideSecretsDefaultInstance typeObj@(TypeObject _ (typeName, (_, _)) _) =
      when (isHideSecretsInstanceDerived typeObj) $
        instanceDW emptyContext (cT "Kernel.Types.HideSecrets.HideSecrets" ~~ cT (T.unpack typeName)) $
          TH.funDW "hideSecrets" $
            TH.clauseW [] $ TH.normalB (vE "Kernel.Prelude.identity")

-- used in dashboard common apis
generateServantApiType :: ApiRead -> ApiTT -> Writer CodeUnit
generateServantApiType apiRead api = do
  tySynDW (TH.mkNameT $ mkApiName api) [] $ do
    apiTTToText apiRead API_TYPES api
  whenJust (api ^. apiHelperApi) $ \_ -> do
    tySynDW (TH.mkNameT $ mkApiNameHelper api) [] $ do
      apiTTToTextHelper apiRead API_TYPES api

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
              let allTypes = handlerSignatureClientHelper apiT
                  showType = init allTypes
                  handlerName = TH.mkNameT $ handlerFunctionText apiT
                  handlerType = showType <> [cT "EulerHS.Types.EulerClient" ~~ last allTypes]
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

generateUserActionType :: Writer CodeUnit
generateUserActionType = do
  input <- ask
  let mbSingleApiT = case input ^. apis of
        [] -> error "Spec module should contain at least one API"
        [a] -> Just a
        _ -> Nothing

  let modName = input ^. moduleName
  let userActionTypeName = T.unpack modName <> "UserActionType"
  dataDW
    (TH.mkName userActionTypeName)
    []
    do
      forM_ (input ^. apis) $ \apiT -> do
        normalCW (TH.mkName $ Common.mkUserActionTypeName apiT) []
    do
      derivClauseW (Just TH.StockStrategy) $ ConT <$> ["Show", "Read", "Generic", "Eq", "Ord"]
      derivClauseW (Just TH.AnyclassStrategy) $
        ConT <$> if isNothing mbSingleApiT then ["ToJSON", "FromJSON", "ToSchema"] else ["ToSchema"]

  -- Autogenerated instances for types with single value work not as expected
  whenJust mbSingleApiT $ \singleApiT -> do
    let singleUserActionTypeName = Common.mkUserActionTypeName singleApiT
    instanceDW emptyContext (cT "ToJSON" ~~ cT userActionTypeName) $ do
      funDW "toJSON" $
        clauseW [cP singleUserActionTypeName []] $
          TH.normalB (cE "Data.Aeson.String" ~* strE singleUserActionTypeName)
    instanceDW emptyContext (cT "FromJSON" ~~ cT userActionTypeName) $ do
      funDW "parseJSON" $ do
        clauseW [cP "Data.Aeson.String" [strP singleUserActionTypeName]] $
          TH.normalB (vE "pure" ~* cE singleUserActionTypeName)
        clauseW [wildP] $
          TH.normalB (vE "fail" ~* strE (singleUserActionTypeName <> " expected"))

  spliceW (vE "Data.Singletons.TH.genSingletons" ~* listE [vE $ "''" <> userActionTypeName])
