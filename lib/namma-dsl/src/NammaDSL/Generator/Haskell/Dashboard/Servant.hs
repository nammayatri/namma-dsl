module NammaDSL.Generator.Haskell.Dashboard.Servant (generateServantAPIDashboard) where

import Control.Lens ((^.))
import Control.Monad (forM_)
import Control.Monad.Reader (ask)
import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, isJust, maybeToList)
import qualified Data.Text as T
import NammaDSL.Config (ApiKind (..), DefaultImports (..), GenerationType (SERVANT_API_DASHBOARD))
import NammaDSL.DSL.Syntax.API
import NammaDSL.Generator.Haskell.Common
import NammaDSL.GeneratorCore
import NammaDSL.Lib hiding (Q, Writer)
import qualified NammaDSL.Lib.TH as TH
import qualified NammaDSL.Lib.Types as TH
import NammaDSL.Utils
import Prelude

type Writer w = TH.Writer Apis w

type Q w = TH.Q Apis w

generateServantAPIDashboard :: DefaultImports -> ApiRead -> Apis -> Code
generateServantAPIDashboard (DefaultImports qualifiedImp simpleImp _packageImports _) apiRead input =
  generateCode generatorInput
  where
    generationType = SERVANT_API_DASHBOARD
    codeBody' = generateCodeBody (mkCodeBody generationType apiRead) input
    servantApiDashboardModulePrefix = apiServantDashboardImportPrefix apiRead ++ "."
    domainHandlerDashboardModulePrefix = apiDomainHandlerDashboardImportPrefix apiRead ++ "."
    packageOverride :: [String] -> [String]
    packageOverride = checkForPackageOverrides (input ^. importPackageOverrides)

    generatorInput :: GeneratorInput
    generatorInput =
      GeneratorInput
        { _ghcOptions = ["-Wno-orphans", "-Wno-unused-imports"],
          _extensions = [],
          _moduleNm = servantApiDashboardModulePrefix <> T.unpack (_moduleName input),
          _moduleExports = Just ["API", "handler"],
          _simpleImports = packageOverride allSimpleImports,
          _qualifiedImports = packageOverride $ removeUnusedQualifiedImports codeBody' allQualifiedImports,
          _packageImports,
          _codeBody = codeBody'
        }

    allQualifiedImports :: [String]
    allQualifiedImports =
      [ domainHandlerDashboardModulePrefix
          <> T.unpack (_moduleName input)
      ]
        <> nub (qualifiedImp <> figureOutImports (T.unpack <$> concatMap handlerSignature (_apis input)) <> apiTypesImport)
        <> ["Domain.Types.MerchantOperatingCity" | ifProviderPlatform]
        <> when_ ifTransactionStore ["Dashboard.Common" #. T.unpack (input ^. moduleName), "Domain.Types.Transaction"]
        <> ["Kernel.Utils.Validation" | ifValidationRequired]
        <> [apiTypesImportPrefix apiRead]

    allSimpleImports :: [String]
    allSimpleImports =
      ["Storage.Beam.SystemConfigs ()" | ifNotDashboard]
        <> ["Tools.Auth.Webhook" | ifSafetyDashboard]
        <> simpleImp

    apiTypesImport :: [String]
    apiTypesImport = [apiTypesImportPrefix apiRead #. T.unpack (_moduleName input)] -- we need API type from this module
    ifNotDashboard :: Bool
    ifNotDashboard =
      any
        ( \authType' -> do
            case authType' of
              Just (DashboardAuth _) -> False
              Just (SafetyWebhookAuth _) -> False
              Just (ApiAuth {}) -> False
              Just (ApiAuthV2 {}) -> False
              Just NoAuth | apiReadKind apiRead == DASHBOARD -> False
              _ -> True
        )
        (map _authType $ _apis input)

    ifSafetyDashboard :: Bool
    ifSafetyDashboard =
      any
        ( \authType' -> do
            case authType' of
              Just (SafetyWebhookAuth _) -> True
              _ -> False
        )
        (map _authType $ _apis input)

    ifProviderPlatform :: Bool
    ifProviderPlatform =
      any
        ( \authType' -> do
            case authType' of
              Just (TokenAuth PROVIDER_TYPE) -> True
              _ -> False
        )
        (map _authType $ _apis input)

    ifTransactionStore :: Bool
    ifTransactionStore = any (\apiT -> apiT ^. apiType /= GET) $ input ^. apis

    ifValidationRequired :: Bool
    ifValidationRequired = any (\apiT -> isJust $ apiT ^. requestValidation) $ input ^. apis

when_ :: Bool -> [a] -> [a]
when_ False _ = []
when_ True as = as

mkCodeBody :: GenerationType -> ApiRead -> ApisM ()
mkCodeBody generationType apiRead = do
  input <- ask
  let allApis = input ^. apis
  tellM . fromMaybe mempty $
    interpreter input $ do
      generateAPIType SERVANT_API_DASHBOARD apiRead
      generateAPIHandler apiRead
      forM_ allApis $ generateServantApiType generationType apiRead
      forM_ allApis $ handlerFunctionDef generationType apiRead

generateAPIHandler :: ApiRead -> Writer CodeUnit
generateAPIHandler apiRead = do
  input <- ask
  let allApis = _apis input

  TH.decsW $ do
    sigDW "handler" mkSign
    funDW "handler" $
      TH.clauseW mkPat $
        TH.normalB $
          appendInfixE (vE ":<|>") (NE.fromList $ mkExp <$> allApis)
  where
    mkSign = do
      let defSignature = cT "Environment.FlowServer" ~~ cT "API"
      case apiReadKind apiRead of
        UI -> defSignature
        DASHBOARD -> _ShortId ~~ _Merchant --> cT "Kernel.Types.Beckn.Context.City" --> defSignature
    mkPat = case apiReadKind apiRead of
      UI -> []
      DASHBOARD -> [vP "merchantId", vP "city"]
    mkExp api = do
      let defExp = vE (T.unpack $ handlerFunctionText api)
      case apiReadKind apiRead of
        UI -> defExp
        DASHBOARD -> defExp ~* vE "merchantId" ~* vE "city"

generateServantApiType :: GenerationType -> ApiRead -> ApiTT -> Writer CodeUnit
generateServantApiType generationType apiRead apiTT = do
  input <- ask
  let moduleName' = input ^. moduleName
  tySynDW (TH.mkNameT $ mkApiName apiTT) [] $ do
    TH.appendInfixT ":>" . NE.fromList $
      maybeToList (addAuthToApi apiRead generationType apiTT)
        <> [cT (((apiTypesImportPrefix apiRead <> "." <> T.unpack moduleName' <> ".") <>) . T.unpack . mkApiName $ apiTT)]

handlerFunctionDef :: GenerationType -> ApiRead -> ApiTT -> Writer CodeUnit
handlerFunctionDef generationType apiRead apiT = do
  input <- ask
  useAuth <- case (apiT ^. authType) of
    Just ApiAuthV2 {} -> pure True
    Just NoAuth -> pure False
    _ -> error "Please use ApiAuthV2, or NoAuth in case of auth not required for dashboard api"
  let moduleName' = input ^. moduleName
  let functionName = handlerFunctionText apiT
      signatureUnits = mkApiSignatureUnits apiT
      allTypes = map apiSignatureType signatureUnits
      apiUnits = map apiSignatureUnit signatureUnits
      showType = cT . T.unpack <$> filter (/= T.empty) (init allTypes)
      handlerTypes = apiAuthTypeMapperServant generationType apiT <> showType <> [cT "Environment.FlowHandler" ~~ cT (T.unpack $ last allTypes)]
  TH.decsW $ do
    TH.sigDW (TH.mkNameT functionName) $ do
      TH.forallT [] [] $
        TH.appendArrow $ NE.fromList handlerTypes
    TH.funDW (TH.mkNameT functionName) $ do
      let pats = vP "merchantShortId" : vP "opCity" : [vP "apiTokenInfo" | useAuth] <> generateParamsPat apiUnits
      TH.clauseW pats $
        TH.normalB $
          generateWithFlowHandlerAPI $ do
            let defExp = apiDomainHandlerDashboardImportPrefix apiRead #. T.unpack moduleName' #. T.unpack (handlerFunctionText apiT)
            TH.appendE $ vE defExp NE.:| vE "merchantShortId" : vE "opCity" : [vE "apiTokenInfo" | useAuth] <> generateParamsExp apiUnits

generateWithFlowHandlerAPI :: Q TH.Exp -> Q TH.Exp
generateWithFlowHandlerAPI = (vE "withFlowHandlerAPI'" ~$)
