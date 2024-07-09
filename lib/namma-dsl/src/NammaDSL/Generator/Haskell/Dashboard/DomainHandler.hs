module NammaDSL.Generator.Haskell.Dashboard.DomainHandler (mkCodeBodyDomainHandlerDashboard, generateDomainHandlerDashboard) where

import Control.Lens ((^.))
import Control.Monad (forM_)
import Control.Monad.Extra (whenJust)
import Control.Monad.Reader (ask)
import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import NammaDSL.Config (DefaultImports (..), GenerationType (DOMAIN_HANDLER_DASHBOARD))
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

generateDomainHandlerDashboard :: DefaultImports -> ApiRead -> Apis -> Code
generateDomainHandlerDashboard (DefaultImports qualifiedImp simpleImp _packageImports _) apiRead input =
  generateCode generatorInput
  where
    clientFuncName = getClientFunctionName apiRead
    codeBody' = generateCodeBody (mkCodeBody apiRead) input
    domainHandlerDashboardModulePrefix = apiDomainHandlerDashboardImportPrefix apiRead ++ "."
    domainHandlerModulePrefix = apiDomainHandlerImportPrefix apiRead ++ "."
    packageOverride :: [String] -> [String]
    packageOverride = checkForPackageOverrides (input ^. importPackageOverrides)

    generatorInput :: GeneratorInput
    generatorInput =
      GeneratorInput
        { _ghcOptions = ["-Wno-orphans", "-Wno-unused-imports"],
          _extensions = [],
          _moduleNm = domainHandlerDashboardModulePrefix <> T.unpack (_moduleName input),
          _moduleExports = Just $ T.unpack . handlerFunctionText <$> input ^. apis,
          _simpleImports = packageOverride allSimpleImports,
          _qualifiedImports = packageOverride $ removeUnusedQualifiedImports codeBody' allQualifiedImports,
          _packageImports,
          _codeBody = codeBody'
        }

    allQualifiedImports :: [String]
    allQualifiedImports =
      [ domainHandlerModulePrefix
          <> T.unpack (_moduleName input)
          <> " as "
          <> domainHandlerModulePrefix
          <> T.unpack (_moduleName input)
      ]
        <> nub (qualifiedImp <> figureOutImports (T.unpack <$> concatMap handlerSignature (_apis input)))
        <> ["Domain.Types.MerchantOperatingCity" | ifProviderPlatform]
        <> when_ ifTransactionStore ["Dashboard.Common" #. T.unpack (input ^. moduleName), "Domain.Types.Transaction"]
        <> ["Kernel.Utils.Validation" | ifValidationRequired]
        <> [extraApiTypesImportPrefix apiRead <> "." <> T.unpack (input ^. moduleName) | EXTRA_API_TYPES_FILE `elem` input ^. extraOperations]
        <> [getClientModuleName clientFuncName]
    allSimpleImports :: [String]
    allSimpleImports =
      ["Storage.Beam.SystemConfigs ()" | ifNotDashboard]
        <> ["Tools.Auth.Webhook" | ifSafetyDashboard]
        <> simpleImp

    ifNotDashboard :: Bool
    ifNotDashboard =
      any
        ( \authType' -> do
            case authType' of
              Just (DashboardAuth _) -> False
              Just (SafetyWebhookAuth _) -> False
              Just (ApiAuth {}) -> False
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

getClientFunctionName :: ApiRead -> String
getClientFunctionName apiRead = do
  fromMaybe (error "clientFunction should be provided for dashboard api") $ apiClientFunction apiRead

getClientModuleName :: String -> String
getClientModuleName = fromMaybe (error "Client function name should contain module name") . figureOutImport

mkCodeBody :: ApiRead -> ApisM ()
mkCodeBody apiRead = do
  input <- ask
  tellM . fromMaybe mempty $
    mkCodeBodyDomainHandlerDashboard apiRead input

mkCodeBodyDomainHandlerDashboard :: ApiRead -> Apis -> Maybe String
mkCodeBodyDomainHandlerDashboard apiRead input = do
  let clientFuncName = getClientFunctionName apiRead
  let allApis = input ^. apis
  interpreter input $ do
    forM_ allApis $ handlerFunctionDef clientFuncName

handlerFunctionDef :: String -> ApiTT -> Writer CodeUnit
handlerFunctionDef clientFuncName apiT = do
  input <- ask
  let moduleName' = input ^. moduleName
  let functionName = handlerFunctionText apiT
      signatureUnits = mkApiSignatureUnits apiT
      allTypes = map apiSignatureType signatureUnits
      apiUnits = map apiSignatureUnit signatureUnits
      showType = cT . T.unpack <$> filter (/= T.empty) (init allTypes)
      handlerTypes = apiAuthTypeMapperServant DOMAIN_HANDLER_DASHBOARD apiT <> showType <> [cT "Environment.Flow" ~~ cT (T.unpack $ last allTypes)]
  TH.decsW $ do
    TH.sigDW (TH.mkNameT functionName) $ do
      TH.forallT [] [] $
        TH.appendArrow $ NE.fromList handlerTypes
    TH.funDW (TH.mkNameT functionName) $ do
      let pats = vP "merchantShortId" : vP "opCity" : vP "apiTokenInfo" : generateParamsPat apiUnits
      TH.clauseW pats $
        TH.normalB $
          TH.doEW $ do
            whenJust (apiT ^. requestValidation) $ \validationFunc -> do
              let reqParam = case findRequest apiUnits of
                    Just paramText -> vE paramText
                    Nothing -> error "Did not found request for validation"
              TH.noBindSW $ vE "Kernel.Utils.Validation.runRequestValidation" ~* vE (T.unpack validationFunc) ~* reqParam
            vP "checkedMerchantId" <-- vE "merchantCityAccessCheck" ~* vE "merchantShortId" ~* vE "apiTokenInfo.merchant.shortId" ~* vE "opCity" ~* vE "apiTokenInfo.city"
            let transactionWrapper clientCall = case apiT ^. apiType of
                  GET -> clientCall
                  _ -> do
                    let apiName = "Domain.Types.Transaction" #. T.unpack moduleName' <> "API"
                    let endpointName = "Dashboard.Common" #. T.unpack moduleName' #. (T.unpack (mkApiName apiT) <> "Endpoint")
                    vP "transaction"
                      <-- vE "SharedLogic.Transaction.buildTransaction"
                      ~* (cE apiName ~* cE endpointName)
                      ~* (cE "Kernel.Prelude.Just" ~* mkServerName (apiT ^. authType))
                      ~* (cE "Kernel.Prelude.Just" ~* vE "apiTokenInfo")
                      ~* generateHandlerParam apiUnits (CaptureUnit "driverId")
                      ~* generateHandlerParam apiUnits (CaptureUnit "rideId")
                      ~* generateReqParam apiUnits
                    TH.noBindSW $ vE "SharedLogic.Transaction.withTransactionStoring" ~* vE "transaction" ~$ TH.doEW clientCall
            transactionWrapper $
              TH.noBindSW $
                TH.appendE $
                  vE clientFuncName
                    NE.:| vE "checkedMerchantId" :
                  vE "opCity" :
                  vE ("." <> T.unpack (headToLower moduleName') <> "DSL" #. T.unpack functionName) :
                  generateParamsExp apiUnits
  where
    mkServerName :: Maybe AuthType -> Q TH.Exp
    mkServerName (Just (ApiAuth serverName _ _)) = cE serverName.getServerName
    mkServerName _ = error "ApiAuth expected for dashboard api"

    generateHandlerParam :: [ApiUnit] -> ApiUnit -> Q TH.Exp
    generateHandlerParam apiUnits param = case findParamText apiUnits param of
      Just paramText -> cE "Kernel.Prelude.Just" ~* vE paramText
      Nothing -> cE "Kernel.Prelude.Nothing"

    generateReqParam :: [ApiUnit] -> Q TH.Exp
    generateReqParam apiUnits = case findRequest apiUnits of
      Just paramText -> cE "Kernel.Prelude.Just" ~* vE paramText
      Nothing -> cE "Kernel.Prelude.Nothing"
