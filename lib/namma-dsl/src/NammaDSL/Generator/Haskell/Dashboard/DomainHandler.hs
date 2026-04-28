module NammaDSL.Generator.Haskell.Dashboard.DomainHandler (mkCodeBodyDomainHandlerDashboard, generateDomainHandlerDashboard) where

import Control.Lens ((^.))
import Control.Monad (forM_)
import Control.Monad.Extra (whenJust)
import Control.Monad.Reader (ask)
import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, isJust, isNothing, maybeToList)
import qualified Data.Text as T
import NammaDSL.Config (ApiKind (..), DefaultImports (..), GenerationType (DOMAIN_HANDLER_DASHBOARD))
import NammaDSL.DSL.Syntax.API
import NammaDSL.Generator.Haskell.Common as Common
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
    generationType = DOMAIN_HANDLER_DASHBOARD
    codeBody' = generateCodeBody (mkCodeBody apiRead) input
    domainHandlerDashboardModulePrefix = apiDomainHandlerDashboardImportPrefix apiRead ++ "."
    packageOverride :: [String] -> [String]
    packageOverride = checkForPackageOverrides generationType (apiPackageMapping apiRead) (input ^. importPackageOverrides)

    generatorInput :: GeneratorInput
    generatorInput =
      GeneratorInput
        { _ghcOptions = ["-Wwarn=unused-imports"],
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
      nub (qualifiedImp <> figureOutImports (T.unpack <$> concatMap handlerSignature (_apis input)))
        <> ["Domain.Types.MerchantOperatingCity" | ifProviderPlatform]
        <> storeTransactionImports
        <> ["Kernel.Utils.Validation" | ifValidationRequired]
        <> [apiClientImportPrefix apiRead | isApiTreeClientGenerated apiRead]
        <> multipartImports
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
              Just (ApiAuthV2 {}) -> False
              Just (ApiAuthV3 {}) -> False
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

    storeTransactionImports :: [String]
    storeTransactionImports =
      when_
        (any (\apiT -> apiT ^. apiType /= GET) $ input ^. apis)
        (maybeToList $ findModuleName "Domain.Types.Transaction" (apiImportsMapping apiRead))

    ifValidationRequired :: Bool
    ifValidationRequired = any (\apiT -> isJust $ apiT ^. requestValidation) $ input ^. apis

    multipartImports :: [String]
    multipartImports = [findModuleNameWithDefault "Dashboard.Common" (apiImportsMapping apiRead) | apiReadKind apiRead == DASHBOARD && any (isJust . (^. apiMultipartType)) (input ^. apis)]

when_ :: Bool -> [a] -> [a]
when_ False _ = []
when_ True as = as

getClientFunctionName :: ApiRead -> String
getClientFunctionName apiRead = do
  let folderName = fromMaybe (error "folderName should be provided for dashboard api") $ apiFolderName apiRead
  apiClientImportPrefix apiRead #. "call" <> folderName <> "API"

mkCodeBody :: ApiRead -> ApisM ()
mkCodeBody apiRead = do
  input <- ask
  tellM . fromMaybe mempty $
    mkCodeBodyDomainHandlerDashboard apiRead input

mkCodeBodyDomainHandlerDashboard :: ApiRead -> Apis -> Maybe String
mkCodeBodyDomainHandlerDashboard apiRead input = do
  let clientFuncName = getClientFunctionName apiRead
  let serverName = fromMaybe (error "serverName should be provided for dashboard api") $ apiServerName apiRead
  let allApis = input ^. apis
  interpreter input $ do
    forM_ allApis $ handlerFunctionDef apiRead serverName clientFuncName

handlerFunctionDef :: ApiRead -> String -> String -> ApiTT -> Writer CodeUnit
handlerFunctionDef apiRead serverName clientFuncName apiT = do
  input <- ask
  useAuth <- case (apiT ^. authType) of
    Just ApiAuthV2 {} -> pure True
    Just ApiAuthV3 {} -> pure True
    Just NoAuth -> pure False
    _ -> error "Please use ApiAuthV2, ApiAuthV3, or NoAuth in case of auth not required for dashboard api"
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
      let pats = vP "merchantShortId" : vP "opCity" : [vP "apiTokenInfo" | useAuth] <> generateParamsPat apiUnits
      TH.clauseW pats $
        TH.normalB $
          TH.doEW $ do
            whenJust (apiT ^. requestValidation) $ \validationFunc -> do
              let reqParam = case findRequest apiUnits of
                    Just paramText -> vE paramText
                    Nothing -> error "Did not found request for validation"
              TH.noBindSW $ vE "Kernel.Utils.Validation.runRequestValidation" ~* vE (T.unpack validationFunc) ~* reqParam
            if useAuth
              then do
                let checkMerchantExp = vE "merchantCityAccessCheck" ~* vE "merchantShortId" ~* vE "apiTokenInfo.merchant.shortId" ~* vE "opCity" ~* vE "apiTokenInfo.city"
                if isApiTreeClientGenerated apiRead
                  then vP "checkedMerchantId" <-- checkMerchantExp
                  else TH.noBindSW $ vE "Kernel.Prelude.void" ~$ checkMerchantExp
              else letStmt "checkedMerchantId" $ vE "skipMerchantCityAccessCheck" ~* vE "merchantShortId"
            let transactionWrapper clientCall = case apiT ^. apiType of
                  GET -> clientCall
                  _ | not useAuth -> clientCall
                  _ -> do
                    let sharedLogicTransaction = findModuleNameWithDefault "SharedLogic.Transaction" (apiImportsMapping apiRead)
                    let mbDomainTypesTransaction = findModuleName "Domain.Types.Transaction" (apiImportsMapping apiRead)
                    vP "transaction"
                      <-- vE (sharedLogicTransaction <> ".buildTransaction")
                      ~* maybe (vE "apiTokenInfo.userActionType") (\domainTypesTransaction -> vE (domainTypesTransaction <> ".castEndpoint") ~* vE "apiTokenInfo.userActionType") mbDomainTypesTransaction
                      ~* (cE "Kernel.Prelude.Just" ~* cE serverName)
                      ~* (cE "Kernel.Prelude.Just" ~* vE "apiTokenInfo")
                      ~* generateHandlerParam apiUnits "driverId"
                      ~* generateHandlerParam apiUnits "rideId"
                      ~* generateReqParam apiUnits
                    -- TODO implement response transaction storing
                    TH.noBindSW $ vE (sharedLogicTransaction <> ".withTransactionStoring") ~* vE "transaction" ~$ TH.doEW clientCall
            if isApiTreeClientGenerated apiRead
              then do
                case apiT ^. apiHelperApiExtra ^. urlPartsExtra of
                  [] -> pure ()
                  extraParams -> forM_ extraParams $ \case
                    QueryParamExtra "requestorId" "Text" True -> letStmt "requestorId" (vE "apiTokenInfo.personId.getId")
                    QueryParamExtra "requestorId" "Text" False -> letStmt "requestorId" (cE "Kernel.Prelude.Just" ~* vE "apiTokenInfo.personId.getId")
                    QueryParamExtra queryParamName _queryParamType _isMandatory -> letStmt (TH.mkName $ T.unpack queryParamName) (vE "error" ~* strE "Logic yet to be decided")
                transactionWrapper $
                  TH.noBindSW $ do
                    let clientCall = "." <> T.unpack (headToLower moduleName') <> "DSL" #. T.unpack functionName
                    let clientCallWithBoundary =
                          if isJust (apiT ^. apiMultipartType)
                            then (vE (findModuleNameWithDefault "Dashboard.Common" (apiImportsMapping apiRead) <> ".addMultipartBoundary") ~* strE "XXX00XXX") ~. vE clientCall
                            else vE clientCall
                    let clientCallExp units =
                          TH.appendE $
                            vE clientFuncName
                              NE.:| vE "checkedMerchantId" :
                            vE "opCity" :
                            clientCallWithBoundary :
                            generateParamsExp units
                    if isNothing (apiT ^. apiHelperApi)
                      then clientCallExp apiUnits
                      else case (not . null $ apiT ^. apiHelperApiExtra ^. urlPartsExtra, apiT ^. apiHelperApi) of
                        (True, Just helperApiT) -> do
                          let signatureUnitsHelper = mkApiSignatureUnits (helperApiT ^. getHelperAPI)
                              apiUnitsHelper = map apiSignatureUnit signatureUnitsHelper
                          clientCallExp apiUnitsHelper
                        _ -> appendE $ do
                          let errStr = strE $ "Client call " <> clientCall <> " for separate helperApi couldn't be autogenerated. Logic yet to be decided"
                          vE "error" NE.:| errStr : vE "checkedMerchantId" : generateParamsExp apiUnits -- just for avoid unused vars error
              else do
                -- currently transaction store is not used for admin apis without client call
                let errStr = strE "Logic yet to be decided"
                TH.noBindSW $ appendE $ vE "error" NE.:| errStr : generateParamsExp apiUnits -- just for avoid unused vars error
  where
    generateHandlerParam :: [ApiUnit] -> String -> Q TH.Exp
    generateHandlerParam apiUnits param = case findParamText apiUnits param of
      Just paramText -> cE "Kernel.Prelude.Just" ~* vE paramText
      Nothing -> cE "Kernel.Prelude.Nothing"

    generateReqParam :: [ApiUnit] -> Q TH.Exp
    generateReqParam apiUnits = case findRequest apiUnits of
      Just paramText -> cE "Kernel.Prelude.Just" ~* vE paramText
      Nothing -> cE (findModuleNameWithDefault "SharedLogic.Transaction" (apiImportsMapping apiRead) <> ".emptyRequest")
