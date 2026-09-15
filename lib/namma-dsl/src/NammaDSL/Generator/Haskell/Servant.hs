module NammaDSL.Generator.Haskell.Servant (handlerFunctionText, generateServantAPI, handlerSignature, apiTTToText) where

import Control.Lens ((^.))
import Control.Monad (forM_, when)
import Control.Monad.Reader (ask)
import Data.List (intercalate, nub, stripPrefix)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, isJust, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import NammaDSL.Config (ApiKind (..), DefaultImports (..), GenerationType (SERVANT_API))
import NammaDSL.DSL.Syntax.API
import NammaDSL.Generator.Haskell.Common hiding (generateParamsExp)
import NammaDSL.GeneratorCore
import NammaDSL.Lib hiding (Q, Writer)
import qualified NammaDSL.Lib.TH as TH
import qualified NammaDSL.Lib.Types as TH
import NammaDSL.Utils
import Prelude

type Writer w = TH.Writer Apis w

type Q w = TH.Q Apis w

generateServantAPI :: DefaultImports -> ApiRead -> Apis -> Code
generateServantAPI (DefaultImports qualifiedImp simpleImp _packageImports _) apiRead input =
  generateCode generatorInput
  where
    generationType = SERVANT_API
    codeBody' = generateCodeBody (mkCodeBody apiRead) input
    servantApiModulePrefix = apiServantImportPrefix apiRead ++ "."
    domainHandlerModulePrefix = apiDomainHandlerImportPrefix apiRead ++ "."
    packageOverride :: [String] -> [String]
    packageOverride = checkForPackageOverrides generationType (apiPackageMapping apiRead) (input ^. importPackageOverrides)

    generatorInput :: GeneratorInput
    generatorInput =
      GeneratorInput
        { _ghcOptions = ["-Wno-orphans", "-Wno-unused-imports"],
          _extensions = [],
          _moduleNm = servantApiModulePrefix <> T.unpack (_moduleName input),
          _moduleExports = allModuleExports,
          _simpleImports = packageOverride allSimpleImports,
          _qualifiedImports = packageOverride $ removeUnusedQualifiedImports codeBody' allQualifiedImports,
          _packageImports,
          _codeBody = codeBody'
        }

    allQualifiedImports :: [String]
    allQualifiedImports =
      [ domainHandlerModulePrefix
          <> T.unpack (_moduleName input)
      ]
        <> customHandlerImport
        <> nub (qualifiedImp <> figureOutImports allHandlersSignatures <> apiTypesImport)
        <> ["Domain.Types.MerchantOperatingCity" | ifProviderPlatform]
        <> multipartImports

    allHandlersSignatures :: [String]
    allHandlersSignatures = case apiReadKind apiRead of
      UI -> T.unpack <$> concatMap handlerSignature (_apis input)
      -- Both shapes: an application-server handler may use the public signature
      -- (see appServerUsesPublicApi), whose types the Helper need not mention.
      -- Unused qualified imports are pruned afterwards.
      DASHBOARD -> T.unpack <$> concatMap (\apiT -> handlerSignature apiT <> handlerSignatureHelper apiT) (_apis input)

    customHandlerImport :: [String]
    customHandlerImport =
      [ appServerCustomHandlerModulePrefix apiRead <> "." <> T.unpack (_moduleName input)
        | appServerDashboardAuth apiRead,
          any (^. apiAppServerCustomHandler) (_apis input)
      ]

    allSimpleImports :: [String]
    allSimpleImports =
      ["Storage.Beam.SystemConfigs ()" | ifNotDashboard]
        <> ["Tools.Auth.Webhook" | ifSafetyDashboard]
        <> ["Tools.Auth.DashboardUserAuth" | appServerDashboardAuth apiRead]
        <> simpleImp

    apiTypesImport :: [String]
    apiTypesImport = [apiTypesImportPrefix apiRead #. T.unpack (_moduleName input) | apiReadKind apiRead == DASHBOARD] -- we need API type for reexport
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

    allModuleExports = do
      let moduleName' = _moduleName input
      let apiTypeName
            | apiReadKind apiRead == UI || appServerDashboardAuth apiRead = "API"
            | otherwise = apiTypesImportPrefix apiRead #. T.unpack moduleName' #. "API"
      Just [apiTypeName, "handler"]

    multipartImports :: [String]
    multipartImports = do
      if apiReadKind apiRead == UI && any (isJust . (^. apiMultipartType)) (input ^. apis)
        then ["Kernel.ServantMultipart"]
        else []

mkCodeBody :: ApiRead -> ApisM ()
mkCodeBody apiRead = do
  input <- ask
  tellM . fromMaybe mempty $
    interpreter input $ do
      when (apiReadKind apiRead == UI) $
        generateAPIType SERVANT_API apiRead
      when (appServerDashboardAuth apiRead) $
        generateAPITypeAppServer (appServerUsesPublicApi apiRead) SERVANT_API apiRead
      when (appServerDashboardAuth apiRead) $
        forM_ (_apis input) $ generateAppServerApiType apiRead
      generateAPIHandler apiRead

-- | Does this spec folder have the application server authorize its dashboard
-- routes? Only meaningful for DASHBOARD-kind APIs.
appServerDashboardAuth :: ApiRead -> Bool
appServerDashboardAuth = apiAppServerDashboardAuth

-- | Does the application server serve the PUBLIC shape of this endpoint (no
-- caller-identifying captures or query params) rather than its Helper?
--
-- Yes when a hand-written handler supplies whatever the Helper needs, or when
-- every parameter the Helper adds identifies the caller and can be taken from
-- the verified session. Otherwise the Helper is served and the client supplies
-- those parameters.
appServerUsesPublicApi :: ApiRead -> ApiTT -> Bool
appServerUsesPublicApi apiRead apiT =
  appServerDashboardAuth apiRead
    && ( apiT ^. apiAppServerCustomHandler
           || ( apiHasOperatorArg apiT
                  && isJust (apiT ^. apiHelperApi)
                  && not (null extras)
                  && all (isJust . sessionDerivedParam) extras
              )
       )
  where
    extras = helperExtraUnits apiT

-- | Module prefix of the hand-written functions behind @appServerHandler: custom@
-- endpoints: the DashboardAuth servant prefix with @API.Action.@ replaced by
-- @Domain.Action.@ (API.Action.DashboardAuth.Fleet -> Domain.Action.DashboardAuth.Fleet).
appServerCustomHandlerModulePrefix :: ApiRead -> String
appServerCustomHandlerModulePrefix apiRead =
  maybe
    (error $ "Unexpected DashboardAuth servant module prefix: " <> prefix)
    ("Domain.Action." <>)
    (stripPrefix "API.Action." prefix)
  where
    prefix = apiServantImportPrefix apiRead

-- | Endpoints authorized per-operator get the verified operator as an extra
-- servant argument; without one there is no session to derive captures from.
apiHasOperatorArg :: ApiTT -> Bool
apiHasOperatorArg apiT = case _authType apiT of
  Just ApiAuthV2 {} -> True
  Just ApiAuthV3 {} -> True
  _ -> False

generateAppServerApiType :: ApiRead -> ApiTT -> Writer CodeUnit
generateAppServerApiType apiRead apiTT = do
  input <- ask
  let moduleName' = input ^. moduleName
      appServerApiName = (if appServerUsesPublicApi apiRead apiTT then mkApiName else mkApiNameHelper) apiTT
  tySynDW (TH.mkNameT appServerApiName) [] $ do
    TH.appendInfixT ":>" . NE.fromList $
      maybeToList (addAuthToApi apiRead SERVANT_API apiTT)
        <> [cT (apiTypesImportPrefix apiRead <> "." <> T.unpack moduleName' <> "." <> T.unpack appServerApiName)]

generateAPIHandler :: ApiRead -> Writer CodeUnit
generateAPIHandler apiRead = do
  input <- ask
  let allApis = _apis input
      moduleName' = _moduleName input

  TH.decsW $ do
    sigDW "handler" $ mkSign moduleName'
    funDW "handler" $
      TH.clauseW mkPat $
        TH.normalB $
          appendInfixE (vE ":<|>") (NE.fromList $ mkExp <$> allApis)
  forM_ allApis $ handlerFunctionDef moduleName'
  where
    domainHandlerModulePrefix = apiDomainHandlerImportPrefix apiRead ++ "."

    mkSign moduleName' = do
      let apiTypeName
            | apiReadKind apiRead == UI || appServerDashboardAuth apiRead = "API"
            | otherwise = apiTypesImportPrefix apiRead #. T.unpack moduleName' #. "API"
      let defSignature = cT "Environment.FlowServer" ~~ cT apiTypeName
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

    isAuthPresent :: ApiTT -> Bool
    isAuthPresent apiT = case _authType apiT of
      Just NoAuth -> False
      _ -> True

    isDashboardAuth :: ApiTT -> Bool
    isDashboardAuth apiT = case _authType apiT of
      Just (DashboardAuth _) -> True
      Just (SafetyWebhookAuth _) -> True
      _ -> False

    isApiTokenAuth :: ApiTT -> Bool
    isApiTokenAuth apiT = case _authType apiT of
      Just ApiTokenAuth -> True
      _ -> False

    generateParamsExp :: Bool -> Int -> [Q TH.Exp]
    generateParamsExp _ 0 = []
    generateParamsExp useAuthWithTuple n =
      ( if useAuthWithTuple
          then vE "Control.Lens.over" ~* vE "Control.Lens._1" ~* cE "Kernel.Prelude.Just" ~* vE ("a" <> show n)
          else vE ("a" <> show n)
      ) :
      generateParamsExp False (n - 1)

    handlerFunctionDef :: Text -> ApiTT -> Writer CodeUnit
    handlerFunctionDef moduleName' apiT = do
      let functionName = handlerFunctionText apiT
          usePublic = appServerUsesPublicApi apiRead apiT
          allTypes = case apiReadKind apiRead of
            UI -> handlerSignature apiT
            -- Public shape when the Helper's extra captures come from the session.
            DASHBOARD -> if usePublic then handlerSignature apiT else handlerSignatureHelper apiT
          showType = cT . T.unpack <$> filter (/= T.empty) (init allTypes)
          handlerTypes = apiAuthTypeMapperServant (apiAppServerDashboardAuth apiRead) SERVANT_API apiT <> showType <> [cT "Environment.FlowHandler" ~~ cT (T.unpack $ last allTypes)]
      TH.decsW $ do
        TH.sigDW (TH.mkNameT functionName) $ do
          TH.forallT [] [] $
            TH.appendArrow $ NE.fromList handlerTypes
        TH.funDW (TH.mkNameT functionName) $ do
          -- The verified operator is an extra servant argument but is NOT passed
          -- on: the domain handlers are shared with the proxied tree and take
          -- merchant and city only. So the pattern binds it and the call skips it.
          let hasOperatorArg = appServerDashboardAuth apiRead && apiHasOperatorArg apiT
              isCustom = appServerDashboardAuth apiRead && apiT ^. apiAppServerCustomHandler
              -- Reads are not audited: provider-dashboard recorded mutations only.
              emitAudit = hasOperatorArg && _apiType apiT /= GET
              operatorUsed = isCustom || emitAudit || usePublic
          let paramsNumber = case apiReadKind apiRead of
                DASHBOARD -> length allTypes + (if hasOperatorArg then 2 else 1)
                UI | isAuthPresent apiT -> length allTypes
                UI -> length allTypes - 1
          -- Arguments are named aN..a1 left to right, so the operator -- third
          -- after merchant and city -- is a(paramsNumber - 2).
          let operatorArgIndex = paramsNumber - 2
              operatorArg = vE ("a" <> show operatorArgIndex)
          let pats =
                [ if hasOperatorArg && n == operatorArgIndex && not operatorUsed
                    then vP ("_a" <> show n)
                    else vP ("a" <> show n)
                  | n <- reverse [1 .. paramsNumber]
                ]

          let publicUnits = init (mkApiSignatureUnits apiT)
              publicArgFor u =
                case lookup (unitName u) (zip (unitName <$> publicUnits) [0 ..]) of
                  Just i -> vE ("a" <> show (paramsNumber - 3 - i))
                  Nothing -> operatorArg
              -- A parameter the Helper adds that identifies the caller, taken
              -- from the verified operator. Optional query params are Maybe.
              sessionArgFor u =
                let fn = fromMaybe (error $ "No session value for " <> unitName u <> " in " <> T.unpack functionName) (sessionDerivedParam u)
                    value = vE fn ~* operatorArg
                 in case apiSignatureUnit u of
                      QueryParamUnit _ -> cE "Kernel.Prelude.Just" ~* value
                      _ -> value
              helperArgsExp =
                [ if unitName u `elem` (unitName <$> publicUnits)
                    then publicArgFor u
                    else sessionArgFor u
                  | u <- init (mkApiSignatureUnitsHelper apiT)
                ]
          let dashboardParamsExp
                | usePublic =
                  [vE ("a" <> show paramsNumber), vE ("a" <> show (paramsNumber - 1))] <> helperArgsExp
                | otherwise =
                  [ vE ("a" <> show n)
                    | n <- reverse [1 .. paramsNumber],
                      not (hasOperatorArg && n == operatorArgIndex)
                  ]
          let domainCallExp =
                TH.appendE $
                  vE (domainHandlerModulePrefix <> T.unpack moduleName' #. T.unpack functionName)
                    NE.:| ( if apiReadKind apiRead == DASHBOARD
                              then dashboardParamsExp
                              else generateParamsExp (isAuthPresent apiT && (not $ isApiTokenAuth apiT) && not (isDashboardAuth apiT) && (apiReadKind apiRead /= DASHBOARD)) paramsNumber
                          )
              -- The hand-written handler takes exactly this handler's arguments,
              -- the verified operator included, and does the rest itself.
              customCallExp =
                TH.appendE $
                  vE (appServerCustomHandlerModulePrefix apiRead <> "." <> T.unpack moduleName' #. T.unpack functionName)
                    NE.:| [vE ("a" <> show n) | n <- reverse [1 .. paramsNumber]]
              -- The body is decoded by now, so the audit row can carry it.
              endpointId = either error (\(f, m, e) -> intercalate "/" [f, m, e]) (mkFullUserActionType apiRead apiT)
              serverNameCtor = "Tools.Auth.DashboardUserAuth." <> fromMaybe (error "serverName should be provided for dashboard api") (apiServerName apiRead)
              requestArgExp
                | apiT ^. apiAuditRequestBody && (isJust (apiT ^. apiReqType) || isJust (apiT ^. apiMultipartType)) = cE "Kernel.Prelude.Just" ~* vE "a1"
                | otherwise = TH.sigE (cE "Kernel.Prelude.Nothing") (cT "Kernel.Prelude.Maybe" ~~ cT "()")
              auditExp =
                vE "Tools.Auth.DashboardUserAuth.auditDashboardAction"
                  ~* cE serverNameCtor
                  ~* strE endpointId
                  ~* operatorArg
                  ~* requestArgExp
              callExp = if isCustom then customCallExp else domainCallExp
              bodyExp
                | emitAudit = TH.doEW $ do
                  TH.noBindSW auditExp
                  TH.noBindSW callExp
                | otherwise = callExp
          TH.clauseW pats $
            TH.normalB $
              generateWithFlowHandlerAPI (apiReadKind apiRead) (isDashboardAuth apiT) bodyExp

generateWithFlowHandlerAPI :: ApiKind -> Bool -> (Q TH.Exp -> Q TH.Exp)
generateWithFlowHandlerAPI UI True = (vE "withFlowHandlerAPI'" ~$)
generateWithFlowHandlerAPI UI False = (vE "withFlowHandlerAPI" ~$)
generateWithFlowHandlerAPI DASHBOARD _ = (vE "withDashboardFlowHandlerAPI" ~$)
