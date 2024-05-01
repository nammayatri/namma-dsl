module NammaDSL.Generator.Haskell.Servant (generateServantAPI, handlerSignature, handlerFunctionText) where

import Control.Lens ((^.))
import Control.Monad.Reader (ask)
import Data.List (nub)
import Data.List.Extra (snoc)
import Data.Maybe (maybeToList, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import NammaDSL.Config (DefaultImports (..))
import NammaDSL.DSL.Syntax.API
import NammaDSL.Generator.Haskell.Common (apiAuthTypeMapperServant, checkForPackageOverrides)
import NammaDSL.GeneratorCore
import NammaDSL.Utils
import NammaDSL.Lib hiding (Q, Writer)
import qualified NammaDSL.Lib.TH as TH
import qualified NammaDSL.Lib.Types as TH
import Prelude
import qualified Data.List.NonEmpty as NE
import Control.Monad (forM_)

type Writer w = TH.Writer Apis w

type Q w = TH.Q Apis w

generateServantAPI :: DefaultImports -> ApiRead -> Apis -> Code
generateServantAPI (DefaultImports qualifiedImp simpleImp _) apiRead input =
  generateCode generatorInput
  where
    codeBody' = generateCodeBody (mkCodeBody apiRead) input
    servantApiModulePrefix = apiServantImportPrefix apiRead ++ "."
    domainHandlerModulePrefix = apiDomainHandlerImportPrefix apiRead ++ "."
    packageOverride :: [String] -> [String]
    packageOverride = checkForPackageOverrides (input ^. importPackageOverrides)

    generatorInput :: GeneratorInput
    generatorInput =
      GeneratorInput
        { _ghcOptions = ["-Wno-orphans", "-Wno-unused-imports"],
          _extensions = [],
          _moduleNm = servantApiModulePrefix <> T.unpack (_moduleName input),
          _simpleImports = packageOverride allSimpleImports,
          _qualifiedImports = packageOverride $ removeUnusedQualifiedImports codeBody' allQualifiedImports,
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
        <> ( nub $
               qualifiedImp
                 <> ( figureOutImports
                        (T.unpack <$> concatMap handlerSignature (_apis input))
                    )
           )
        <> ["Domain.Types.Merchant.MerchantOperatingCity" | ifProviderPlatform]

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

mkCodeBody :: ApiRead -> ApisM ()
mkCodeBody apiRead = do
  input <- ask
  tellM . fromMaybe mempty $ interpreter input $ do
    generateAPIHandler apiRead

generateAPIHandler :: ApiRead -> Writer CodeUnit
generateAPIHandler apiRead = do
  input <- ask
  let allApis = _apis input
      moduleName' = _moduleName input

  tySynDW "API" [] $ do
    appendInfixT ":<|>" . NE.fromList $ apiTTToText <$> allApis

  TH.decsW $ do
    sigDW "handler" $ do
      cT "Environment.FlowServer" ~~ cT "API"
    funDW "handler" $ do
      TH.clauseW [] $
        TH.normalB $
          appendInfixE (vE ":<|>") (NE.fromList $ vE . T.unpack . handlerFunctionText <$> allApis)
  forM_ allApis $ handlerFunctionDef moduleName'
  where
    domainHandlerModulePrefix = apiDomainHandlerImportPrefix apiRead ++ "."

    isAuthPresent :: ApiTT -> Bool
    isAuthPresent apiT = case _authType apiT of
      Just NoAuth -> False
      _ -> True

    isDashboardAuth :: ApiTT -> Bool
    isDashboardAuth apiT = case _authType apiT of
      Just (DashboardAuth _) -> True
      Just (SafetyWebhookAuth _) -> True
      _ -> False

    generateParamsPat :: Bool -> Int -> Int -> [Q TH.Pat]
    generateParamsPat _ _ 0 = []
    generateParamsPat isAuth mx n = vP ("a" <> show n) : generateParamsPat isAuth mx (n - 1)

    generateParamsExp :: Bool -> Int -> Int -> [Q TH.Exp]
    generateParamsExp _ _ 0 = []
    generateParamsExp isAuth mx n =
      ( if mx == n && isAuth
          then vE "Control.Lens.over" ~ vE "Control.Lens._1" ~ cE "Kernel.Prelude.Just" ~ vE ("a" <> show n)
          else vE ("a" <> show n)
      )
        : generateParamsExp isAuth mx (n - 1)

    handlerFunctionDef :: Text -> ApiTT -> Writer CodeUnit
    handlerFunctionDef moduleName' apiT = do
      let functionName = handlerFunctionText apiT
          allTypes = handlerSignature apiT
          showType = cT . T.unpack <$> filter (/= T.empty) (init allTypes)
          handlerTypes = maybeToList (apiAuthTypeMapperServant apiT) <> showType <> [cT "Environment.FlowHandler" ~~ cT (T.unpack $ last allTypes)]
      TH.decsW $ do
        TH.sigDW (TH.mkNameT functionName) $ do
          TH.forallT [] [] $
            TH.appendArrow $ NE.fromList handlerTypes
        TH.funDW (TH.mkNameT functionName) $ do
          let pats = generateParamsPat (isAuthPresent apiT && not (isDashboardAuth apiT)) (length allTypes) (if isAuthPresent apiT then length allTypes else length allTypes - 1)
          TH.clauseW pats $
            TH.normalB $
              generateWithFlowHandlerAPI (isDashboardAuth apiT) $
                TH.appendE $ vE (domainHandlerModulePrefix <> T.unpack moduleName' #. T.unpack functionName)
                  NE.:| generateParamsExp (isAuthPresent apiT && not (isDashboardAuth apiT)) (length allTypes) (if isAuthPresent apiT then length allTypes else length allTypes - 1)

generateWithFlowHandlerAPI :: Bool -> (Q TH.Exp -> Q TH.Exp)
generateWithFlowHandlerAPI = \case
  True -> (vE "withFlowHandlerAPI'" ~$)
  False -> (vE "withFlowHandlerAPI" ~$)

apiTTToText :: ApiTT -> Q TH.Type
apiTTToText apiTT = do

  let urlPartsText = map urlPartToText (_urlParts apiTT)
      apiTypeText = apiTypeToText (_apiType apiTT)
      apiReqText = apiReqToText <$> _apiReqType apiTT
      apiResText = apiResToText apiTypeText (_apiResType apiTT)
      headerText = map headerToText (_header apiTT)

  TH.appendInfixT ":>" . NE.fromList $
    maybeToList (addAuthToApi $ _authType apiTT)
      <> urlPartsText
      <> headerText
      <> maybeToList apiReqText
      <> [apiResText]
  where
    addAuthToApi :: Maybe AuthType -> Maybe (Q TH.Type)
    addAuthToApi authtype = case authtype of
      Just AdminTokenAuth -> Just $ cT "AdminTokenAuth"
      Just (TokenAuth _) -> Just $ cT "TokenAuth"
      Just (SafetyWebhookAuth dashboardAuthType) -> Just $ cT "SafetyWebhookAuth" ~~ cT ("'" <> show dashboardAuthType)
      Just (DashboardAuth dashboardAuthType) -> Just $ cT "DashboardAuth" ~~ cT ("'" <> show dashboardAuthType)
      Just NoAuth -> Nothing
      Nothing -> Just $ cT "TokenAuth"

    urlPartToText :: UrlParts -> Q TH.Type
    urlPartToText (UnitPath path) = strT (T.unpack path)
    urlPartToText (Capture path ty) = cT "Capture" ~~ strT (T.unpack path) ~~ TH.appendT (NE.fromList $ cT <$> words (T.unpack ty))
    urlPartToText (QueryParam path ty isMandatory) = if isMandatory
      then cT "MandatoryQueryParam"  ~~ strT (T.unpack path) ~~ cT (T.unpack ty)
      else cT "QueryParam"  ~~ strT (T.unpack path) ~~ cT (T.unpack ty)

    apiReqToText :: ApiReq -> Q TH.Type
    apiReqToText (ApiReq ty frmt) = cT "ReqBody" ~~ promotedList1T (T.unpack frmt) ~~ cT (T.unpack ty)

    apiResToText :: Text -> ApiRes -> Q TH.Type
    apiResToText apiTypeText (ApiRes name ty) = cT (T.unpack apiTypeText) ~~ promotedList1T (T.unpack ty) ~~ cT (T.unpack name)

    headerToText :: HeaderType -> Q TH.Type
    headerToText (Header name ty) = cT "Header" ~~ strT (T.unpack name) ~~ cT (T.unpack ty)

handlerFunctionText :: ApiTT -> Text
handlerFunctionText apiTT =
  let apiTypeText = T.toLower $ apiTypeToText (_apiType apiTT)
      urlPartsText = map urlPartToName (_urlParts apiTT)
   in apiTypeText <> T.intercalate "" (filter (/= T.empty) urlPartsText)
  where
    urlPartToName :: UrlParts -> Text
    urlPartToName (UnitPath name) = (T.toUpper . T.singleton . T.head) name <> T.tail name
    urlPartToName _ = ""

handlerSignature :: ApiTT -> [Text]
handlerSignature input =
  let urlTypeText = map urlToText (_urlParts input)
      headerTypeText = map (\(Header _ ty) -> ty) (_header input)
      reqTypeText = reqTypeToText $ _apiReqType input
      resTypeText = (\(ApiRes ty _) -> ty) $ _apiResType input
   in filter (/= T.empty) (snoc (snoc (urlTypeText ++ headerTypeText) reqTypeText) resTypeText)
  where
    urlToText :: UrlParts -> Text
    urlToText (Capture _ ty) = ty
    urlToText (QueryParam _ ty isMandatory) = do
      if isMandatory
        then ty
        else "Kernel.Prelude.Maybe (" <> ty <> ")"
    urlToText _ = ""

    reqTypeToText :: Maybe ApiReq -> Text
    reqTypeToText Nothing = ""
    reqTypeToText (Just (ApiReq ty _)) = ty
