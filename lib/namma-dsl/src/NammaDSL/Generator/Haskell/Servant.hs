module NammaDSL.Generator.Haskell.Servant (generateServantAPI, handlerSignature, handlerFunctionText) where

import Control.Lens ((^.))
import Control.Monad.Reader (ask)
import Data.List (nub)
import Data.List.Extra (snoc)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import NammaDSL.Config (DefaultImports (..))
import NammaDSL.DSL.Syntax.API
import NammaDSL.Generator.Haskell.Common (apiAuthTypeMapperServant, checkForPackageOverrides)
import NammaDSL.GeneratorCore
import NammaDSL.Utils
import Prelude

generateServantAPI :: DefaultImports -> ApiRead -> Apis -> Code
generateServantAPI (DefaultImports qualifiedImp simpleImp _) apiRead input =
  generateCode generatorInput
  where
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
          _qualifiedImports = packageOverride allQualifiedImports,
          _codeBody = generateCodeBody (mkCodeBody apiRead) input
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
  let allApis = _apis input
      moduleName' = _moduleName input
      seperator = onNewLine (tellM " :<|> ")
      handlerFunctionText' = tellM . T.unpack . handlerFunctionText
  onNewLine $ do
    tellM "type API = "
    onNewLine $ withSpace $ intercalateA seperator (map apiTTToText allApis)
  onNewLine $ do
    tellM "handler  :: Environment.FlowServer API"
    onNewLine $ tellM "handler = "
    withSpace $ intercalateA seperator (map handlerFunctionText' allApis)
  onNewLine $ intercalateA newLine (map (handlerFunctionDef moduleName') allApis)
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

    generateParams :: Bool -> Bool -> Int -> Int -> Text
    generateParams _ _ _ 0 = ""
    generateParams isAuth isbackParam mx n =
      ( if mx == n && isbackParam && isAuth
          then " (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a" <> T.pack (show n) <> ")"
          else " a" <> T.pack (show n)
      )
        <> generateParams isAuth isbackParam mx (n - 1)

    handlerFunctionDef :: Text -> ApiTT -> ApisM ()
    handlerFunctionDef moduleName' apiT =
      let functionName = handlerFunctionText apiT
          allTypes = handlerSignature apiT
          showType = case filter (/= T.empty) (init allTypes) of
            [] -> T.empty
            ty -> T.intercalate " -> " ty
          handlerTypes = showType <> (if length allTypes > 1 then " -> " else " ") <> "Environment.FlowHandler " <> last allTypes
       in tellM $
            T.unpack $
              functionName <> maybe " :: " (\x -> " :: " <> x <> " -> ") (apiAuthTypeMapperServant apiT) <> handlerTypes
                <> "\n"
                <> functionName
                <> generateParams (isAuthPresent apiT && not (isDashboardAuth apiT)) False (length allTypes) (if isAuthPresent apiT then length allTypes else length allTypes - 1)
                <> generateWithFlowHandlerAPI (isDashboardAuth apiT)
                <> (T.pack domainHandlerModulePrefix)
                <> moduleName'
                <> "."
                <> functionName
                <> generateParams (isAuthPresent apiT && not (isDashboardAuth apiT)) True (length allTypes) (if isAuthPresent apiT then length allTypes else length allTypes - 1)
                <> "\n"

generateWithFlowHandlerAPI :: Bool -> Text
generateWithFlowHandlerAPI = \case
  True -> " = withFlowHandlerAPI' $ "
  False -> " = withFlowHandlerAPI $ "

apiTTToText :: ApiTT -> ApisM ()
apiTTToText apiTT =
  let urlPartsText = map urlPartToText (_urlParts apiTT)
      apiTypeText = apiTypeToText (_apiType apiTT)
      apiReqText = apiReqToText (_apiReqType apiTT)
      apiResText = apiResToText (_apiResType apiTT)
      headerText = map headerToText (_header apiTT)
   in tellM $
        T.unpack $
          addAuthToApi (_authType apiTT) (T.concat urlPartsText <> T.concat headerText <> apiReqText <> " :> " <> apiTypeText <> apiResText)
  where
    addAuthToApi :: Maybe AuthType -> Text -> Text
    addAuthToApi authtype apiDef = case authtype of
      Just AdminTokenAuth -> "AdminTokenAuth" <> apiDef
      Just (TokenAuth _) -> "TokenAuth" <> apiDef
      Just (SafetyWebhookAuth dashboardAuthType) -> "SafetyWebhookAuth '" <> T.pack (show dashboardAuthType) <> apiDef
      Just (DashboardAuth dashboardAuthType) -> "DashboardAuth '" <> T.pack (show dashboardAuthType) <> apiDef
      Just NoAuth -> fromMaybe apiDef (T.stripPrefix " :>" apiDef)
      Nothing -> "TokenAuth" <> apiDef

    urlPartToText :: UrlParts -> Text
    urlPartToText (UnitPath path) = " :> \"" <> path <> "\""
    urlPartToText (Capture path ty) = " :> Capture \"" <> path <> "\" (" <> ty <> ")"
    urlPartToText (QueryParam path ty isMandatory) =
      " :> " <> (if isMandatory then "Mandatory" else "") <> "QueryParam \"" <> path <> "\" (" <> ty <> ")"

    apiReqToText :: Maybe ApiReq -> Text
    apiReqToText Nothing = ""
    apiReqToText (Just (ApiReq ty frmt)) = " :> ReqBody '[" <> frmt <> "] " <> ty

    apiResToText :: ApiRes -> Text
    apiResToText (ApiRes name ty) = " '[" <> ty <> "] " <> name

    headerToText :: HeaderType -> Text
    headerToText (Header name ty) = " :> Header \"" <> name <> "\" " <> ty

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
