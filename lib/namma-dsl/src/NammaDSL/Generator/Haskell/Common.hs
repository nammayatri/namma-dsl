module NammaDSL.Generator.Haskell.Common where

import Control.Applicative ((<|>))
import Control.Lens ((^.))
import Control.Monad.Reader (ask)
import qualified Data.Char as Char
import Data.List.Extra (find, nub, snoc)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe, maybeToList)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import NammaDSL.Config (ApiKind (..), GenerationType (..))
import NammaDSL.DSL.Syntax.API
import NammaDSL.DSL.Syntax.Common
import NammaDSL.Lib
import qualified NammaDSL.Lib.TH as TH
import qualified NammaDSL.Lib.Types as TH
import NammaDSL.Utils
import Text.Casing (camel, quietSnake)
import Prelude

_Maybe :: TH.Q r TH.Type
_Maybe = cT "Kernel.Prelude.Maybe"

_Id :: TH.Q r TH.Type
_Id = cT "Kernel.Types.Id.Id"

_ShortId :: TH.Q r TH.Type
_ShortId = cT "Kernel.Types.Id.ShortId"

_Person :: TH.Q r TH.Type
_Person = cT "Domain.Types.Person.Person"

_Merchant :: TH.Q r TH.Type
_Merchant = cT "Domain.Types.Merchant.Merchant"

-- TODO: These should n't be hardcoded ..
_MerchantOperatingCity :: TH.Q r TH.Type
_MerchantOperatingCity = cT "Domain.Types.MerchantOperatingCity.MerchantOperatingCity"

apiAuthTypeMapperDomainHandler :: ApiTT -> [TH.Q r TH.Type]
apiAuthTypeMapperDomainHandler apiT = case _authType apiT of
  Just (DashboardAuth _) -> pure $ cT "TokenInfo"
  Just ApiTokenAuth -> pure $ cT "Verified"
  Just ApiAuth {} -> error "ApiAuth is deprecated, use ApiAuthV2"
  Just ApiAuthV2 {} -> [_ShortId ~~ _Merchant, cT "Kernel.Types.Beckn.Context.City"]
  Just ApiAuthV3 {} -> [_ShortId ~~ _Merchant, cT "Kernel.Types.Beckn.Context.City"]
  Just NoAuth -> case apiT ^. apiTypeKind of
    DASHBOARD -> [_ShortId ~~ _Merchant, cT "Kernel.Types.Beckn.Context.City"]
    UI -> []
  Just (SafetyWebhookAuth _) -> pure $ cT "AuthToken"
  Just (TokenAuth tp) -> case tp of
    RIDER_TYPE -> pure $ tupleT 2 ~~ (_Maybe ~~ (_Id ~~ _Person)) ~~ (_Id ~~ _Merchant)
    PROVIDER_TYPE -> pure $ tupleT 3 ~~ (_Maybe ~~ (_Id ~~ _Person)) ~~ (_Id ~~ _Merchant) ~~ (_Id ~~ _MerchantOperatingCity)
  _ -> pure $ tupleT 2 ~~ (_Maybe ~~ (_Id ~~ _Person)) ~~ (_Id ~~ _Merchant)

apiAuthTypeMapperServant :: GenerationType -> ApiTT -> [TH.Q r TH.Type]
apiAuthTypeMapperServant generationType apiT = case _authType apiT of
  Just (DashboardAuth _) -> pure $ cT "TokenInfo"
  Just ApiTokenAuth -> pure $ cT "Verified"
  Just ApiAuth {} -> error "ApiAuth is deprecated, use ApiAuthV2"
  Just ApiAuthV2 {} -> case generationType of
    SERVANT_API_DASHBOARD -> [_ShortId ~~ _Merchant, cT "Kernel.Types.Beckn.Context.City", cT "ApiTokenInfo"]
    DOMAIN_HANDLER_DASHBOARD -> [_ShortId ~~ _Merchant, cT "Kernel.Types.Beckn.Context.City", cT "ApiTokenInfo"]
    _ -> [_ShortId ~~ _Merchant, cT "Kernel.Types.Beckn.Context.City"]
  Just ApiAuthV3 {} -> case generationType of
    SERVANT_API_DASHBOARD -> [_ShortId ~~ _Merchant, cT "Kernel.Types.Beckn.Context.City", cT "ApiTokenInfo"]
    DOMAIN_HANDLER_DASHBOARD -> [_ShortId ~~ _Merchant, cT "Kernel.Types.Beckn.Context.City", cT "ApiTokenInfo"]
    _ -> [_ShortId ~~ _Merchant, cT "Kernel.Types.Beckn.Context.City"]
  Just (SafetyWebhookAuth _) -> pure $ cT "AuthToken"
  Just NoAuth -> case apiT ^. apiTypeKind of
    DASHBOARD -> [_ShortId ~~ _Merchant, cT "Kernel.Types.Beckn.Context.City"]
    UI -> []
  Just (TokenAuth tp) -> case tp of
    RIDER_TYPE -> pure $ tupleT 2 ~~ (_Id ~~ _Person) ~~ (_Id ~~ _Merchant)
    PROVIDER_TYPE -> pure $ tupleT 3 ~~ (_Id ~~ _Person) ~~ (_Id ~~ _Merchant) ~~ (_Id ~~ _MerchantOperatingCity)
  _ -> pure $ tupleT 2 ~~ (_Id ~~ _Person) ~~ (_Id ~~ _Merchant)

getRecordType :: RecordType -> String
getRecordType = \case
  NewType -> "newtype"
  Data -> "data"
  Type -> "type"

checkForPackageOverrides :: forall a. (Importable a, Eq a, Ord a, Semigroup a, IsString a) => GenerationType -> [(GenerationType, a)] -> M.Map a a -> [a] -> [a]
checkForPackageOverrides generatorType packageMapping packageOverrides = map (\x -> maybe x (\a -> "\"" <> (if lookup generatorType packageMapping == Just a then "this" else a) <> "\" " <> x) (M.lookup (getImportSignature x) packageOverrides))

generatePackageImport :: forall a. (Importable a, Eq a, Ord a, Semigroup a, IsString a) => GenerationType -> GenerationType -> [(GenerationType, a)] -> a
generatePackageImport currentGenerator importGenerator packageMapping = do
  let mbCurrentPackage = lookup currentGenerator packageMapping
  let mbImportPackage = lookup importGenerator packageMapping
  case mbImportPackage of
    Just importPackage | mbCurrentPackage /= mbImportPackage -> "\"" <> importPackage <> "\" "
    Just _ -> "\"this\" "
    Nothing -> ""

mkApiNameHelper :: ApiTT -> Text
mkApiNameHelper apiT = case apiT ^. apiHelperApi of
  Just helperApi -> mkApiName (helperApi ^. getHelperAPI) <> "Helper"
  Nothing -> mkApiName apiT

mkApiName :: ApiTT -> Text
mkApiName = headToUpper . handlerFunctionText

handlerFunctionText :: ApiTT -> Text
handlerFunctionText apiTT = flip fromMaybe (headToLower <$> apiTT ^. apiName) $ do
  let moduleName' = apiTT ^. apiModuleName
      apiKind' = apiTT ^. apiTypeKind
  let apiTypeText = T.toLower $ apiTypeToText (_apiType apiTT)
      urlPartsText = map urlPartToName (_urlParts apiTT)
      urlPartsConcat = T.intercalate "" (filter (/= T.empty) urlPartsText)
  case apiKind' of
    UI -> apiTypeText <> urlPartsConcat
    DASHBOARD -> apiTypeText <> moduleName' <> urlPartsConcat
  where
    urlPartToName :: UrlParts -> Text
    urlPartToName (UnitPath name) = (T.toUpper . T.singleton . T.head) name <> T.tail name
    urlPartToName _ = ""

addAuthToApi :: ApiRead -> GenerationType -> ApiTT -> Maybe (Q r TH.Type)
addAuthToApi apiRead generationType apiTT = case _authType apiTT of
  Just AdminTokenAuth -> Just $ cT "AdminTokenAuth"
  Just ApiTokenAuth -> Just $ cT "ApiTokenAuth"
  Just (TokenAuth _) -> Just $ cT "TokenAuth"
  Just (SafetyWebhookAuth dashboardAuthType) -> Just $ cT "SafetyWebhookAuth" ~~ cT' (show dashboardAuthType)
  Just (DashboardAuth dashboardAuthType) -> Just $ cT "DashboardAuth" ~~ cT' (show dashboardAuthType)
  Just (ApiAuth _ _ _) -> error "ApiAuth is deprecated, use ApiAuthV2"
  Just ApiAuthV2 -> buildApiAuth apiRead generationType apiTT True
  Just ApiAuthV3 -> buildApiAuth apiRead generationType apiTT False
  Just NoAuth -> Nothing
  Nothing -> Just $ cT "TokenAuth"
  where
    buildApiAuth :: ApiRead -> GenerationType -> ApiTT -> Bool -> Maybe (Q r TH.Type)
    buildApiAuth apiRead' generationType' apiTT' includeDSL = case generationType' of
      SERVANT_API_DASHBOARD -> do
        let sn = fromMaybe (error "serverName should be provided for dashboard api") $ apiServerName apiRead'
        let baseAuth = cT "ApiAuth" ~~ cT' sn
        if includeDSL
          then do
            -- ApiAuthV2: Use nested path structure
            -- TODO use short synonyms
            let apiTreeModule = apiTypesImportPrefix apiRead'
            let apiTypesModule = apiTypesImportPrefix apiRead' #. T.unpack (apiTT' ^. apiModuleName)
            let (folderUserActionType, moduleUserActionType, endpointUserActionType) = either error id $ mkFullUserActionType apiRead' apiTT'
            let uat =
                  appendInfixT (TH.mkName "/") $
                    cT' (folderUserActionType)
                      NE.:| [cT' (apiTreeModule #. moduleUserActionType), cT' $ apiTypesModule #. endpointUserActionType]
            Just $ baseAuth ~~ cT' "DSL" ~~ uat
          else do
            -- ApiAuthV3: Use simple enum value
            let fullEnumName = mkFullUserActionTypeEnum apiRead' apiTT'
            Just $ baseAuth ~~ cT' fullEnumName
      _ -> Nothing -- auth already added in common folder

type IsHelperApi = Bool

apiTTToTextHelper :: ApiRead -> GenerationType -> ApiTT -> Q r TH.Type
apiTTToTextHelper apiRead generationType = withHelperApi (apiTTToText apiRead generationType)

textToType :: Text -> Q r TH.Type
textToType ty = TH.appendT $ NE.fromList $ cT <$> words (T.unpack ty)

apiTTToText :: ApiRead -> GenerationType -> ApiTT -> Q r TH.Type
apiTTToText apiRead generationType apiTT = do
  let urlPartsText = map urlPartToText (_urlParts apiTT)
      apiTypeText = apiTypeToText (_apiType apiTT)
      apiMultipartText = apiMultipartToText <$> _apiMultipartType apiTT
      apiReqText = apiReqToText <$> apiTT ^. apiReqType
      apiResText = apiResToText apiTypeText (apiTT ^. apiResType) (_responseHeader apiTT)
      headerText = map headerToText (_header apiTT)

  TH.appendInfixT ":>" . NE.fromList $
    maybeToList (addAuthToApi apiRead generationType apiTT)
      <> urlPartsText
      <> headerText
      <> maybeToList apiMultipartText
      <> maybeToList apiReqText
      <> [apiResText]
  where
    urlPartToText :: UrlParts -> Q r TH.Type
    urlPartToText (UnitPath path) = strT (T.unpack path)
    urlPartToText (Capture path ty) = cT "Capture" ~~ strT (T.unpack path) ~~ textToType ty
    urlPartToText (QueryParam path ty isMandatory) =
      if isMandatory
        then cT "MandatoryQueryParam" ~~ strT (T.unpack path) ~~ textToType ty
        else cT "QueryParam" ~~ strT (T.unpack path) ~~ textToType ty

    apiMultipartToText :: ApiMultipart -> Q r TH.Type
    apiMultipartToText (ApiMultipart ty) = cT "Kernel.ServantMultipart.MultipartForm" ~~ cT "Kernel.ServantMultipart.Tmp" ~~ textToType ty

    apiReqToText :: ApiReq -> Q r TH.Type
    apiReqToText (ApiReq ty frmt) = cT "ReqBody" ~~ promotedList1T (T.unpack frmt) ~~ textToType ty

    apiResToText :: Text -> ApiRes -> [HeaderType] -> Q r TH.Type
    apiResToText apiTypeText apiRes [] =
      cT (T.unpack apiTypeText) ~~ promotedList1T (T.unpack $ _apiResApiType apiRes) ~~ textToType (_apiResTypeName apiRes)
    apiResToText apiTypeText apiRes responseHeaders =
      cT (T.unpack apiTypeText)
        ~~ promotedList1T (T.unpack $ _apiResApiType apiRes)
        ~~ cT ("(" <> responseHeadersTypeStr responseHeaders (_apiResTypeName apiRes) <> ")")

    headerToText :: HeaderType -> Q r TH.Type
    headerToText (Header name ty) = cT "Header" ~~ strT (T.unpack name) ~~ textToType ty

-- | Builds "Headers '[Header \"Name\" Type, ...] ResponseType" (no outer parens).
-- Callers wrap in parens as appropriate:
--   Servant type  → TH.parensT (cT (responseHeadersTypeStr ...))
--   Handler sig   → cT ("(" <> responseHeadersTypeStr ... <> ")")
responseHeadersTypeStr :: [HeaderType] -> Text -> String
responseHeadersTypeStr responseHeaders resTypeName =
  "Headers '[" <> T.unpack (T.intercalate ", " (map (\(Header name ty) -> "Header \"" <> name <> "\" " <> ty) responseHeaders)) <> "] " <> T.unpack resTypeName

generateAPIType :: GenerationType -> ApiRead -> Writer Apis CodeUnit
generateAPIType = generateAPIType' False

generateAPITypeHelper :: GenerationType -> ApiRead -> Writer Apis CodeUnit
generateAPITypeHelper = generateAPIType' True

generateAPIType' :: IsHelperApi -> GenerationType -> ApiRead -> Writer Apis CodeUnit
generateAPIType' isHelperApi generationType apiRead = do
  input <- ask
  let allApis = input ^. apis
  tySynDW "API" [] $ do
    case apiReadKind apiRead of
      UI -> do
        let apiTTToText_ = apiTTToText apiRead generationType
        appendInfixT ":<|>" . NE.fromList $ apiTTToText_ <$> allApis
      DASHBOARD -> do
        let apiTTToText_ = cT . T.unpack . (if isHelperApi then mkApiNameHelper else mkApiName)
        let apiPrefix' =
              T.unpack $
                fromMaybe (headToLower $ input ^. moduleName) $
                  if isHelperApi
                    then input ^. helperApiPrefix <|> input ^. apiPrefix
                    else input ^. apiPrefix
        let apiTree = TH.parensT . appendInfixT ":<|>" . NE.fromList $ apiTTToText_ <$> allApis
        if null apiPrefix' then apiTree else uInfixT (strT apiPrefix') ":>" apiTree

data ApiSignatureUnit = ApiSignatureUnit
  { apiSignatureUnit :: ApiUnit,
    apiSignatureType :: Text
  }

data ApiUnit
  = HeaderUnit Text
  | CaptureUnit Text
  | QueryParamUnit Text
  | MandatoryQueryParamUnit Text
  | MultipartUnit
  | RequestUnit
  | ResponseUnit
  deriving (Eq)

--TODO add checks for identical params
apiUnitToText :: ApiUnit -> String
apiUnitToText apiUnit = camel $ T.unpack case apiUnit of
  HeaderUnit name -> name
  CaptureUnit name -> name
  QueryParamUnit name -> name
  MandatoryQueryParamUnit name -> name
  MultipartUnit -> "req" -- shouldn't be both MultipartUnit and RequestUnit in the same api
  RequestUnit -> "req"
  ResponseUnit -> "resp"

withHelperApi :: (ApiTT -> a) -> (ApiTT -> a)
withHelperApi func apiTT = func $ maybe apiTT (^. getHelperAPI) (apiTT ^. apiHelperApi)

mkApiSignatureUnitsHelper :: ApiTT -> [ApiSignatureUnit]
mkApiSignatureUnitsHelper = withHelperApi mkApiSignatureUnits

mkApiSignatureUnits :: ApiTT -> [ApiSignatureUnit]
mkApiSignatureUnits input = do
  let urlTypeText = map urlToText (_urlParts input)
      headerTypeText = map (\(Header name ty) -> ApiSignatureUnit (HeaderUnit name) (headerTypeConversion ty)) (_header input)
      reqTypeText = reqTypeToText <$> input ^. apiReqType
      resTypeText = respTypeToText (input ^. apiResType) (_responseHeader input)
      multipartTypeText = multipartTypeToText <$> _apiMultipartType input

  let signatureUnits = snoc (catMaybes urlTypeText <> headerTypeText <> maybeToList multipartTypeText <> maybeToList reqTypeText) resTypeText
  let apiUnits = apiUnitToText . apiSignatureUnit <$> signatureUnits
  if length (nub apiUnits) /= length apiUnits
    then error $ "Please remove duplicating unit names from api definition " <> T.unpack (handlerFunctionText input) <> ": " <> show apiUnits
    else signatureUnits
  where
    urlToText :: UrlParts -> Maybe ApiSignatureUnit
    urlToText (Capture name ty) = Just $ ApiSignatureUnit (CaptureUnit name) ty
    urlToText (QueryParam name ty isMandatory) = do
      if isMandatory
        then Just $ ApiSignatureUnit (MandatoryQueryParamUnit name) ty
        else Just $ ApiSignatureUnit (QueryParamUnit name) $ "Kernel.Prelude.Maybe (" <> ty <> ")"
    urlToText _ = Nothing

    multipartTypeToText :: ApiMultipart -> ApiSignatureUnit
    multipartTypeToText (ApiMultipart ty) = ApiSignatureUnit MultipartUnit ty

    reqTypeToText :: ApiReq -> ApiSignatureUnit
    reqTypeToText (ApiReq ty _) = ApiSignatureUnit RequestUnit ty

    respTypeToText :: ApiRes -> [HeaderType] -> ApiSignatureUnit
    respTypeToText apiRes [] = ApiSignatureUnit ResponseUnit $ _apiResTypeName apiRes
    respTypeToText apiRes responseHeaders =
      ApiSignatureUnit ResponseUnit $
        T.pack $ "(" <> responseHeadersTypeStr responseHeaders (_apiResTypeName apiRes) <> ")"

    headerTypeConversion :: Text -> Text
    headerTypeConversion tc = "Kernel.Prelude.Maybe (" <> tc <> ")"

handlerSignatureHelper :: ApiTT -> [Text]
handlerSignatureHelper = withHelperApi handlerSignature

handlerSignature :: ApiTT -> [Text]
handlerSignature = fmap apiSignatureType . mkApiSignatureUnits

handlerSignatureClientHelper :: ApiTT -> [Q r TH.Type]
handlerSignatureClientHelper = fmap apiSignatureTypeClient . mkApiSignatureUnitsHelper
  where
    apiSignatureTypeClient (ApiSignatureUnit MultipartUnit ty) = tupleT 2 ~~ cT "Data.ByteString.Lazy.ByteString" ~~ cT (T.unpack ty)
    apiSignatureTypeClient apiSignatureUnit = cT . T.unpack $ apiSignatureType apiSignatureUnit

-- Last one is response, so no need to generate param
generateParamsPat :: [ApiUnit] -> [Q r TH.Pat]
generateParamsPat apiUnits = init $ vP . apiUnitToText <$> apiUnits

-- Last one is response, so no need to generate param
generateParamsExp :: [ApiUnit] -> [Q r TH.Exp]
generateParamsExp apiUnits = init $ vE . apiUnitToText <$> apiUnits

findParamText :: [ApiUnit] -> String -> Maybe String
findParamText units param = do
  let paramString = T.pack param
  findParam units (CaptureUnit paramString)
    <|> findParam units (QueryParamUnit paramString)
    <|> findParam units (MandatoryQueryParamUnit paramString)

findRequest :: [ApiUnit] -> Maybe String
findRequest units = findParam units RequestUnit <|> findParam units MultipartUnit

findParam :: [ApiUnit] -> ApiUnit -> Maybe String
findParam units unit = apiUnitToText <$> find (== unit) units

class Importable a where
  getImportSignature :: a -> a

instance Importable Text where
  getImportSignature = head . T.words

instance Importable String where
  getImportSignature = head . words

mkEndpointName :: ApiTT -> String
mkEndpointName apiT = do
  T.unpack (mkApiName apiT) <> "Endpoint"

mkUserActionTypeName :: ApiTT -> String
mkUserActionTypeName = screamingSnake . T.unpack . mkApiName

screamingSnake :: String -> String
screamingSnake = map Char.toUpper . quietSnake

mkFullUserActionType :: ApiRead -> ApiTT -> Either String (String, String, String)
mkFullUserActionType apiRead apiTT = do
  endpointPrefix <- maybe (Left "Endpoint prefix required for dashboard api generation") pure $ apiEndpointPrefix apiRead
  folderName <- maybe (Left "Folder name required for dashboard api generation") pure $ apiFolderName apiRead
  let folderUserActionType = screamingSnake endpointPrefix <> "_" <> screamingSnake folderName
  let moduleUserActionType = screamingSnake $ T.unpack (apiTT ^. apiModuleName)
  let endpointUserActionType = mkUserActionTypeName apiTT
  pure (folderUserActionType, moduleUserActionType, endpointUserActionType)

-- | Constructs the full UserActionType enum name for ApiAuthV3
-- Uses the last part (endpointUserActionType) from ApiAuthV2 and inserts the folder name
-- Example: POST_ADMIN_PERSON_CREATE from endpointUserActionType="POST_PERSON_CREATE" and folderName="Admin"
mkFullUserActionTypeEnum :: ApiRead -> ApiTT -> String
mkFullUserActionTypeEnum apiRead apiTT = do
  let folderName = fromMaybe (error "Folder name required for dashboard api generation") $ apiFolderName apiRead
  let folderPart = screamingSnake folderName -- e.g., "ADMIN"
  let endpointUserActionType = mkUserActionTypeName apiTT -- e.g., "POST_PERSON_CREATE" (last part of ApiAuthV2)
  -- Insert folder name after HTTP method: POST_PERSON_CREATE -> POST_ADMIN_PERSON_CREATE
  let endpointParts = T.splitOn "_" (T.pack endpointUserActionType)
  case endpointParts of
    (httpMethod : rest) -> T.unpack $ T.intercalate "_" (httpMethod : T.pack folderPart : rest)
    _ -> endpointUserActionType -- Fallback if structure is unexpected

---------- ActorInfo ----------

-- | ApiTT that actually carries actorInfo / helper signature for Servant codegen.
effectiveActorInfoApi :: ApiKind -> ApiTT -> ApiTT
effectiveActorInfoApi DASHBOARD = withHelperApi id
effectiveActorInfoApi UI = id

hasActorInfo :: ApiKind -> ApiTT -> Bool
hasActorInfo apiKind apiT = isJust $ effectiveActorInfoApi apiKind apiT ^. actorInfo

-- | Resolve actorInfo YAML field into a Servant wrapper: wrap $ action.
-- Int is paramsNumber used for numbered aN bindings in Servant.hs.
resolveActorInfoWrapper :: ApiKind -> ApiTT -> Int -> Maybe (Q r TH.Exp -> Q r TH.Exp)
resolveActorInfoWrapper apiKind apiT paramsNumber =
  case effectiveApi ^. actorInfo of
    Nothing -> Nothing
    Just "auth" -> Just $ mkAuthActorInfoWrapper apiKind apiT paramsNumber
    Just paramName -> Just $ mkParamActorInfoWrapper apiKind effectiveApi paramName
  where
    effectiveApi = effectiveActorInfoApi apiKind apiT

applyActorInfoWrapper :: ApiKind -> ApiTT -> Int -> Q r TH.Exp -> Q r TH.Exp
applyActorInfoWrapper apiKind apiT paramsNumber action =
  case resolveActorInfoWrapper apiKind apiT paramsNumber of
    Nothing -> action
    Just wrap -> wrap action

mkAuthActorInfoWrapper :: ApiKind -> ApiTT -> Int -> Q r TH.Exp -> Q r TH.Exp
mkAuthActorInfoWrapper UI apiT paramsNumber action =
  case apiT ^. authType of
    Just (TokenAuth _) ->
      let personExp = vE "Control.Lens.view" ~* vE "Control.Lens._1" ~* vE ("a" <> show paramsNumber)
       in vE "Tools.ActorInfo.withPersonIdActorInfo" ~* personExp ~$ action
    _ -> error $ "actorInfo: auth requires TokenAuth for API " <> T.unpack (handlerFunctionText apiT)
mkAuthActorInfoWrapper DASHBOARD apiT _ _ =
  error $ "actorInfo: auth is only supported for UI APIs, got dashboard API " <> T.unpack (handlerFunctionText apiT)

mkParamActorInfoWrapper :: ApiKind -> ApiTT -> Text -> Q r TH.Exp -> Q r TH.Exp
mkParamActorInfoWrapper apiKind effectiveApi paramName action =
  case findActorInfoParamUnit inputUnits paramName of
    Nothing ->
      error $
        "actorInfo param '"
          <> T.unpack paramName
          <> "' not found in API "
          <> T.unpack (handlerFunctionText effectiveApi)
          <> " signature units: "
          <> show (apiUnitToText . apiSignatureUnit <$> inputUnits)
    Just (idx, isOptional) ->
      let varName = "a" <> show (length inputUnits - idx)
          personExp = mkPersonIdExp isOptional varName
          wrapperFun = actorInfoWrapperFun apiKind isOptional
       in vE wrapperFun ~* personExp ~$ action
  where
    inputUnits = init $ mkApiSignatureUnits effectiveApi

findActorInfoParamUnit :: [ApiSignatureUnit] -> Text -> Maybe (Int, Bool)
findActorInfoParamUnit units paramName =
  listToMaybe $
    flip mapMaybe (zip [0 ..] units) $ \(idx, unit) ->
      case apiSignatureUnit unit of
        CaptureUnit name | name == paramName -> Just (idx, False)
        MandatoryQueryParamUnit name | name == paramName -> Just (idx, False)
        QueryParamUnit name | name == paramName -> Just (idx, True)
        _ -> Nothing

mkPersonIdExp :: Bool -> String -> Q r TH.Exp
mkPersonIdExp True varName = cE "Kernel.Types.Id.Id" ~<$> vE varName
mkPersonIdExp False varName = cE "Kernel.Types.Id.Id" ~* vE varName

actorInfoWrapperFun :: ApiKind -> Bool -> String
actorInfoWrapperFun UI False = "Tools.ActorInfo.withPersonIdActorInfo"
actorInfoWrapperFun UI True = "Tools.ActorInfo.withMbPersonIdActorInfo"
actorInfoWrapperFun DASHBOARD False = "Tools.ActorInfo.withDashboardPersonIdActorInfo"
actorInfoWrapperFun DASHBOARD True = "Tools.ActorInfo.withDashboardMbPersonIdActorInfo"
