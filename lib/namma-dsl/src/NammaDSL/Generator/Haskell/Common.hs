module NammaDSL.Generator.Haskell.Common where

import Control.Applicative ((<|>))
import Control.Lens ((^.))
import Control.Monad.Reader (ask)
import qualified Data.Char as Char
import Data.List.Extra (find, nub, snoc)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map, lookup)
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
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
import Prelude hiding (lookup)

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

checkForPackageOverrides :: forall a. (Importable a, Eq a, Ord a, Semigroup a, IsString a) => Map a a -> [a] -> [a]
checkForPackageOverrides packageOverrides = map (\x -> maybe x (\a -> "\"" <> a <> "\" " <> x) (lookup (getImportSignature x) packageOverrides))

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
  Just ApiAuthV2 -> case generationType of
    SERVANT_API_DASHBOARD -> do
      let sn = fromMaybe (error "serverName should be provided for dashboard api") $ apiServerName apiRead
      -- TODO use short synonyms
      let apiTreeModule = apiTypesImportPrefix apiRead
      let apiTypesModule = apiTypesImportPrefix apiRead #. T.unpack (apiTT ^. apiModuleName)
      let (folderUserActionType, moduleUserActionType, endpointUserActionType) = either error id $ mkFullUserActionType apiRead apiTT
      let uat =
            appendInfixT (TH.mkName "/") $
              cT' (folderUserActionType)
                NE.:| [cT' (apiTreeModule #. moduleUserActionType), cT' $ apiTypesModule #. endpointUserActionType]
      Just $ cT "ApiAuth" ~~ cT' sn ~~ cT' "DSL" ~~ uat
    _ -> Nothing -- auth already added in common folder
  Just NoAuth -> Nothing
  Nothing -> Just $ cT "TokenAuth"

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
      apiResText = apiResToText apiTypeText (apiTT ^. apiResType)
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

    apiResToText :: Text -> ApiRes -> Q r TH.Type
    apiResToText apiTypeText apiRes =
      cT (T.unpack apiTypeText) ~~ promotedList1T (T.unpack $ _apiResApiType apiRes) ~~ textToType (_apiResTypeName apiRes)

    headerToText :: HeaderType -> Q r TH.Type
    headerToText (Header name ty) = cT "Header" ~~ strT (T.unpack name) ~~ textToType ty

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
      headerTypeText = map (\(Header name ty) -> ApiSignatureUnit (HeaderUnit name) ty) (_header input)
      reqTypeText = reqTypeToText <$> input ^. apiReqType
      resTypeText = respTypeToText $ input ^. apiResType
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

    respTypeToText :: ApiRes -> ApiSignatureUnit
    respTypeToText = ApiSignatureUnit ResponseUnit . _apiResTypeName

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
