module NammaDSL.Generator.Purs.API where

import Control.Lens ((^.))
import Control.Monad.State
import Data.Aeson (Object)
import qualified Data.List.Extra as L
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple.Extra (both)
import NammaDSL.DSL.Syntax.API
import NammaDSL.DSL.Syntax.Common (RecordType (..))
import NammaDSL.Lib.Extractor
import NammaDSL.Utils (apiTypeToText, capitalize, checkArray)
import Prelude

mkRestEndpointInstance :: ApiTT -> Text -> Text -> Maybe (Text, Text, Text) -> Text
mkRestEndpointInstance apiTT req resp wrapperReq =
  "instance make" <> req <> " :: RestEndpoint " <> restEPReq <> " " <> resp <> " where\n"
    <> "  makeRequest "
    <> reqBodyText
    <> " headers = do\n"
    <> "    let url = "
    <> baseUrlText
    <> url
    <> "\n"
    <> "    defaultMakeRequest "
    <> httpMethod
    <> " url headers reqBody Nothing\n"
    <> "  decodeResponse = decodeJSON\n"
    <> "  encodeRequest req = standardEncode req\n"
  where
    httpMethod = T.toUpper $ apiTypeToText $ _apiType apiTT
    url = getUrlText apiTT
    (restEPReq, reqBodyText) = do
      case wrapperReq of
        Just (wrReq, _, _) -> do
          let urlParams = getUrlParamsName apiTT
          (wrReq, "reqBody@(" <> wrReq <> " " <> urlParams <> " _)")
        Nothing -> (req, "reqBody")

mkOtherInstances :: Text -> Maybe Text -> Text
mkOtherInstances reqRespType extraParams =
  "derive instance generic" <> reqRespType <> " :: Generic " <> reqRespType <> " _\n"
    <> "derive instance newtype"
    <> reqRespType
    <> " :: Newtype "
    <> reqRespType
    <> " _\n"
    <> "instance standardEncode"
    <> reqRespType
    <> " :: StandardEncode "
    <> reqRespType
    <> " where standardEncode ("
    <> reqRespType
    <> maybe T.empty (" " <>) extraParams
    <> " body) = standardEncode body\n"
    <> "instance show"
    <> reqRespType
    <> " :: Show "
    <> reqRespType
    <> " where show = genericShow\n"
    <> "instance decode"
    <> reqRespType
    <> " :: Decode "
    <> reqRespType
    <> " where decode = defaultDecode\n"
    <> "instance encode"
    <> reqRespType
    <> " :: Encode "
    <> reqRespType
    <> " where encode = defaultEncode\n"

mkApiTypeInstances :: ApiTT -> Text
mkApiTypeInstances apiTT =
  mkTypeDefinition req
    <> mkTypeDefinition resp
    <> mkArrayTypeDefinition req reqWrapper
    <> mkArrayTypeDefinition resp respWrapper
    <> mkRequestWrapperTypeDefinition fullRequestType
    <> mkRestEndpointInstance apiTT (fromMaybe req reqWrapper) (fromMaybe resp respWrapper) fullRequestType
    <> "\n"
    <> mkOtherInstances req Nothing
    <> "\n"
    <> mkOtherInstances resp Nothing
    <> "\n"
    <> mkFullRequestInstances
    <> mkWrapperInstances reqWrapper
    <> mkWrapperInstances respWrapper
  where
    (req, reqWrapper) = requestType apiTT
    (resp, respWrapper) = responseType apiTT

    fullRequestType = do
      let urlParamsType = getUrlParamsType apiTT
      let urlParamsPlaceholder = getUrlParamsPlaceholder apiTT
      if T.null urlParamsType
        then Nothing
        else Just ((fromMaybe req reqWrapper) <> "Full", urlParamsType, urlParamsPlaceholder)

    mkFullRequestInstances = do
      case fullRequestType of
        Just (wrReq, _, placeholders) -> mkOtherInstances wrReq (Just placeholders) <> "\n"
        Nothing -> T.empty

    mkWrapperInstances wrapperType = do
      case wrapperType of
        Just tp -> mkOtherInstances tp Nothing <> "\n"
        Nothing -> T.empty

mkImports :: Text
mkImports = do
  T.intercalate "\n" $
    [ "import Data.Maybe",
      "import Data.Generic.Rep (class Generic)",
      "import Data.Newtype (class Newtype)",
      "import Data.Show.Generic (genericShow)",
      "import Foreign.Class (class Decode, class Encode, decode, encode)",
      "import Foreign.Generic (decodeJSON)",
      "import Prelude",
      "import Presto.Core.Types.API (class RestEndpoint, class StandardEncode, ErrorResponse, Method(..), defaultMakeRequest, standardEncode)",
      "import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode)",
      "import Services.Config (getBaseUrl)"
    ]

mkTypeDefinition :: Text -> Text
mkTypeDefinition tp = T.pack "newtype " <> tp <> " = " <> tp <> " {}\n\n"

mkArrayTypeDefinition :: Text -> Maybe Text -> Text
mkArrayTypeDefinition tp mbWrapperType =
  case mbWrapperType of
    Just wrapperType -> T.pack "newtype " <> wrapperType <> " = " <> wrapperType <> " (Array " <> tp <> ")\n\n"
    Nothing -> T.empty

mkRequestWrapperTypeDefinition :: Maybe (Text, Text, Text) -> Text
mkRequestWrapperTypeDefinition wrapperReq =
  case wrapperReq of
    Just (name, urlParamsType, _) -> do
      T.pack "newtype " <> name <> " = " <> name <> " " <> urlParamsType <> "\n\n"
    Nothing -> T.empty

determineType :: Text -> (Text, Maybe Text)
determineType rawType = do
  let (rawType', isArrayType) = checkArray rawType
  let tp = removeQualifiedImports rawType'
  if isArrayType
    then (tp, Just $ tp <> "Wrapper")
    else (tp, Nothing)
  where
    removeQualifiedImports :: Text -> Text
    removeQualifiedImports str = last $ T.splitOn (T.pack ".") str

generateAPIIntegrationCode :: Apis -> [EXT_TO] -> String
generateAPIIntegrationCode input _exts = do
  mkModuleName (_moduleName input)
    <> T.unpack mkImports
    <> "\n\n"
    <> L.intercalate "\n" (map makeEXTPursTypes _exts)
    <> "\n\n"
    <> T.unpack (T.intercalate "\n------------------------\n" (map mkApiTypeInstances (_apis input)))

-- 1. For enum put the whole thing
-- 2. For others put the fields
-- 3. If enum then the type should be data
-- 4. If many fields then the type should be newtype

makeEXTPursTypes :: EXT_TO -> String
makeEXTPursTypes (EXT_TO _ name fields) = do
  let requiredRecordType = if isEnumStyle then EXT_D else EXT_NT
  let recordType' = case requiredRecordType of
        EXT_NT -> "newtype"
        EXT_D -> "data"
        EXT_T -> "type"
  case isEnumStyle of
    True -> recordType' <> " " <> name <> " = " <> name <> " " <> (L.intercalate " | " $ map L.trim $ L.splitOn "," $ snd $ head fields) <> "\n"
    False ->
      recordType' <> " " <> name <> " = " <> name <> " {\n"
        <> L.intercalate ",\n" (map makeField fields)
        <> "\n }\n"
  where
    isEnumStyle :: Bool
    isEnumStyle = length fields == 1 && (fst $ head fields) == "enum"

    makeField :: (String, String) -> String
    makeField (fieldName, fieldType) = "  " <> fieldName <> " :: " <> fieldType

getAllEXTType :: [FilePath] -> Object -> Apis -> IO [EXT_TO]
getAllEXTType rootPathPrefixes primitives input = do
  let _extImports' = _extImports input
      _hsImports' = _hsImports input
      _typesObjs = input ^. apiTypes . types
      _dnames = map (\(TypeObject _ (name, _)) -> T.unpack name) _typesObjs
      _extTOs = map tObjToExt _typesObjs
      initialAnalysisState =
        AnalysisState
          { rootPathPrefix = rootPathPrefixes,
            extImports = _extImports',
            haskellImports = _hsImports',
            dTypes = _dnames,
            primitives = primitives,
            alreadyNoticedDeepA = mempty,
            currentQualifiedImports = mempty,
            remainingEXT_TO = _extTOs,
            remaining = mempty,
            result = mempty
          }
  analysedState <- execStateT deepAnalysis initialAnalysisState
  pure $ result analysedState

tObjToExt :: TypeObject -> EXT_TO
tObjToExt (TypeObject rt (name, (fields, _))) = do
  let fields' = map (both T.unpack) fields
      name' = T.unpack name
      recordType' = case rt of
        NewType -> EXT_NT
        Data -> EXT_D
        Type -> EXT_T
  EXT_TO recordType' name' fields'

mkModuleName :: Text -> String
mkModuleName name = "module API.Instances." <> T.unpack name <> " where\n\n"

requestType :: ApiTT -> (Text, Maybe Text)
requestType apiTT =
  case _apiReqType apiTT of
    Just (ApiReq reqType _) -> determineType reqType
    Nothing -> do
      let apiName = handlerFunctionName apiTT
      (T.pack $ capitalize (T.unpack apiName) <> "Request", Nothing)

responseType :: ApiTT -> (Text, Maybe Text)
responseType apiTT =
  case _apiResType apiTT of
    ApiRes resType _ -> determineType resType

getUrlText :: ApiTT -> Text
getUrlText api = do
  foldl urlPartToText T.empty (_urlParts api)
  where
    urlPartToText :: Text -> UrlParts -> Text
    urlPartToText acc (UnitPath name) = acc <> T.pack " <> \"/" <> name <> T.pack "\""
    urlPartToText acc (Capture name _) = acc <> T.pack " <> \"/\" <> " <> name
    urlPartToText acc (QueryParam name _ _) =
      if "?" `T.isInfixOf` acc
        then acc <> " <> \"&" <> name <> "=\" <> " <> name
        else acc <> " <> \"?" <> name <> "=\" <> " <> name

baseUrlText :: Text
baseUrlText = "(getBaseUrl \"\")"

getUrlParamsType :: ApiTT -> Text
getUrlParamsType input =
  T.intercalate " " $ filter (/= T.empty) $ map urlParamType (_urlParts input)
  where
    urlParamType :: UrlParts -> Text
    urlParamType (Capture _ _) = "String"
    urlParamType (QueryParam _ _ _) = "String"
    urlParamType _ = T.empty

getUrlParamsPlaceholder :: ApiTT -> Text
getUrlParamsPlaceholder input =
  T.intercalate " " $ filter (/= T.empty) $ map urlParamPlaceholder (_urlParts input)
  where
    urlParamPlaceholder :: UrlParts -> Text
    urlParamPlaceholder (Capture _ _) = "_"
    urlParamPlaceholder (QueryParam _ _ _) = "_"
    urlParamPlaceholder _ = T.empty

getUrlParamsName :: ApiTT -> Text
getUrlParamsName apiTT = do
  let urlParams = filter (/= T.empty) (map urlParamName (_urlParts apiTT))
  T.intercalate " " urlParams
  where
    urlParamName :: UrlParts -> Text
    urlParamName (UnitPath _) = T.empty
    urlParamName (Capture name _) = name
    urlParamName (QueryParam name _ _) = name

handlerFunctionName :: ApiTT -> Text
handlerFunctionName apiTT =
  let apiTypeText = T.toLower $ apiTypeToText (_apiType apiTT)
      urlPartsText = map urlPartToName (_urlParts apiTT)
   in apiTypeText <> T.intercalate "" (filter (/= T.empty) urlPartsText)
  where
    urlPartToName :: UrlParts -> Text
    urlPartToName (UnitPath name) = (T.toUpper . T.singleton . T.head) name <> T.tail name
    urlPartToName _ = ""
