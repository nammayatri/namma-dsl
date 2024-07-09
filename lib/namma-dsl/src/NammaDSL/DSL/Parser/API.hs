{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module NammaDSL.DSL.Parser.API where

import Control.Lens hiding (noneOf)
import Control.Monad.Trans.RWS.Lazy
import Data.Aeson
import Data.Aeson.Key (fromText, toText)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Lens (_Array, _Object, _String, _Value)
import Data.Bifunctor
import Data.Bool
import qualified Data.ByteString as BS
import Data.Default
import Data.List.Split (splitWhen)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml
import NammaDSL.AccessorTH
import NammaDSL.Config (ApiKind)
import NammaDSL.DSL.Syntax.API
import NammaDSL.DSL.Syntax.Common
import NammaDSL.Utils (valueToString)
import qualified NammaDSL.Utils as U
import Prelude

parseApis' :: ApiParserM ()
parseApis' = do
  parseModule'
  parseTypes'
  parseAllApis'
  makeApiTTPartsQualified'
  parseImports'
  parseExtraOperations'

parseModule' :: ApiParserM ()
parseModule' = do
  obj <- gets (^. extraParseInfo . yamlObj)
  let parsedModuleName = fromMaybe (error "Required module name") $ obj ^? ix acc_module . _String
  modify $ \s -> s & apisRes . moduleName .~ parsedModuleName

parseExtraOperations' :: ApiParserM ()
parseExtraOperations' = do
  obj <- gets (^. extraParseInfo . yamlObj)
  let parsedExtraOperations = fromMaybe [] $ obj ^? ix acc_extraOperations . _Array . to V.toList . to (map (extraOperation . valueToString))
  modify $ \s -> s & apisRes . extraOperations .~ parsedExtraOperations

parseTypes' :: ApiParserM ()
parseTypes' = do
  obj <- gets (^. extraParseInfo . yamlObj)
  moduleName <- gets (^. apisRes . moduleName)
  defaultImportModule <- asks apiTypesImportPrefix
  defaultTypeImportMap <- asks apiDefaultTypeImportMapper
  let parsedTypeObjects = typesToTypeObject (obj ^? ix acc_types . _Value)
      parsedTypesDataNames' = map (\(TypeObject _ (nm, _)) -> T.unpack nm) parsedTypeObjects
      parsedTypeData = makeQualifiedTypesInTypes defaultTypeImportMap moduleName (T.pack defaultImportModule) parsedTypeObjects obj
  modify $ \s ->
    s
      & apisRes . apiTypes . types .~ parsedTypeData
      & extraParseInfo . parsedTypesDataNames .~ parsedTypesDataNames'

parseAllApis' :: ApiParserM ()
parseAllApis' = do
  obj <- gets (^. extraParseInfo . yamlObj)
  moduleName <- gets (^. apisRes . moduleName)
  apiKind <- asks apiReadKind
  let allApis = fromMaybe (error "Failed to parse apis or no apis defined") $ obj ^? ix acc_apis . _Array . to V.toList >>= mapM (parseSingleApi moduleName apiKind)
  modify $ \s -> s & apisRes . apis .~ allApis
  where
    parseSingleApi :: Text -> ApiKind -> Value -> Maybe ApiTT
    parseSingleApi moduleName apiKind (Object ob) = do
      let (key, val) = head $ KM.toList ob
          apiTp = getApiType $ toText key
      obj <- preview (_Object) val
      let params = fromMaybe KM.empty $ preview (ix acc_params ._Object) obj
          endpoint = parseEndpoint params $ fromMaybe (error "Endpoint not found !") $ preview (ix acc_endpoint . _String) obj
          auth = getAuthType <$> preview (ix acc_auth . _String) obj

          requestObj = preview (ix acc_request . _Object) obj
          requestTp = requestObj >>= preview (ix acc_type . _String)
          requestFmt = Just $ fromMaybe "JSON" $ requestObj >>= preview (ix acc_format . _String)
          req = ApiReq <$> requestTp <*> requestFmt

          responseObj = fromMaybe (error "Response Object is required") $ preview (ix acc_response . _Object) obj
          responseTp = fromMaybe (error "Response type is required") $ preview (ix acc_type . _String) responseObj
          responseFmt = fromMaybe "JSON" $ preview (ix acc_format . _String) responseObj
          res = ApiRes responseTp responseFmt

          query = fromMaybe [] $ preview (ix acc_query . _Value . to mkList . to (map (\(a, b) -> QueryParam a b False))) obj
          mQuery = fromMaybe [] $ preview (ix acc_mandatoryQuery . _Value . to mkList . to (map (\(a, b) -> QueryParam a b True))) obj
          allApiParts = endpoint <> query <> mQuery

          headers = fromMaybe [] (preview (ix acc_headers ._Array . to (mkHeaderList . V.toList)) obj)

          requestValidation = preview (ix acc_validation . _String) obj

          multipartObj = preview (ix acc_multipart . _Object) obj
          multipartTp = multipartObj >>= preview (ix acc_type . _String)
          multipart = ApiMultipart <$> multipartTp
      return $ ApiTT allApiParts apiTp auth headers multipart req res apiKind moduleName requestValidation
    parseSingleApi _ _ _ = error "Api specs missing"

parseImports' :: ApiParserM ()
parseImports' = do
  parseImportPackageOverrides'
  extractImports'
  extractComplexTypeImports'

parseImportPackageOverrides' :: ApiParserM ()
parseImportPackageOverrides' = do
  obj <- gets (^. extraParseInfo . yamlObj)
  let parsedImportPackageOverrides = fromMaybe M.empty $ obj ^? ix acc_importPackageOverrides . _Value . to U.mkList . to M.fromList
  modify $ \s -> s & apisRes . importPackageOverrides .~ parsedImportPackageOverrides

extractImports' :: ApiParserM ()
extractImports' = do
  apiTTParts <- gets (^. apisRes . apis)
  let importUrlPart = apiTTParts ^.. traverse . urlParts . traverse . to importFromUrlPart . _Just
      importHeader = apiTTParts ^.. traverse . header . traverse . to (\(Header _ t2) -> t2)
      importApiRes = apiTTParts ^.. traverse . apiResType . to (\(ApiRes t1 _) -> t1)
      importApiReq = apiTTParts ^.. traverse . apiReqType . _Just . to (\(ApiReq t1 _) -> t1)
      imports' = figureOutImports (importUrlPart ++ importHeader ++ importApiRes ++ importApiReq)
  modify $ \s -> s & apisRes . imports .~ imports'
  where
    importFromUrlPart :: UrlParts -> Maybe Text
    importFromUrlPart = \case
      (Capture _ t2) -> Just t2
      (QueryParam _ t2 _) -> Just t2
      _ -> Nothing

extractComplexTypeImports' :: ApiParserM ()
extractComplexTypeImports' = do
  api <- gets (^. apisRes)
  let complexTypeImports = figureOutImports (concatMap figureOutImports' (api ^. apiTypes . types))
  modify $ \s -> s & apisRes . apiTypes . typeImports .~ complexTypeImports
  where
    isEnumType :: TypeObject -> Bool
    isEnumType (TypeObject _ (_, (arrOfFields, _))) = any (\(k, _) -> k == "enum") arrOfFields

    figureOutImports' :: TypeObject -> [Text]
    figureOutImports' tobj@(TypeObject _ (_, (tps, _))) =
      let isEnum = isEnumType tobj
       in concatMap
            ( ( \potentialImport ->
                  if isEnum
                    then fmap T.pack $ filter ('.' `elem`) $ splitWhen (`elem` ("() []," :: String)) (T.unpack potentialImport)
                    else [potentialImport]
              )
                . snd
            )
            tps

apiParser' :: ApiRead -> FilePath -> IO Apis
apiParser' apiRead filepath = do
  contents <- BS.readFile filepath
  case Yaml.decodeEither' contents of
    Left _ -> error "Not a Valid Yaml"
    Right yml -> do
      let extraParseInfo = ExtraParseInfo yml []
          apiState = ApiState def extraParseInfo
      _apisRes <$> evalParser parseApis' apiRead apiState

makeApiTTPartsQualified' :: ApiParserM ()
makeApiTTPartsQualified' = do
  obj <- gets (^. extraParseInfo . yamlObj)
  defaultImportModule <- asks apiTypesImportPrefix
  defaultTypeImportMap <- asks apiDefaultTypeImportMapper
  moduleName <- gets (^. apisRes . moduleName)
  parsedTypeDataNames <- gets (^. extraParseInfo . parsedTypesDataNames)
  let mkQualified = T.pack . U.makeTypeQualified defaultTypeImportMap (Just $ T.unpack moduleName) (Just parsedTypeDataNames) Nothing defaultImportModule obj . T.unpack
      mkQApiReq (ApiReq t1 t2) = ApiReq (mkQualified t1) t2
      mkQApiRes (ApiRes t1 t2) = ApiRes (mkQualified t1) t2
      mkQUrlParts (Capture t1 t2) = Capture t1 (mkQualified t2)
      mkQUrlParts (QueryParam t1 t2 b) = QueryParam t1 (mkQualified t2) b
      mkQUrlParts other = other
      mkQHeaders (Header t1 t2) = Header t1 (mkQualified t2)
      mkQUrlApiTT apiTT =
        apiTT
          & apiReqType . _Just %~ mkQApiReq
          & apiResType %~ mkQApiRes
          & urlParts . traverse %~ mkQUrlParts
          & header . traverse %~ mkQHeaders
  modify $ \s -> s & apisRes . apis . traverse %~ mkQUrlApiTT

makeQualifiedTypesInTypes :: [(String, String)] -> Text -> Text -> [TypeObject] -> Object -> [TypeObject]
makeQualifiedTypesInTypes defaultTypeImportMap moduleName defaultTypeImportModule input obj =
  map
    ( \(TypeObject rt (x, (y, z))) ->
        TypeObject
          rt
          ( x,
            ( map
                ( \(a, b) ->
                    ( a,
                      if a == "enum"
                        then mkEnumTypeQualified dataNames defaultTypeImportModule b
                        else T.pack $ U.makeTypeQualified defaultTypeImportMap (Just $ T.unpack moduleName) (Just dataNames) Nothing (T.unpack defaultTypeImportModule) obj (T.unpack b)
                    )
                )
                y,
              z
            )
          )
    )
    input
  where
    dataNames = map (\(TypeObject _ (nm, _)) -> T.unpack nm) input
    mkEnumTypeQualified :: [String] -> Text -> Text -> Text
    mkEnumTypeQualified excluded defaultTypeImportModule enumTp =
      let individualEnums = T.strip <$> T.splitOn "," enumTp
       in T.intercalate "," $ map (uncurry (<>) . second (T.pack . U.makeTypeQualified defaultTypeImportMap (Just $ T.unpack moduleName) (Just excluded) Nothing (T.unpack defaultTypeImportModule) obj . T.unpack) . T.breakOn " ") individualEnums

extractComplexTypeImports :: Apis -> [Text]
extractComplexTypeImports api = figureOutImports (concatMap figureOutImports' (api ^. apiTypes . types))
  where
    isEnumType :: TypeObject -> Bool
    isEnumType (TypeObject _ (_, (arrOfFields, _))) = any (\(k, _) -> k == "enum") arrOfFields

    figureOutImports' :: TypeObject -> [Text]
    figureOutImports' tobj@(TypeObject _ (_, (tps, _))) =
      let isEnum = isEnumType tobj
       in concatMap
            ( ( \potentialImport ->
                  if isEnum
                    then fmap T.pack $ filter ('.' `elem`) $ splitWhen (`elem` ("() []," :: String)) (T.unpack potentialImport)
                    else [potentialImport]
              )
                . snd
            )
            tps

mkList :: Value -> [(Text, Text)]
mkList (Object obj) =
  KM.toList obj >>= \(k, v) -> case v of
    String t -> [((toText k), t)]
    _ -> []
mkList _ = []

mkHeaderList :: [Value] -> [HeaderType]
mkHeaderList val =
  map
    ( \case
        Object obj -> fromMaybe (error "Header fields missing") $ Header <$> (obj ^? ix acc_name . _String) <*> (obj ^? ix acc_type . _String)
        _ -> error "Header is not of correct format"
    )
    val

getAuthType :: Text -> AuthType
getAuthType = \case
  "AdminTokenAuth" -> AdminTokenAuth
  "TokenAuth" -> TokenAuth RIDER_TYPE
  "NoAuth" -> NoAuth
  "SafetyWebhookAuth MERCHANT_SERVER" -> SafetyWebhookAuth MERCHANT_SERVER
  "TokenAuth RIDER_TYPE" -> TokenAuth RIDER_TYPE
  "TokenAuth PROVIDER_TYPE" -> TokenAuth PROVIDER_TYPE
  "DashboardAuth DASHBOARD_USER" -> DashboardAuth DASHBOARD_USER
  "DashboardAuth DASHBOARD_ADMIN" -> DashboardAuth DASHBOARD_ADMIN
  "DashboardAuth FLEET_OWNER" -> DashboardAuth FLEET_OWNER
  "DashboardAuth DASHBOARD_RELEASE_ADMIN" -> DashboardAuth DASHBOARD_RELEASE_ADMIN
  "DashboardAuth MERCHANT_ADMIN" -> DashboardAuth MERCHANT_ADMIN
  "DashboardAuth MERCHANT_MAKER" -> DashboardAuth MERCHANT_MAKER
  "DashboardAuth MERCHANT_CHECKER" -> DashboardAuth MERCHANT_CHECKER
  "DashboardAuth MERCHANT_SERVER" -> DashboardAuth MERCHANT_SERVER
  "DashboardAuth MERCHANT_USER" -> DashboardAuth MERCHANT_USER
  authType -> do
    case T.words authType of
      ["ApiAuth", sn, ae, uat] -> ApiAuth (ServerName $ T.unpack sn) (ApiEntity $ T.unpack ae) (UserActionType $ T.unpack uat)
      _ -> error "Not a valid auth type"

getApiType :: Text -> ApiType
getApiType = \case
  "GET" -> GET
  "POST" -> POST
  "PUT" -> PUT
  "DELETE" -> DELETE
  _ -> error "Wrong api type"

parseEndpoint :: Object -> Text -> [UrlParts]
parseEndpoint obj txt =
  map
    ( \x ->
        if (isCapture x)
          then
            let capName = extractCapture x
             in Capture capName (fromMaybe (error "Wrong Capture grp") (obj ^? ix (fromText capName) ._String))
          else UnitPath x
    )
    $ filter (not . T.null) (T.splitOn "/" txt)
  where
    isCapture :: Text -> Bool
    isCapture t = T.isPrefixOf "{" t && T.isSuffixOf "}" t

    extractCapture :: Text -> Text
    extractCapture cap =
      case T.stripPrefix "{" (T.dropEnd 1 cap) of
        Just content -> content
        Nothing -> error "Not enclosed in curly braces"

figureOutImports :: [Text] -> [Text]
figureOutImports imps = T.pack <$> U.figureOutImports (T.unpack <$> imps)

typesToTypeObject :: Maybe Value -> [TypeObject]
typesToTypeObject (Just (Object obj)) =
  map processType1 (KM.toList obj)
  where
    extractRecordType :: Object -> RecordType
    extractRecordType =
      (fromMaybe Data)
        . ( preview
              ( ix acc_recordType . _String
                  . to
                    ( \case
                        "NewType" -> NewType
                        "Data" -> Data
                        "Type" -> Type
                        _ -> error "Not a valid"
                    )
              )
          )

    extractFields :: KM.KeyMap Value -> [(T.Text, T.Text)]
    extractFields = map (first toText) . KM.toList . fmap extractString

    extractString :: Value -> T.Text
    extractString (String t) = t
    extractString (Array arr) = case V.head arr of
      String t -> "[" <> t <> "]"
      _ -> error "Unexpected type in array: "
    extractString _ = error "Non-string type found in field definition"

    splitTypeAndDerivation :: [(Text, Text)] -> ([(Text, Text)], [Text])
    splitTypeAndDerivation fields = (filter (\(k, _) -> not $ k `elem` ["derive", "recordType"]) fields, extractDerive fields)
      where
        extractDerive :: [(Text, Text)] -> [Text]
        extractDerive [] = []
        extractDerive ((k, value) : xs)
          | k == "derive" = T.split (== ',') value
          | otherwise = extractDerive xs

    processType1 :: (Key, Value) -> TypeObject
    processType1 (typeName, Object typeDef) =
      TypeObject (extractRecordType typeDef) (toText typeName, splitTypeAndDerivation $ extractFields typeDef)
    processType1 _ = error "Expected an object in fields"
typesToTypeObject _ = error "Expecting Object in Types"
