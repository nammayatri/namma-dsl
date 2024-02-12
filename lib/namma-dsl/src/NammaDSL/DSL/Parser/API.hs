{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module NammaDSL.DSL.Parser.API where

import Control.Lens hiding (noneOf)
import Data.Aeson
import Data.Aeson.Key (fromText, toText)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Lens (_Array, _Object, _String, _Value)
import Data.Bool
import qualified Data.ByteString as BS
import Data.List.Split (splitWhen)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml
import Kernel.Prelude hiding (toText)
import NammaDSL.DSL.Syntax.API
import qualified NammaDSL.Utils as U

apiParser :: FilePath -> IO Apis
apiParser filepath = do
  contents <- BS.readFile filepath
  case Yaml.decodeEither' contents of
    Left _ -> error "Not a Valid Yaml"
    Right yml -> pure $ parseApis yml

parseModule :: Object -> Maybe Text
parseModule = preview (ix "module" . _String)

parseTypes :: Object -> Maybe Value
parseTypes = preview (ix "types" . _Value)

parseApis :: Object -> Apis
parseApis obj =
  res
    & imports .~ extractImports res
    & apiTypes . typeImports .~ extractComplexTypeImports res
  where
    defaultImportModule = "API.Types.UI."
    parsedImportPackageOverrides = fromMaybe M.empty $ preview (ix "importPackageOverrides" . _Value . to U.mkList . to M.fromList) obj
    res = mkQApis (Apis modelName allApis [] parsedImportPackageOverrides (TypesInfo [] parseTyp))
    mkQualified = T.pack . U.makeTypeQualified (Just $ T.unpack modelName) (Just parsedTypeDataNames) Nothing defaultImportModule obj . T.unpack
    modelName = fromMaybe (error "Required module name") $ parseModule obj
    parsedTypeObjects = typesToTypeObject (parseTypes obj)
    parsedTypeDataNames = map (\(TypeObject (nm, _)) -> T.unpack nm) parsedTypeObjects
    parseTyp = markQualifiedTypesInTypes modelName parsedTypeObjects obj
    allApis = fromMaybe (error "Failed to parse apis") $ preview (ix "apis" . _Array . to V.toList) obj >>= mapM parseSingleApi
    mkQApis aps = aps & apis . traverse %~ mkQUrlApiTT
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

markQualifiedTypesInTypes :: Text -> [TypeObject] -> Object -> [TypeObject]
markQualifiedTypesInTypes moduleName input obj =
  map
    ( \(TypeObject (x, (y, z))) ->
        TypeObject
          ( x,
            ( map
                ( \(a, b) ->
                    ( a,
                      if a == "enum"
                        then mkEnumTypeQualified dataNames b
                        else T.pack $ U.makeTypeQualified (Just $ T.unpack moduleName) (Just dataNames) Nothing defaultTypeImportModule obj (T.unpack b)
                    )
                )
                y,
              z
            )
          )
    )
    input
  where
    dataNames = map (\(TypeObject (nm, _)) -> T.unpack nm) input
    defaultTypeImportModule = "API.Types.UI."
    mkEnumTypeQualified :: [String] -> Text -> Text
    mkEnumTypeQualified excluded enumTp =
      let individualEnums = T.strip <$> T.splitOn "," enumTp
       in T.intercalate "," $ map (uncurry (<>) . second (T.pack . U.makeTypeQualified (Just $ T.unpack moduleName) (Just excluded) Nothing defaultTypeImportModule obj . T.unpack) . T.breakOn " ") individualEnums

extractComplexTypeImports :: Apis -> [Text]
extractComplexTypeImports api = figureOutImports (concatMap figureOutImports' (api ^. apiTypes . types))
  where
    isEnumType :: TypeObject -> Bool
    isEnumType (TypeObject (_, (arrOfFields, _))) = any (\(k, _) -> k == "enum") arrOfFields

    figureOutImports' :: TypeObject -> [Text]
    figureOutImports' tobj@(TypeObject (_, (tps, _))) =
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

extractImports :: Apis -> [Text]
extractImports api =
  figureOutImports (importUrlPart ++ importHeader ++ importApiRes ++ importApiReq)
  where
    apiTTParts = api ^. apis
    importUrlPart = apiTTParts ^.. traverse . urlPartsTraversal . to importFromUrlPart . _Just
    importHeader = apiTTParts ^.. traverse . headerTraversal . to (\(Header _ t2) -> t2)
    importApiRes = apiTTParts ^.. traverse . apiResTraversal . to (\(ApiRes t1 _) -> t1)
    importApiReq = apiTTParts ^.. traverse . apiReqTraversal . to (\(ApiReq t1 _) -> t1)

    apiReqTraversal :: Traversal' ApiTT ApiReq
    apiReqTraversal = apiReqType . _Just

    urlPartsTraversal :: Traversal' ApiTT UrlParts
    urlPartsTraversal = urlParts . traverse

    headerTraversal :: Traversal' ApiTT HeaderType
    headerTraversal = header . traverse

    apiResTraversal :: Traversal' ApiTT ApiRes
    apiResTraversal = apiResType

    importFromUrlPart :: UrlParts -> Maybe Text
    importFromUrlPart (Capture _ t2) = Just t2
    importFromUrlPart (QueryParam _ t2 _) = Just t2
    importFromUrlPart _ = Nothing

parseSingleApi :: Value -> Maybe ApiTT
parseSingleApi (Object ob) = do
  let (key, val) = head $ KM.toList ob
  let apiTp = getApiType $ toText key
  obj <- preview (_Object) val
  let params = fromMaybe KM.empty $ preview (ix "params" ._Object) obj
  let endpoint = parseEndpoint params $ fromMaybe (error "Endpoint not found !") $ preview (ix "endpoint" . _String) obj
  let auth = getAuthType <$> preview (ix "auth" . _String) obj

  let requestObj = preview (ix "request" . _Object) obj
  let requestTp = requestObj >>= preview (ix "type" . _String)
  let requestFmt = Just $ fromMaybe "JSON" $ requestObj >>= preview (ix "format" . _String)
  let req = ApiReq <$> requestTp <*> requestFmt

  let responseObj = fromMaybe (error "Response Object is required") $ preview (ix "response" . _Object) obj
  let responseTp = fromMaybe (error "Response type is required") $ preview (ix "type" . _String) responseObj
  let responseFmt = fromMaybe "JSON" $ preview (ix "format" . _String) responseObj
  let res = ApiRes responseTp responseFmt

  let query = fromMaybe [] $ preview (ix "query" . _Value . to mkList . to (map (\(a, b) -> QueryParam a b False))) obj
  let mQuery = fromMaybe [] $ preview (ix "mandatoryQuery" . _Value . to mkList . to (map (\(a, b) -> QueryParam a b True))) obj
  let allApiParts = endpoint <> query <> mQuery

  let headers = fromMaybe [] (preview (ix "headers" ._Array . to (mkHeaderList . V.toList)) obj)
  return $ ApiTT allApiParts apiTp auth headers req res
parseSingleApi _ = error "Api specs missing"

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
        Object obj -> fromMaybe (error "Header fields missing") $ Header <$> (obj ^? ix "name" . _String) <*> (obj ^? ix "type" . _String)
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
    extractFields :: KM.KeyMap Value -> [(T.Text, T.Text)]
    extractFields = map (first toText) . KM.toList . fmap extractString

    extractString :: Value -> T.Text
    extractString (String t) = t
    extractString (Array arr) = case V.head arr of
      String t -> "[" <> t <> "]"
      _ -> error "Unexpected type in array: "
    extractString _ = error "Non-string type found in field definition"

    splitTypeAndDerivation :: [(Text, Text)] -> ([(Text, Text)], [Text])
    splitTypeAndDerivation fields = (filter (\(k, _) -> k /= "derive") fields, extractDerive fields)
      where
        extractDerive :: [(Text, Text)] -> [Text]
        extractDerive [] = []
        extractDerive ((k, value) : xs)
          | k == "derive" = T.split (== ',') value
          | otherwise = extractDerive xs

    processType1 :: (Key, Value) -> TypeObject
    processType1 (typeName, Object typeDef) =
      TypeObject (toText typeName, splitTypeAndDerivation $ extractFields typeDef)
    processType1 _ = error "Expected an object in fields"
typesToTypeObject _ = error "Expecting Object in Types"
