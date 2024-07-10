{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module NammaDSL.DSL.Syntax.API where

import Control.Lens hiding (noneOf)
import Data.Aeson (Object)
import Data.Default
import Data.Map (Map)
import Data.Text (Text)
import NammaDSL.Config (ApiKind (..))
import NammaDSL.DSL.Syntax.Common
import NammaDSL.GeneratorCore
import Text.Read (readEither)
import Prelude

data UrlParts
  = UnitPath Text
  | Capture Text Text
  | QueryParam Text Text Bool
  deriving (Show)

data ApiType = GET | POST | PUT | DELETE deriving (Show, Eq)

data AuthType
  = AdminTokenAuth
  | TokenAuth TokenAuthType
  | NoAuth
  | SafetyWebhookAuth DashboardAuthType
  | DashboardAuth DashboardAuthType
  | ApiAuth ServerName ApiEntity UserActionType
  deriving (Show)

newtype ServerName = ServerName {getServerName :: String}

instance Show ServerName where
  show = (.getServerName)

newtype ApiEntity = ApiEntity {getApiEntity :: String}

instance Show ApiEntity where
  show = (.getApiEntity)

newtype UserActionType = UserActionType {getUserActionType :: String}

instance Show UserActionType where
  show = (.getUserActionType)

data TokenAuthType = RIDER_TYPE | PROVIDER_TYPE deriving (Show)

data DashboardAuthType = DASHBOARD_USER | DASHBOARD_ADMIN | FLEET_OWNER | DASHBOARD_RELEASE_ADMIN | MERCHANT_ADMIN | MERCHANT_MAKER | MERCHANT_CHECKER | MERCHANT_SERVER | MERCHANT_USER
  deriving (Show)

data HeaderType = Header Text Text deriving (Show)

data ApiReq = ApiReq Text Text deriving (Show)

data ApiRes = ApiRes
  { _apiResTypeName :: Text,
    _apiResApiType :: Text -- FIXME String
  }
  deriving (Show)

$(makeLenses ''ApiRes)

data ApiParts = ApiTU ApiType [UrlParts] | HeaderT HeaderType | Auth (Maybe AuthType) | Req Text Text | Res Text Text | ModuleName Text deriving (Show)

data ApiTT = ApiTT
  { _urlParts :: [UrlParts],
    _apiType :: ApiType,
    _authType :: Maybe AuthType,
    _header :: [HeaderType],
    _apiMultipartType :: Maybe ApiMultipart,
    _apiReqType :: Maybe ApiReq,
    _apiResType :: ApiRes,
    _apiHelperApi :: Maybe HelperApiTT,
    _apiTypeKind :: ApiKind,
    _apiModuleName :: Text,
    _requestValidation :: Maybe Text
  }
  deriving (Show)

newtype HelperApiTT = HelperApiTT {_getHelperAPI :: ApiTT}
  deriving (Show)

newtype ApiMultipart = ApiMultipart Text
  deriving (Show)

$(makeLenses ''ApiTT)

$(makeLenses ''HelperApiTT)

data TypeObject = TypeObject RecordType (Text, ([(Text, Text)], [Text])) deriving (Show)

data TypesInfo = TypesInfo
  { _typeImports :: [Text],
    _types :: [TypeObject]
  }
  deriving (Show)

$(makeLenses ''TypesInfo)

data Apis = Apis
  { _moduleName :: Text,
    _apis :: [ApiTT],
    _imports :: [Text],
    _importPackageOverrides :: Map String String,
    _apiTypes :: TypesInfo,
    _extraOperations :: [ExtraOperations]
  }
  deriving (Show)

data ExtraOperations = EXTRA_API_TYPES_FILE deriving (Show, Eq, Read)

extraOperation :: String -> ExtraOperations
extraOperation str = case readEither str of
  Right operation -> operation
  Left _ -> error "Invalid extra operation"

$(makeLenses ''Apis)

type ApisM = BuilderM Apis

data ApiRead = ApiRead
  { apiTypesImportPrefix :: String,
    extraApiTypesImportPrefix :: String,
    apiServantImportPrefix :: String,
    apiServantDashboardImportPrefix :: String,
    apiDomainHandlerImportPrefix :: String,
    apiDomainHandlerDashboardImportPrefix :: String,
    apiDefaultTypeImportMapper :: [(String, String)],
    apiClientFunction :: Maybe String,
    apiReadKind :: ApiKind
  }

data ExtraParseInfo = ExtraParseInfo
  { _yamlObj :: Object,
    _parsedTypesDataNames :: [String]
  }
  deriving (Show)

$(makeLenses ''ExtraParseInfo)

data ApiState = ApiState
  { _apisRes :: Apis,
    _extraParseInfo :: ExtraParseInfo
  }
  deriving (Show)

$(makeLenses ''ApiState)

instance Default ExtraParseInfo where
  def = ExtraParseInfo mempty []

instance Default ApiState where
  def = ApiState def def

instance Default TypesInfo where
  def = TypesInfo [] []

instance Default ApiRead where
  def = ApiRead "" "" "" "" "" "" [] Nothing UI

instance Default Apis where
  def = Apis "" [] [] mempty def []

type ApiParserM = ParserM ApiRead ApiState
