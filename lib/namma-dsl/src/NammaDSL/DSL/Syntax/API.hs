{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module NammaDSL.DSL.Syntax.API where

import Control.Lens hiding (noneOf)
import Data.Aeson (FromJSON, Object, ToJSON)
import Data.Default
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import NammaDSL.Config (ApiKind (..), ApiMigration)
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
  | ApiTokenAuth
  | TokenAuth TokenAuthType
  | NoAuth
  | SafetyWebhookAuth DashboardAuthType
  | DashboardAuth DashboardAuthType
  | ApiAuth ServerName ApiEntity UserActionType
  | ApiAuthV2
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
    _apiName :: Maybe Text,
    _authType :: Maybe AuthType,
    _header :: [HeaderType],
    _apiMultipartType :: Maybe ApiMultipart,
    _apiReqType :: Maybe ApiReq,
    _apiResType :: ApiRes,
    _apiHelperApi :: Maybe HelperApiTT,
    _apiTypeKind :: ApiKind,
    _apiModuleName :: Text,
    _requestValidation :: Maybe Text,
    _apiMigrate :: [ApiMigration]
  }
  deriving (Show)

data MigrationFile = MigrationFile
  { apiMigrationKeys :: [ApiMigrationKey],
    rawLastSqlFile :: String
  }
  deriving (Show)

-- depending on this key in sql file we can define that migration already exists
data ApiMigrationKey = ApiMigrationKey
  { schema :: Text,
    api :: Text,
    migration :: Text,
    param :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype HelperApiTT = HelperApiTT {_getHelperAPI :: ApiTT}
  deriving (Show)

newtype ApiMultipart = ApiMultipart Text
  deriving (Show)

$(makeLenses ''ApiTT)

$(makeLenses ''HelperApiTT)

$(makeLenses ''ApiMigration)

type OverrideDefaultDerive = Bool

data TypeObject = TypeObject RecordType (Text, ([(Text, Text)], [Text])) OverrideDefaultDerive
  deriving (Show)

data TypesInfo = TypesInfo
  { _typeImports :: [Text],
    _types :: [TypeObject]
  }
  deriving (Show)

$(makeLenses ''TypesInfo)

data Apis = Apis
  { _moduleName :: Text,
    _apiPrefix :: Maybe Text,
    _helperApiPrefix :: Maybe Text,
    _apis :: [ApiTT],
    _imports :: [Text],
    _importPackageOverrides :: Map String String,
    _apiTypes :: TypesInfo,
    _extraOperations :: [ExtraOperations]
  }
  deriving (Show)

data ExtraOperations = EXTRA_API_TYPES_FILE | EXTRA_API_COMMON_TYPES_FILE deriving (Show, Eq, Read)

extraOperation :: String -> ExtraOperations
extraOperation str = case readEither str of
  Right operation -> operation
  Left _ -> error "Invalid extra operation"

$(makeLenses ''Apis)

type ApisM = BuilderM Apis

data ApiRead = ApiRead
  { apiTypesImportPrefix :: String,
    extraApiTypesImportPrefix :: String,
    extraApiCommonTypesImportPrefix :: String,
    apiServantImportPrefix :: String,
    apiServantDashboardImportPrefix :: String,
    apiDomainHandlerImportPrefix :: String,
    apiDomainHandlerDashboardImportPrefix :: String,
    apiClientImportPrefix :: String,
    apiDefaultTypeImportMapper :: [(String, String)],
    apiServerName :: Maybe String,
    apiReadKind :: ApiKind,
    apiEndpointPrefix :: Maybe String,
    apiFolderName :: Maybe String,
    apiMigrationParams :: [ApiMigration]
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

-- instance Default ApiRead where
--   def = ApiRead "" "" "" "" "" "" [] Nothing UI Nothing

instance Default Apis where
  def = Apis "" Nothing Nothing [] [] mempty def []

type ApiParserM = ParserM ApiRead ApiState

newtype ApiTree = ApiTree
  { specModules :: [String]
  }

type ApiTreeM = BuilderM ApiTree
