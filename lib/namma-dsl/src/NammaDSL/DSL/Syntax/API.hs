{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module NammaDSL.DSL.Syntax.API where

import Control.Lens hiding (noneOf)
import Data.Aeson (Object)
import Data.Default
import Data.Map (Map)
import Data.Text (Text)
import NammaDSL.DSL.Syntax.Common
import NammaDSL.GeneratorCore
import Prelude

data UrlParts
  = UnitPath Text
  | Capture Text Text
  | QueryParam Text Text Bool
  deriving (Show)

data ApiType = GET | POST | PUT | DELETE deriving (Show)

data AuthType = AdminTokenAuth | TokenAuth TokenAuthType | NoAuth | SafetyWebhookAuth DashboardAuthType | DashboardAuth DashboardAuthType deriving (Show)

data TokenAuthType = RIDER_TYPE | PROVIDER_TYPE deriving (Show)

data DashboardAuthType = DASHBOARD_USER | DASHBOARD_ADMIN | FLEET_OWNER | DASHBOARD_RELEASE_ADMIN | MERCHANT_ADMIN | MERCHANT_MAKER | MERCHANT_CHECKER | MERCHANT_SERVER | MERCHANT_USER
  deriving (Show)

data HeaderType = Header Text Text deriving (Show)

data ApiReq = ApiReq Text Text deriving (Show)

data ApiRes = ApiRes Text Text deriving (Show)

data ApiParts = ApiTU ApiType [UrlParts] | HeaderT HeaderType | Auth (Maybe AuthType) | Req Text Text | Res Text Text | ModuleName Text deriving (Show)

data ApiTT = ApiTT
  { _urlParts :: [UrlParts],
    _apiType :: ApiType,
    _authType :: Maybe AuthType,
    _header :: [HeaderType],
    _apiReqType :: Maybe ApiReq,
    _apiResType :: ApiRes
  }
  deriving (Show)

$(makeLenses ''ApiTT)

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
    _apiTypes :: TypesInfo
  }
  deriving (Show)

$(makeLenses ''Apis)

type ApisM = BuilderM Apis

data ApiRead = ApiRead
  { apiTypesImportPrefix :: String,
    apiServantImportPrefix :: String,
    apiDomainHandlerImportPrefix :: String,
    apiDefaultTypeImportMapper :: [(String, String)]
  }

data ExtraParseInfo = ExtraParseInfo
  { _yamlObj :: Object,
    _parsedTypesDataNames :: [String]
  }

$(makeLenses ''ExtraParseInfo)

data ApiState = ApiState
  { _apisRes :: Apis,
    _extraParseInfo :: ExtraParseInfo
  }

$(makeLenses ''ApiState)

instance Default ExtraParseInfo where
  def = ExtraParseInfo mempty []

instance Default ApiState where
  def = ApiState def def

instance Default TypesInfo where
  def = TypesInfo [] []

instance Default ApiRead where
  def = ApiRead "" "" "" []

instance Default Apis where
  def = Apis "" [] [] mempty def

type ApiParserM = ParserM ApiRead ApiState

-- For testing purposes please ignore, will remove soon --
-- data API_OBJ = API_OBJ {
--   aaa :: String,
--   bbb :: String} deriving (Show)
