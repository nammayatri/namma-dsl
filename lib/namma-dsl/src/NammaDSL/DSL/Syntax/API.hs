{-# LANGUAGE TemplateHaskell #-}

module NammaDSL.DSL.Syntax.API where

import Control.Lens hiding (noneOf)
import Kernel.Prelude
import NammaDSL.GeneratorCore

data UrlParts
  = UnitPath Text
  | Capture Text Text
  | QueryParam Text Text Bool
  deriving (Show)

data ApiType = GET | POST | PUT | DELETE deriving (Show)

data AuthType = AdminTokenAuth | TokenAuth | NoAuth | DashboardAuth DashboardAuthType deriving (Show)

data DashboardAuthType = DASHBOARD_USER | DASHBOARD_ADMIN | FLEET_OWNER | DASHBOARD_RELEASE_ADMIN | MERCHANT_ADMIN
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

type TypeObject = (Text, [(Text, Text)])

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
    _apiTypes :: TypesInfo
  }
  deriving (Show)

$(makeLenses ''Apis)

type ApisM = BuilderM Apis
