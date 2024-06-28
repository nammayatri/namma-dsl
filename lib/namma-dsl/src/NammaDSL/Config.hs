{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module NammaDSL.Config where

import Control.Lens
import Dhall (FromDhall)
import GHC.Generics
import System.FilePath
import Text.Read (readEither)
import Prelude

data GenerationType
  = SERVANT_API
  | SERVANT_API_DASHBOARD
  | API_TYPES
  | DOMAIN_HANDLER
  | BEAM_QUERIES
  | CACHED_QUERIES
  | BEAM_TABLE
  | DOMAIN_TYPE
  | SQL
  | PURE_SCRIPT_FRONTEND
  deriving (Generic, Show, Eq, FromDhall)

data InputPath = InputPath
  { _api :: FilePath,
    _storage :: FilePath
  }
  deriving (Generic, Show, FromDhall)

$(makeLenses ''InputPath)

data OutputPath = OutputPath
  { _apiRelatedTypes :: FilePath,
    _extraApiRelatedTypes :: FilePath,
    _beamQueries :: FilePath,
    _extraBeamQueries :: FilePath,
    _cachedQueries :: FilePath,
    _extraCachedQueries :: FilePath,
    _beamTable :: FilePath,
    _domainHandler :: FilePath,
    _domainType :: FilePath,
    _servantApi :: FilePath,
    _servantApiDashboard :: FilePath,
    _sql :: [(FilePath, String)],
    _purescriptFrontend :: FilePath
  }
  deriving (Generic, Show, FromDhall)

$(makeLenses ''OutputPath)

data StorageConfig = StorageConfig
  { _sqlTypeMapper :: [(String, String)],
    _extraDefaultFields :: [(String, String)],
    _defaultCachedQueryKeyPrefix :: String
  }
  deriving (Generic, Show, FromDhall)

$(makeLenses ''StorageConfig)

data DefaultImports = DefaultImports
  { _qualifiedImports :: [String],
    _simpleImports :: [String],
    _packageImports :: [PackageImport],
    _generationType :: GenerationType
  }
  deriving (Generic, Show, FromDhall)

data PackageImport = PackageImport
  { _importType :: ImportType,
    _importPackageName :: String,
    _importModuleName :: String
  }
  deriving (Generic, Show, FromDhall, Eq)

data ImportType = SIMPLE | QUALIFIED
  deriving (Generic, Show, FromDhall, Eq)

data AppConfigs = AppConfigs
  { _output :: OutputPath,
    _defaultTypeImportMapper :: [(String, String)],
    _defaultImports :: [DefaultImports],
    _storageConfig :: StorageConfig,
    _generate :: [GenerationType],
    _apiKind :: ApiKind,
    _apiTreeMapper :: [ApiTreeMapper]
  }
  deriving (Generic, Show, FromDhall)

data ApiTree = MANAGEMENT | FLEET | RIDE_BOOKING
  deriving (Generic, Show, Read, Eq, FromDhall)

-- if FilePath did not specified then used default value from OutputPath
data ApiTreeMapper = ApiTreeMapper
  { _apiTree :: ApiTree,
    _apiTreeClientName :: String,
    _apiTreeApiRelatedTypes :: Maybe FilePath,
    _apiTreeDomainHandler :: Maybe FilePath,
    _apiTreeServantApi :: Maybe FilePath,
    _apiTreeServantApiDashboard :: Maybe FilePath
  }
  deriving (Generic, Show, FromDhall)

parseApiTree :: String -> ApiTree
parseApiTree str = case readEither str of
  Right tree -> tree
  Left _ -> error "Invalid client name"

data ApiKind = UI | DASHBOARD
  deriving (Generic, Show, FromDhall, Eq)

$(makeLenses ''ApiTreeMapper)

$(makeLenses ''AppConfigs)
