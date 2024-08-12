{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module NammaDSL.Config where

import Control.Lens
import Data.Text (Text)
import Dhall (FromDhall)
import GHC.Generics
import System.FilePath
import Prelude

data GenerationType
  = SERVANT_API
  | SERVANT_API_DASHBOARD
  | API_TYPES
  | DOMAIN_HANDLER
  | DOMAIN_HANDLER_DASHBOARD
  | BEAM_QUERIES
  | CACHED_QUERIES
  | BEAM_TABLE
  | DOMAIN_TYPE
  | SQL
  | PURE_SCRIPT_FRONTEND
  | PURE_SCRIPT_FRONTEND_COMPONENT
  | PURE_SCRIPT_FRONTEND_SCREEN
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
    _domainHandlerDashboard :: FilePath,
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
    _clientFunction :: Maybe String
  }
  deriving (Generic, Show, FromDhall)

data ApiKind = UI | DASHBOARD
  deriving (Generic, Show, FromDhall, Eq)

$(makeLenses ''AppConfigs)

data TechDesignConfig = TechDesignConfig
  { _tdRootPaths :: [FilePath],
    _defaultModuleMapper :: [(Text, Text)]
  }
  deriving (Generic, Show, FromDhall)

$(makeLenses ''TechDesignConfig)

data FrontendConfig = FrontendConfig
  { _fGenRootPath :: FilePath,
    _fComponentModulePrefix :: String,
    _fScreenModulePrefix :: String,
    _fDefaultImportMapper :: [(String, String)],
    _fDefaultImports :: [String],
    _fGenerate :: [GenerationType]
  }
  deriving (Generic, Show, FromDhall)

$(makeLenses ''FrontendConfig)
