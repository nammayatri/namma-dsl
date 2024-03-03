{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module NammaDSL.Config where

import Control.Lens
import Dhall (FromDhall)
import GHC.Generics
import System.FilePath
--import Data.Text (Text)
import Prelude

data GenerationType
  = SERVANT_API
  | API_TYPES
  | DOMAIN_HANDLER
  | BEAM_QUERIES
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
    _beamQueries :: FilePath,
    _extraBeamQueries :: FilePath,
    _beamTable :: FilePath,
    _domainHandler :: FilePath,
    _domainType :: FilePath,
    _servantApi :: FilePath,
    _sql :: FilePath,
    _purescriptFrontend :: FilePath
  }
  deriving (Generic, Show, FromDhall)

$(makeLenses ''OutputPath)

data StorageConfig = StorageConfig
  { _sqlTypeMapper :: [(String, String)],
    _extraDefaultFields :: [(String, String)],
    _dbName :: String
  }
  deriving (Generic, Show, FromDhall)

$(makeLenses ''StorageConfig)

data DefaultImports = DefaultImports
  { _qualifiedImports :: [String],
    _simpleImports :: [String],
    _generationType :: GenerationType
  }
  deriving (Generic, Show, FromDhall)

data AppConfigs = AppConfigs
  { _output :: OutputPath,
    _defaultTypeImportMapper :: [(String, String)],
    _defaultImports :: [DefaultImports],
    _storageConfig :: StorageConfig,
    _generate :: [GenerationType]
  }
  deriving (Generic, Show, FromDhall)

$(makeLenses ''AppConfigs)
