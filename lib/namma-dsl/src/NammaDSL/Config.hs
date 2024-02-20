{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module NammaDSL.Config where

import Control.Lens
import Dhall (FromDhall)
import GHC.Generics
import System.FilePath
import Prelude

data GenerationTypes
  = SERVANT_API
  | API_TYPES
  | DOMAIN_HANDLER
  | BEAM_QUERIES
  | BEAM_TABLE
  | DOMAIN_TYPE
  | SQL
  deriving (Generic, Show, Eq, FromDhall)

data InputPath = InputPath
  { _api :: Maybe FilePath,
    _storage :: Maybe FilePath
  }
  deriving (Generic, Show, FromDhall)

$(makeLenses ''InputPath)

data OutputPath = OutputPath
  { _apiTypes :: Maybe FilePath,
    _beamQueries :: Maybe FilePath,
    _beamTable :: Maybe FilePath,
    _domainHandler :: Maybe FilePath,
    _domainType :: Maybe FilePath,
    _servantApi :: Maybe FilePath,
    _sql :: Maybe FilePath
  }
  deriving (Generic, Show, FromDhall)

$(makeLenses ''OutputPath)

data AppConfigs = AppConfigs
  { _input :: InputPath,
    _output :: OutputPath,
    _generate :: Maybe [GenerationTypes]
  }
  deriving (Generic, Show, FromDhall)

$(makeLenses ''AppConfigs)
