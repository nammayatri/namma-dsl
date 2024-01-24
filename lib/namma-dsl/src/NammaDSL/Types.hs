{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module NammaDSL.Types where

import Kernel.Prelude
import Kernel.Utils.Dhall (FromDhall)

data NammaDSLConfig = NammaDSLConfig
  { api :: ApiConfigs,
    storage :: StorageConfigs
  }
  deriving (Generic, Show, FromDhall)

data ApiConfigs = ApiConfigs
  { inputPath :: String,
    outputPath :: String,
    enabled :: Bool,
    uiIntegrationEnabled: Maybe Bool
  }
  deriving (Generic, Show, FromDhall)

data StorageConfigs = StorageConfigs
  { inputPath :: String,
    outputPath :: String,
    sqlOutputPath :: String,
    enabled :: Bool
  }
  deriving (Generic, Show, FromDhall)
