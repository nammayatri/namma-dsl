{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module UI.Api.Types.Reels where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.ReelsData
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified NammaDSL.DSL.Syntax.Storage
import Servant
import Tools.Auth

data ReelsResp = ReelsResp {ggg :: [[(Kernel.Prelude.Maybe Kernel.Prelude.Double, Kernel.Prelude.Int)]], reels :: [Domain.Types.ReelsData.ReelsData], rrr :: NammaDSL.DSL.Syntax.Storage.Something}
  deriving (Generic, ToJSON, FromJSON, ToSchema)
