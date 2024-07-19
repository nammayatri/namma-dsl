{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module UI.Api.Action.Reels where

import qualified API.Types.UI.Reels
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

getReelsGetAllReelVideos ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe (Kernel.External.Types.Language) ->
    Kernel.Prelude.Text ->
    Environment.Flow API.Types.UI.Reels.ReelsResp
  )
getReelsGetAllReelVideos = error "Logic yet to be decided"

getReelsGetAllReelVideosForMerchant ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe (Kernel.External.Types.Language) ->
    Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) ->
    Kernel.Prelude.Text ->
    Environment.Flow API.Types.UI.Reels.ReelsResp
  )
getReelsGetAllReelVideosForMerchant = error "Logic yet to be decided"
