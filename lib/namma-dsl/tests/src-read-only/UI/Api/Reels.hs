{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module UI.Api.Reels where

import qualified API.Types.UI.Reels
import qualified Control.Lens
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth
import qualified UI.Api.Action.Reels as UI.Api.Action.Reels

type API =
  ( TokenAuth :> "reels" :> "getAllReelVideos" :> QueryParam "language" Kernel.External.Types.Language :> MandatoryQueryParam "reelsKey" Kernel.Prelude.Text
      :> Get
           ('[JSON])
           API.Types.UI.Reels.ReelsResp
      :<|> TokenAuth
      :> "reels"
      :> "getAllReelVideosForMerchant"
      :> QueryParam
           "language"
           Kernel.External.Types.Language
      :> QueryParam
           "merchantId"
           Kernel.Types.Id.Id
           Domain.Types.Merchant.Merchant
      :> MandatoryQueryParam
           "reelsKey"
           Kernel.Prelude.Text
      :> Get
           ('[JSON])
           API.Types.UI.Reels.ReelsResp
  )

handler :: Environment.FlowServer API
handler = getReelsGetAllReelVideos :<|> getReelsGetAllReelVideosForMerchant

getReelsGetAllReelVideos ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe (Kernel.External.Types.Language) ->
    Kernel.Prelude.Text ->
    Environment.FlowHandler API.Types.UI.Reels.ReelsResp
  )
getReelsGetAllReelVideos a3 a2 a1 = withFlowHandlerAPI $ UI.Api.Action.Reels.getReelsGetAllReelVideos (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

getReelsGetAllReelVideosForMerchant ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe (Kernel.External.Types.Language) ->
    Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) ->
    Kernel.Prelude.Text ->
    Environment.FlowHandler API.Types.UI.Reels.ReelsResp
  )
getReelsGetAllReelVideosForMerchant a4 a3 a2 a1 = withFlowHandlerAPI $ UI.Api.Action.Reels.getReelsGetAllReelVideosForMerchant (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a4) a3 a2 a1
