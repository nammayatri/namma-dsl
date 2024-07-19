{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Estimate where

import qualified Domain.Types.Common
import qualified Domain.Types.Estimate
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Estimate as Beam
import qualified Storage.Cac.FarePolicy
import qualified Storage.Queries.FareParameters

updateQuery :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> m ())
updateQuery id2 = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.id (Kernel.Types.Id.getId id2), Se.Set Beam.updatedAt (Just _now)] [Se.Is Beam.id $ Se.Eq NEW]

instance FromTType' Beam.Estimate Domain.Types.Estimate.Estimate where
  fromTType' (Beam.EstimateT {..}) = do
    farePolicy' <- (maybe (pure Nothing) ((Storage.Cac.FarePolicy.findById Nothing) . Kernel.Types.Id.Id)) farePolicyId
    fareParams' <- Storage.Queries.FareParameters.findById (Kernel.Types.Id.Id fareParamsId)
    pure $
      Just
        Domain.Types.Estimate.Estimate
          { createdAt = createdAt,
            currency = Kernel.Prelude.fromMaybe Kernel.Types.Common.INR currency,
            estimatedDistance = estimatedDistance,
            fareParams = fareParams',
            farePolicy = farePolicy',
            id = Kernel.Types.Id.Id id,
            isBlockedRoute = isBlockedRoute,
            isCustomerPrefferedSearchRoute = isCustomerPrefferedSearchRoute,
            isScheduled = Kernel.Prelude.fromMaybe Kernel.Prelude.False isScheduled,
            maxFare = Kernel.Types.Common.mkAmountWithDefault maxFareAmount maxFare,
            minFare = Kernel.Types.Common.mkAmountWithDefault minFareAmount minFare,
            requestId = Kernel.Types.Id.Id requestId,
            specialLocationTag = specialLocationTag,
            tollNames = tollNames,
            tripCategory = Kernel.Prelude.fromMaybe (Domain.Types.Common.OneWay Domain.Types.Common.OneWayOnDemandDynamicOffer) tripCategory,
            updatedAt = Kernel.Prelude.fromMaybe createdAt updatedAt,
            vehicleServiceTier = vehicleVariant,
            vehicleServiceTierName = vehicleServiceTierName
          }

instance ToTType' Beam.Estimate Domain.Types.Estimate.Estimate where
  toTType' (Domain.Types.Estimate.Estimate {..}) = do
    minFare' <- Kernel.Prelude.roundToIntegral minFare
    minFareAmount' <- Kernel.Prelude.Just minFare
    Beam.EstimateT
      { Beam.createdAt = createdAt,
        Beam.currency = Kernel.Prelude.Just currency,
        Beam.estimatedDistance = estimatedDistance,
        Beam.fareParamsId = ((Kernel.Types.Id.getId . (.id)) fareParams),
        Beam.farePolicyId = ((Kernel.Types.Id.getId . (.id) <$>)) farePolicy,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isBlockedRoute = isBlockedRoute,
        Beam.isCustomerPrefferedSearchRoute = isCustomerPrefferedSearchRoute,
        Beam.isScheduled = Kernel.Prelude.Just isScheduled,
        Beam.maxFare = Kernel.Prelude.roundToIntegral maxFare,
        Beam.maxFareAmount = Kernel.Prelude.Just maxFare,
        Beam.minFare = minFare',
        Beam.minFareAmount = minFareAmount',
        Beam.requestId = Kernel.Types.Id.getId requestId,
        Beam.specialLocationTag = specialLocationTag,
        Beam.tollNames = tollNames,
        Beam.tripCategory = Kernel.Prelude.Just tripCategory,
        Beam.updatedAt = Kernel.Prelude.Just updatedAt,
        Beam.vehicleVariant = vehicleServiceTier,
        Beam.vehicleServiceTierName = vehicleServiceTierName
      }
