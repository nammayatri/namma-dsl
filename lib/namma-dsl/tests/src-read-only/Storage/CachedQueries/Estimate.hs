{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.CachedQueries.Estimate (module Storage.CachedQueries.Estimate, module ReExport) where

import qualified Domain.Types.Estimate
import qualified Domain.Types.SearchRequest
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.EstimateExtra as ReExport
import qualified Storage.Queries.Estimate as Queries

cacheQuery ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Domain.Types.Estimate.Estimate -> m ())
cacheQuery id requestId createdAt something dataToBeCached = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeKeyForCaching id requestId Kernel.Prelude.True createdAt something) dataToBeCached expTime

deleteQuery :: CacheFlow m r => (Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> m ())
deleteQuery id requestId = do Hedis.withCrossAppRedis (Hedis.del $ "driverOffer:CachedQueries:Estimate:" <> ":Id-" <> Kernel.Types.Id.getId id <> ":RequestId-" <> Kernel.Types.Id.getId requestId <> ":Constant-" <> show (0))

findAndCacheQuery ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Common.HighPrecMoney -> m ([Domain.Types.Estimate.Estimate]))
findAndCacheQuery id createdAt requestId something minFare = do
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeKey1 id requestId Kernel.Prelude.True createdAt something)
    >>= ( \case
            Just a -> pure a
            Nothing ->
              ( \dataToBeCached -> do
                  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                  Hedis.withCrossAppRedis $ Hedis.setExp (makeKey1 id requestId Kernel.Prelude.True createdAt something) dataToBeCached expTime
              )
                /=<< Queries.findAAAAA id requestId True minFare
        )

findOnlyQuery ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> m ([Domain.Types.Estimate.Estimate]))
findOnlyQuery id createdAt requestId something = do
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeKey1 id requestId Kernel.Prelude.True createdAt something)
    >>= ( \case
            Just a -> pure a
            Nothing -> pure []
        )
