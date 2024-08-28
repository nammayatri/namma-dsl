{-# LANGUAGE BangPatterns #-}

module NammaDSL.Generator.SQL.API (generateApiSQL) where

import Control.Lens ((^.))
import Control.Monad (forM)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.UTF8 as LBS
import Data.List (intercalate)
import Data.Maybe (isJust)
import qualified Data.Text as T
import NammaDSL.DSL.Syntax.API
import NammaDSL.DSL.Syntax.Common
import NammaDSL.Generator.Haskell.Common as Common
import Prelude

-- Generates SQL for each api endpoint depending on spec
generateApiSQL :: Database -> Maybe MigrationFile -> ApiRead -> Apis -> Either SQL_ERROR String
generateApiSQL database mbOldApiMigrationFile apiRead input = do
  migrationUnits <- forM (input ^. apis) $ \apiTT -> do
    let newApiMigrationKeys = mkApiMigrationKeys database apiTT
    forM newApiMigrationKeys $ \migrationKey -> do
      let oldMigrationKeys = maybe [] apiMigrationKeys mbOldApiMigrationFile
      when' (migrationKey `notElem` oldMigrationKeys) $ do
        migration' <- generateMigration apiRead apiTT migrationKey
        pure $
          "-- "
            <> LBS.toString (A.encode migrationKey)
            <> "\n"
            <> migration'
            <> "\n"
  let migrationsContent = intercalate "\n" $ filter (not . null) (concat migrationUnits)

  migrationUpdates <- when' (not $ null migrationsContent) $ do
    updateStamp' <- when' (isJust mbOldApiMigrationFile) (pure updateStamp)
    pure $ updateStamp' <> migrationsContent
  pure $
    maybe "" rawLastSqlFile mbOldApiMigrationFile <> migrationUpdates

updateStamp :: String
updateStamp = "\n\n------- SQL updates -------\n\n"

when' :: Applicative m => Bool -> m [a] -> m [a]
when' False _ = pure []
when' True as = as

mkApiMigrationKeys :: Database -> ApiTT -> [ApiMigrationKey]
mkApiMigrationKeys database apiTT = do
  let apiName' = Common.mkApiName apiTT
  flip map (apiTT ^. apiMigrate) $ \apiMigrate' -> do
    ApiMigrationKey
      { schema = T.pack database,
        api = apiName',
        migration = apiMigrate' ^. migrationName,
        param = apiMigrate' ^. migrationParam
      }

generateMigration :: ApiRead -> ApiTT -> ApiMigrationKey -> Either SQL_ERROR String
generateMigration apiRead apiTT migrationKey = case lookup (migration migrationKey) supportedMigrations of
  Just f -> f apiRead apiTT migrationKey
  Nothing -> Left $ "Only " <> show (fst <$> supportedMigrations) <> " migrations supported"

-- supported migrations implementation

supportedMigrations :: [(T.Text, ApiRead -> ApiTT -> ApiMigrationKey -> Either SQL_ERROR String)]
supportedMigrations =
  [ ("endpoint", generateEndpointMigration),
    ("endpointV2", generateEndpointV2Migration)
  ]

generateEndpointMigration :: ApiRead -> ApiTT -> ApiMigrationKey -> Either SQL_ERROR String
generateEndpointMigration apiRead apiTT migrationKey = do
  endpointV1 <- maybe (Left "Migration param required for 'endpoint' migration") pure $ param migrationKey
  endpointV3 <- generateEndpointV3 apiRead apiTT
  pure $
    "UPDATE "
      <> T.unpack (schema migrationKey)
      <> ".transaction\n"
      <> "  SET endpoint = '"
      <> endpointV3
      <> "'\n"
      <> "  WHERE endpoint = '"
      <> T.unpack endpointV1
      <> "';"

generateEndpointV2Migration :: ApiRead -> ApiTT -> ApiMigrationKey -> Either SQL_ERROR String
generateEndpointV2Migration apiRead apiTT migrationKey = do
  endpointV2 <- generateEndpointV2 apiTT
  endpointV3 <- generateEndpointV3 apiRead apiTT
  pure $
    "UPDATE "
      <> T.unpack (schema migrationKey)
      <> ".transaction\n"
      <> "  SET endpoint = '"
      <> endpointV3
      <> "'\n"
      <> "  WHERE endpoint = '"
      <> endpointV2
      <> "';"

generateEndpointV2 :: ApiTT -> Either SQL_ERROR String
generateEndpointV2 apiTT = do
  pure $ T.unpack (apiTT ^. apiModuleName) <> "API" <> " " <> Common.mkEndpointName apiTT

generateEndpointV3 :: ApiRead -> ApiTT -> Either SQL_ERROR String
generateEndpointV3 apiRead apiTT = do
  endpointPrefix <- maybe (Left "Endpoint prefix required for 'endpoint' migration") pure $ apiEndpointPrefix apiRead
  folderName <- maybe (Left "Folder name required for 'endpoint' migration") pure $ apiFolderName apiRead
  pure $ endpointPrefix <> folderName <> "API_" <> T.unpack (apiTT ^. apiModuleName) <> "API_" <> Common.mkEndpointName apiTT
