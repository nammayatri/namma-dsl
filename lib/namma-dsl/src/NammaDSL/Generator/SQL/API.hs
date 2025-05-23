{-# LANGUAGE BangPatterns #-}

module NammaDSL.Generator.SQL.API (generateApiSQL) where

import Control.Lens ((%~), (&), (^.))
import Control.Monad (forM)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.UTF8 as LBS
import Data.Functor ((<&>))
import Data.List (find, intercalate)
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified NammaDSL.DSL.Parser.API as Parser
import NammaDSL.DSL.Syntax.API
import NammaDSL.DSL.Syntax.Common
import qualified NammaDSL.Generator.Haskell.Common as Common
import Prelude

-- Generates SQL for each api endpoint depending on spec
generateApiSQL :: Database -> Maybe MigrationFile -> Bool -> ApiRead -> Apis -> Either SQL_ERROR String
generateApiSQL database mbOldApiMigrationFile isLocal apiRead input = do
  migrationUnits <- forM (input ^. apis) $ \apiTT -> do
    let apiTTWithDefaultMigrations =
          apiTT & apiMigrate %~ \migrationParams -> do
            let migrationNames = migrationParams <&> (^. migrationName)
            let defaultParams = filter (\defaultMigrationParam -> defaultMigrationParam ^. migrationName `notElem` migrationNames) $ apiMigrationParams apiRead
            migrationParams <> defaultParams
    let newApiMigrationKeys = mkApiMigrationKeys database apiTTWithDefaultMigrations
    forM newApiMigrationKeys $ \migrationKey -> do
      let oldMigrationKeys = maybe [] apiMigrationKeys mbOldApiMigrationFile
      when' (migrationKey `notElem` oldMigrationKeys) $ do
        mbMigration <- generateMigration isLocal apiRead apiTTWithDefaultMigrations migrationKey
        whenJust' mbMigration \migration' ->
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

whenJust' :: Applicative m => Maybe a -> (a -> m [b]) -> m [b]
whenJust' Nothing _ = pure []
whenJust' (Just a) f = f a

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

generateMigration :: Bool -> ApiRead -> ApiTT -> ApiMigrationKey -> Either SQL_ERROR (Maybe String)
generateMigration isLocal' apiRead apiTT migrationKey = do
  case find (\m -> m.name == migration migrationKey) allSupportedMigrations of
    Just SupportedMigration {generate, isLocal, deprecate, errorMessage, name} -> do
      if isLocal == isLocal'
        then if deprecate then Left $ errorMessage name else Just <$> generate apiRead apiTT migrationKey
        else pure Nothing
    Nothing -> Left $ "Only " <> show (name <$> allSupportedMigrations) <> " migrations supported"

-- supported migrations implementation

data SupportedMigration = SupportedMigration
  { name :: T.Text,
    deprecate :: Bool,
    errorMessage :: T.Text -> SQL_ERROR,
    isLocal :: Bool,
    generate :: ApiRead -> ApiTT -> ApiMigrationKey -> Either SQL_ERROR String
  }

allSupportedMigrations :: [SupportedMigration]
allSupportedMigrations = do
  let errorMessage name = "Migration \"" <> T.unpack name <> "\" is deprecated. Should not be used for new apis, only when move api from manual code to DSL. Please remove it from spec"
  [ SupportedMigration {name = "endpoint", isLocal = False, generate = generateEndpointMigration, deprecate = True, errorMessage},
    SupportedMigration {name = "endpointV2", isLocal = False, generate = generateEndpointV2Migration, deprecate = True, errorMessage},
    SupportedMigration {name = "userActionType", isLocal = False, generate = generateUserActionTypeMigration, deprecate = True, errorMessage},
    SupportedMigration {name = "localAccessForRoleId", isLocal = True, generate = generatelocalAccessForRoleIdMigration, deprecate = False, errorMessage}
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
  (folderUserActionType, moduleUserActionType, endpointUserActionType) <- Common.mkFullUserActionType apiRead apiTT
  pure $ folderUserActionType <> "/" <> moduleUserActionType <> "/" <> endpointUserActionType

generateUserActionTypeMigration :: ApiRead -> ApiTT -> ApiMigrationKey -> Either SQL_ERROR String
generateUserActionTypeMigration apiRead apiTT migrationKey = do
  apiAuthParam <- maybe (Left "Migration param required for 'userActionType' migration") pure $ param migrationKey
  (ae, uat) <- case Parser.getAuthType apiAuthParam of
    ApiAuth _sn ae uat -> pure (ae, uat)
    _ -> Left "ApiAuth param required for 'userActionType' migration"

  userActionTypeV2 <- generateUserActionTypeV2 apiRead apiTT
  pure $
    "INSERT INTO "
      <> T.unpack (schema migrationKey)
      <> ".access_matrix (id, role_id, api_entity, user_access_type, user_action_type) "
      <> "( SELECT "
      <> T.unpack (schema migrationKey)
      <> ".uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', '"
      <> userActionTypeV2
      <> "' FROM "
      <> T.unpack (schema migrationKey)
      <> ".access_matrix AS T1 "
      <> "WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = '"
      <> ae.getApiEntity
      <> "' AND T1.user_action_type = '"
      <> uat.getUserActionType
      <> "' ) ON CONFLICT DO NOTHING;"

generateUserActionTypeV2 :: ApiRead -> ApiTT -> Either SQL_ERROR String
generateUserActionTypeV2 = generateEndpointV3

generatelocalAccessForRoleIdMigration :: ApiRead -> ApiTT -> ApiMigrationKey -> Either SQL_ERROR String
generatelocalAccessForRoleIdMigration apiRead apiTT migrationKey = do
  roleId <- maybe (Left "Migration param required for 'userActionType' migration") pure $ param migrationKey
  userActionTypeV2 <- generateUserActionTypeV2 apiRead apiTT
  pure $
    "INSERT INTO "
      <> T.unpack (schema migrationKey)
      <> ".access_matrix (id, role_id, api_entity, user_access_type, user_action_type) VALUES "
      <> "( "
      <> T.unpack (schema migrationKey)
      <> ".uuid_generate_v4(), '"
      <> T.unpack roleId
      <> "', 'DSL', 'USER_FULL_ACCESS', '"
      <> userActionTypeV2
      <> "' ) ON CONFLICT DO NOTHING;"
