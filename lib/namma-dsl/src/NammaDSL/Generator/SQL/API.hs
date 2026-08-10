{-# LANGUAGE BangPatterns #-}

module NammaDSL.Generator.SQL.API (generateApiSQL) where

import Control.Lens ((%~), (&), (^.))
import Control.Monad (forM, forM_, unless)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.UTF8 as LBS
import Data.Functor ((<&>))
import Data.List (find, intercalate)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified NammaDSL.DSL.Parser.API as Parser
import NammaDSL.DSL.Syntax.API
import NammaDSL.DSL.Syntax.Common
import qualified NammaDSL.Generator.Haskell.Common as Common
import Prelude

-- Generates SQL for each api endpoint depending on spec
generateApiSQL :: Database -> Maybe MigrationFile -> Bool -> ApiRead -> Apis -> Either SQL_ERROR String
generateApiSQL database mbOldApiMigrationFile isLocal apiRead input = do
  checkCapabilitiesDeclared apiRead input
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

-- | Every authenticated dashboard endpoint must say which capability it needs.
--
-- Enforced only against endpoints absent from the baseline, so the ones that
-- were mapped in bulk when the capability framework landed stay untouched —
-- the DSL cannot otherwise tell "old endpoint that never had a migrate block"
-- from "brand-new endpoint", since both have zero migration keys.
--
-- No baseline configured (every non-dashboard spec) means no requirement.
checkCapabilitiesDeclared :: ApiRead -> Apis -> Either SQL_ERROR ()
checkCapabilitiesDeclared apiRead input =
  whenJustBaseline (apiCapabilityBaseline apiRead) $ \baseline ->
    forM_ (input ^. apis) $ \apiTT -> do
      endpointId <- generateEndpointV3 apiRead apiTT
      let declared = any (\m -> m ^. migrationName == capabilityMigrationName) (apiTT ^. apiMigrate)
      unless (declared || not (needsCapability apiTT) || T.pack endpointId `Set.member` baseline) $
        Left $
          "Endpoint " <> endpointId <> " does not declare a capability.\n"
            <> "Add it to the api in the spec yaml:\n"
            <> "      migrate:\n"
            <> "        capability: <domain>.<resource>.<action>\n"
            <> "Reuse an existing capability id unless you would ever grant this endpoint "
            <> "separately from its neighbours. Use `capability: PUBLIC` if every "
            <> "authenticated user may call it."
  where
    whenJustBaseline Nothing _ = Right ()
    whenJustBaseline (Just b) f = f b

-- | Authenticated dashboard endpoints only. Anything unauthenticated has no
-- caller to check a capability against.
needsCapability :: ApiTT -> Bool
needsCapability apiTT = case apiTT ^. authType of
  Just ApiAuthV2 -> True
  Just ApiAuthV3 -> True
  Just (ApiAuth _ _ _) -> True
  _ -> False

capabilityMigrationName :: T.Text
capabilityMigrationName = "capability"

-- | `capability: PUBLIC` documents a deliberate opt-out: the migration key is
-- still recorded (so it never regenerates) but only an explanatory comment is
-- written, no capability_endpoint row. Declaring it is what keeps "nobody
-- decided" and "decided it is open" from looking alike.
--
-- NOTE: never put "capability" in a config's _migrationParams defaults. Those
-- are injected with no param, and this migration requires one.
publicCapability :: T.Text
publicCapability = "PUBLIC"

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
        then
          if deprecate
            then do
              -- Tolerated for endpoints that predate the capability framework.
              -- Their deprecated migrations already ran against the old schema;
              -- refusing them here would fail the build for every spec nobody
              -- is changing, the moment a second _sql output is added (a new
              -- output has no key history, so all of them look unemitted).
              -- A NEW endpoint reaching for a deprecated migration is still an
              -- error, which is what the deprecation is actually guarding.
              baselined <- isBaselinedEndpoint apiRead apiTT
              if baselined then pure Nothing else Left $ errorMessage name
            else Just <$> generate apiRead apiTT migrationKey
        else pure Nothing
    Nothing -> Left $ "Only " <> show (name <$> allSupportedMigrations) <> " migrations supported"

-- | Does this endpoint predate the capability framework? False when no baseline
-- is configured, which keeps every non-dashboard spec on the strict behaviour.
isBaselinedEndpoint :: ApiRead -> ApiTT -> Either SQL_ERROR Bool
isBaselinedEndpoint apiRead apiTT = case apiCapabilityBaseline apiRead of
  Nothing -> pure False
  Just baseline -> do
    endpointId <- generateEndpointV3 apiRead apiTT
    pure $ T.pack endpointId `Set.member` baseline

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
    SupportedMigration {name = "capability", isLocal = False, generate = generateCapabilityMigration, deprecate = False, errorMessage},
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

-- | Register the endpoint against the capability it needs. This is the row
-- Tools.Auth.Capability reads on every request; without it the endpoint is
-- denied, since there is no access_matrix fallback any more.
--
-- The endpoint id comes from generateEndpointV3, which is the same
-- MODULE/RESOURCE/ACTION string the runtime builds from the UserActionType —
-- so the two sides cannot drift apart.
generateCapabilityMigration :: ApiRead -> ApiTT -> ApiMigrationKey -> Either SQL_ERROR String
generateCapabilityMigration apiRead apiTT migrationKey = do
  capabilityId <-
    maybe
      (Left "Migration param required for 'capability': the capability id this endpoint needs, e.g. city-operations.ride.read")
      pure
      $ param migrationKey
  endpointId <- generateEndpointV3 apiRead apiTT
  -- PUBLIC declares the endpoint needs no capability. A row would make it
  -- un-callable, so only a comment is written; the key is still recorded.
  if capabilityId == publicCapability
    then pure "-- capability: PUBLIC - no capability_endpoint row; every authenticated caller may call this endpoint."
    else
      pure $
        "INSERT INTO "
          <> T.unpack (schema migrationKey)
          <> ".capability_endpoint (capability_id, server_name, endpoint_id) VALUES "
          <> "( '"
          <> T.unpack capabilityId
          <> "', 'DASHBOARD', '"
          <> endpointId
          <> "' ) ON CONFLICT DO NOTHING;"

-- | Grant a local dev role the capability this endpoint needs. Used to be an
-- access_matrix insert, which no longer authorizes anything — authorization is
-- capability-only. Reads the id off the api's own `capability` migration so the
-- two can never disagree.
generatelocalAccessForRoleIdMigration :: ApiRead -> ApiTT -> ApiMigrationKey -> Either SQL_ERROR String
generatelocalAccessForRoleIdMigration _apiRead apiTT migrationKey = do
  roleId <- maybe (Left "Migration param required for 'localAccessForRoleId' migration") pure $ param migrationKey
  -- localAccessForRoleId is a config-level default applied to EVERY api, so it
  -- also fires for endpoints that predate the capability framework. Those have
  -- nothing to grant (their access came from the seed) and erroring on them
  -- would break every old spec the moment a second _sql output is added.
  let mbCapabilityId =
        find (\m -> m ^. migrationName == capabilityMigrationName) (apiTT ^. apiMigrate) >>= (^. migrationParam)
  case mbCapabilityId of
    Nothing -> pure "-- no capability declared (endpoint predates the capability framework); nothing to grant locally."
    Just capabilityId ->
      if capabilityId == publicCapability
        then pure "-- capability: PUBLIC - nothing to grant locally."
        else
          pure $
            "INSERT INTO "
              <> T.unpack (schema migrationKey)
              <> ".role_capability (role_id, capability_id) VALUES "
              <> "( '"
              <> T.unpack roleId
              <> "', '"
              <> T.unpack capabilityId
              <> "' ) ON CONFLICT DO NOTHING;"
