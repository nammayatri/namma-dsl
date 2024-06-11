{-# LANGUAGE QuasiQuotes #-}

module NammaDSL.App where

import Control.Lens ((^.))
import Control.Monad (unless, when)
import Control.Monad.Extra (whenJust)
import Data.List.Extra (replace)
import Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Text as T
import NammaDSL.Config
import NammaDSL.DSL.Parser.API
import NammaDSL.DSL.Parser.Storage
import NammaDSL.DSL.Syntax.API
import NammaDSL.DSL.Syntax.Storage
import NammaDSL.Generator.Haskell
import NammaDSL.Generator.Haskell.ApiTypes
import NammaDSL.Generator.Purs
import NammaDSL.Generator.SQL
import NammaDSL.Utils
import System.Directory
import System.FilePath
import System.Process (readProcess)
--import qualified Debug.Trace as DT
import Prelude

version :: String
version = "1.0.55"

runStorageGenerator :: FilePath -> FilePath -> IO ()
runStorageGenerator configPath yamlPath = do
  config <- fetchDhallConfig configPath
  let modulePrefix tp = haskellModuleNameFromFilePath (config ^. output . tp)
      storageRead =
        StorageRead
          { domainTypeModulePrefix = modulePrefix domainType,
            beamTypeModulePrefix = modulePrefix beamTable,
            queryModulePrefix = modulePrefix beamQueries,
            cachedQueryModulePrefix = modulePrefix NammaDSL.Config.cachedQueries,
            sqlMapper = config ^. storageConfig . sqlTypeMapper,
            extraDefaultFields = _extraDefaultFields (config ^. storageConfig),
            storageDefaultTypeImportMapper = config ^. defaultTypeImportMapper,
            defaultCachedQueryKeyPfx = config ^. storageConfig . defaultCachedQueryKeyPrefix
          }
  tableDefs <- storageParser storageRead yamlPath
  let when' = \(t, f) -> when (elem t (config ^. generate)) $ f yamlPath config storageRead tableDefs
  mapM_
    when'
    [ (DOMAIN_TYPE, mkDomainType),
      (BEAM_TABLE, mkBeamTable),
      (BEAM_QUERIES, mkBeamQueries),
      (CACHED_QUERIES, mkCachedQueries),
      (SQL, mkSQLFile)
    ]

runApiGenerator :: FilePath -> FilePath -> IO ()
runApiGenerator configPath yamlPath = do
  config <- fetchDhallConfig configPath
  let modulePrefix tp = haskellModuleNameFromFilePath (config ^. output . tp)
  let apiRead =
        ApiRead
          { apiTypesImportPrefix = modulePrefix apiRelatedTypes,
            apiServantImportPrefix = modulePrefix servantApi,
            apiDomainHandlerImportPrefix = modulePrefix domainHandler,
            apiDefaultTypeImportMapper = config ^. defaultTypeImportMapper
          }
  apiDef <- apiParser' apiRead yamlPath
  let when' = \(t, f) -> when (elem t (config ^. generate)) $ f yamlPath config apiRead apiDef
  mapM_
    when'
    [ (SERVANT_API, mkServantAPI),
      (API_TYPES, mkApiTypes),
      (DOMAIN_HANDLER, mkDomainHandler),
      (PURE_SCRIPT_FRONTEND, mkFrontendAPIIntegration)
    ]

data FileState = NEW | CHANGED | UNCHANGED | NOT_EXIST deriving (Eq, Show)

getHashObjectAtHEAD :: FilePath -> IO (Maybe String)
getHashObjectAtHEAD filePath = do
  let gitCommand = "git ls-tree -r HEAD " ++ filePath
  result <- readProcess "bash" ["-c", gitCommand] []
  let hashObject = if result == "" then Nothing else Just $ words result !! 2
  return hashObject

getHashObject :: FilePath -> IO (Maybe String)
getHashObject filePath = do
  let gitCommand = "git hash-object " ++ filePath
  result <- readProcess "bash" ["-c", gitCommand] []
  let hashObject = if result == "" then Nothing else Just $ init result
  return hashObject

getFileState :: FilePath -> IO FileState
getFileState filePath = do
  exists <- doesFileExist filePath
  if exists
    then do
      hashObjectAtHEAD <- getHashObjectAtHEAD filePath
      hashObject <- getHashObject filePath
      return $
        if isNothing hashObjectAtHEAD
          then NEW
          else
            if hashObjectAtHEAD /= hashObject
              then CHANGED
              else UNCHANGED
    else return NOT_EXIST

mkBeamTable :: FilePath -> AppConfigs -> StorageRead -> [TableDef] -> IO ()
mkBeamTable yamlPath appConfigs storageRead tableDefs = do
  let filePath = appConfigs ^. output . beamTable
      defaultImportsFromConfig = getGeneratorDefaultImports appConfigs BEAM_TABLE
      generateBeamTable' = generateBeamTable defaultImportsFromConfig storageRead
  mapM_ (\t -> writeToFile yamlPath filePath (tableNameHaskell t ++ ".hs") (show $ generateBeamTable' t)) tableDefs

mkBeamQueries :: FilePath -> AppConfigs -> StorageRead -> [TableDef] -> IO ()
mkBeamQueries yamlPath appConfigs storageRead tableDefs = do
  let defaultFilePath = appConfigs ^. output . beamQueries
      extraFilePath = appConfigs ^. output . extraBeamQueries
      defaultImportsFromConfig = getGeneratorDefaultImports appConfigs BEAM_QUERIES
  mapM_
    ( \t -> do
        let beamQ = generateBeamQueries defaultImportsFromConfig storageRead t
        case beamQ of
          DefaultQueryFile (DefaultQueryCode {..}) -> do
            writeToFile yamlPath defaultFilePath (tableNameHaskell t ++ ".hs") (show readOnlyCode)
            when (isJust transformerCode) $ writeToFileIfNotExists yamlPath (extraFilePath </> "Transformers") (tableNameHaskell t ++ ".hs") (show $ fromJust transformerCode)
          WithExtraQueryFile (ExtraQueryCode {..}) -> do
            writeToFile yamlPath defaultFilePath (tableNameHaskell t ++ ".hs") (show (readOnlyCode defaultCode))
            writeToFile yamlPath (defaultFilePath </> "OrphanInstances") (tableNameHaskell t ++ ".hs") (show instanceCode)
            when (isJust $ transformerCode defaultCode) $ writeToFileIfNotExists yamlPath (extraFilePath </> "Transformers") (tableNameHaskell t ++ ".hs") (show $ fromJust (transformerCode defaultCode))
            writeToFileIfNotExists yamlPath extraFilePath (tableNameHaskell t ++ "Extra.hs") (show extraQueryFile)
    )
    tableDefs

mkCachedQueries :: FilePath -> AppConfigs -> StorageRead -> [TableDef] -> IO ()
mkCachedQueries yamlPath appConfigs storageRead tableDefs = do
  let defaultFilePath = appConfigs ^. output . NammaDSL.Config.cachedQueries
      extraFilePath = appConfigs ^. output . extraCachedQueries
      defaultImportsFromConfig = getGeneratorDefaultImports appConfigs CACHED_QUERIES
  mapM_
    ( \t -> do
        let cachedQ' = generateCachedQueries defaultImportsFromConfig storageRead t
        whenJust cachedQ' $ \cachedQ ->
          case cachedQ of
            DefaultCachedQueryFile (DefaultCachedQueryCode {..}) -> do
              writeToFile yamlPath defaultFilePath (tableNameHaskell t ++ ".hs") (show creadOnlyCode)
            WithExtraCachedQueryFile (ExtraCachedQueryCode {..}) -> do
              writeToFile yamlPath defaultFilePath (tableNameHaskell t ++ ".hs") (show (creadOnlyCode cdefaultCode))
              writeToFileIfNotExists yamlPath extraFilePath (tableNameHaskell t ++ "Extra.hs") (show cextraQueryFile)
    )
    tableDefs

mkDomainType :: FilePath -> AppConfigs -> StorageRead -> [TableDef] -> IO ()
mkDomainType yamlPath appConfigs storageRead tableDefs = do
  let filePath = appConfigs ^. output . domainType
      extraFilePath = (replace "src-read-only" "src" filePath) </> "Extra"
      defaultImportsFromConfig = getGeneratorDefaultImports appConfigs DOMAIN_TYPE
      generateDomainType' = generateDomainType defaultImportsFromConfig storageRead
  mapM_
    ( \t -> do
        let genCode = generateDomainType' t
            defaultCode = domainTypeDefaultCode genCode
            extraCode = domainTypeExtraCode genCode
        writeToFile yamlPath filePath (tableNameHaskell t ++ ".hs") (show $ defaultCode)
        case extraCode of
          Just code -> writeToFileIfNotExists yamlPath extraFilePath (tableNameHaskell t ++ ".hs") (show code)
          Nothing -> return ()
    )
    tableDefs

mkSQLFile :: FilePath -> AppConfigs -> StorageRead -> [TableDef] -> IO ()
mkSQLFile _ appConfigs _storageRead tableDefs = do
  let filePathAndDatabase = appConfigs ^. output . sql
      sqlMapper = appConfigs ^. storageConfig . sqlTypeMapper
  mapM_
    ( \t -> do
        let filename = (tableNameSql t ++ ".sql")
        mapM_
          ( \(filePath', database') -> do
              mbOldMigrationFile <- getOldSqlFile sqlMapper database' $ filePath' </> filename
              writeToFileWithoutSource filePath' filename (generateSQL database' mbOldMigrationFile t)
          )
          filePathAndDatabase
    )
    tableDefs

mkServantAPI :: FilePath -> AppConfigs -> ApiRead -> Apis -> IO ()
mkServantAPI yamlPath appConfigs apiRead apiDef = do
  let filePath = appConfigs ^. output . servantApi
      defaultImportsFromConfig = getGeneratorDefaultImports appConfigs SERVANT_API
      generateServantAPI' = generateServantAPI defaultImportsFromConfig apiRead
  writeToFile yamlPath filePath (T.unpack (_moduleName apiDef) ++ ".hs") (show $ generateServantAPI' apiDef)

mkApiTypes :: FilePath -> AppConfigs -> ApiRead -> Apis -> IO ()
mkApiTypes yamlPath appConfigs apiRead apiDef = do
  let filePath = appConfigs ^. output . apiRelatedTypes
      defaultImportsFromConfig = getGeneratorDefaultImports appConfigs API_TYPES
      generateApiTypes' = generateApiTypes defaultImportsFromConfig apiRead
  when (isApiExtraTypesPresent apiDef) $ writeToFile yamlPath filePath (T.unpack (_moduleName apiDef) ++ ".hs") (show $ generateApiTypes' apiDef)

mkDomainHandler :: FilePath -> AppConfigs -> ApiRead -> Apis -> IO ()
mkDomainHandler yamlPath appConfigs apiRead apiDef = do
  let fileName = T.unpack (_moduleName apiDef) ++ ".hs"
      filePath = appConfigs ^. output . domainHandler
      defaultImportsFromConfig = getGeneratorDefaultImports appConfigs DOMAIN_HANDLER
      generateDomainHandler' = generateDomainHandler defaultImportsFromConfig apiRead
  fileExists <- doesFileExist (filePath </> fileName)
  unless fileExists $ writeToFile yamlPath filePath fileName (show $ generateDomainHandler' apiDef)

mkFrontendAPIIntegration :: FilePath -> AppConfigs -> ApiRead -> Apis -> IO ()
mkFrontendAPIIntegration yamlPath appConfigs _apiRead apiDef = do
  let filePath = appConfigs ^. output . purescriptFrontend
  writeToFile yamlPath filePath (T.unpack (_moduleName apiDef) ++ ".purs") (generateAPIIntegrationCode apiDef)
