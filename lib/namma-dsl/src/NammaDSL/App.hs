{-# LANGUAGE QuasiQuotes #-}

module NammaDSL.App where

import Control.Lens ((^.))
import Control.Monad (unless, when)
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
import Prelude

version :: String
version = "1.0.28"

runStorageGenerator :: FilePath -> FilePath -> IO ()
runStorageGenerator configPath yamlPath = do
  config <- fetchDhallConfig configPath
  let modulePrefix tp = haskellModuleNameFromFilePath (config ^. output . tp)
      storageRead =
        StorageRead
          { domainTypeModulePrefix = modulePrefix domainType,
            beamTypeModulePrefix = modulePrefix beamTable,
            queryModulePrefix = modulePrefix beamQueries,
            sqlMapper = config ^. storageConfig . sqlTypeMapper,
            extraDefaultFields = _extraDefaultFields (config ^. storageConfig),
            storageDefaultTypeImportMapper = config ^. defaultTypeImportMapper
          }
  tableDefs <- storageParser storageRead yamlPath
  let when' = \(t, f) -> when (elem t (config ^. generate)) $ f config storageRead tableDefs
  mapM_
    when'
    [ (DOMAIN_TYPE, mkDomainType),
      (BEAM_TABLE, mkBeamTable),
      (BEAM_QUERIES, mkBeamQueries),
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
  let when' = \(t, f) -> when (elem t (config ^. generate)) $ f config apiRead apiDef
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

mkBeamTable :: AppConfigs -> StorageRead -> [TableDef] -> IO ()
mkBeamTable appConfigs storageRead tableDefs = do
  let filePath = appConfigs ^. output . beamTable
      defaultImportsFromConfig = getGeneratorDefaultImports appConfigs BEAM_TABLE
      generateBeamTable' = generateBeamTable defaultImportsFromConfig storageRead
  mapM_ (\t -> writeToFile filePath (tableNameHaskell t ++ ".hs") (show $ generateBeamTable' t)) tableDefs

mkBeamQueries :: AppConfigs -> StorageRead -> [TableDef] -> IO ()
mkBeamQueries appConfigs storageRead tableDefs = do
  let defaultFilePath = appConfigs ^. output . beamQueries
      extraFilePath = appConfigs ^. output . extraBeamQueries
      defaultImportsFromConfig = getGeneratorDefaultImports appConfigs BEAM_QUERIES
  mapM_
    ( \t -> do
        let beamQ = generateBeamQueries defaultImportsFromConfig storageRead t
        case beamQ of
          DefaultQueryFile (DefaultQueryCode {..}) -> do
            writeToFile defaultFilePath (tableNameHaskell t ++ ".hs") (show readOnlyCode)
            when (isJust transformerCode) $ writeToFileIfNotExists (extraFilePath </> "Transformers") (tableNameHaskell t ++ ".hs") (show $ fromJust transformerCode)
          WithExtraQueryFile (ExtraQueryCode {..}) -> do
            writeToFile defaultFilePath (tableNameHaskell t ++ ".hs") (show (readOnlyCode defaultCode))
            writeToFile (defaultFilePath </> "OrphanInstances") (tableNameHaskell t ++ ".hs") (show instanceCode)
            when (isJust $ transformerCode defaultCode) $ writeToFileIfNotExists (extraFilePath </> "Transformers") (tableNameHaskell t ++ ".hs") (show $ fromJust (transformerCode defaultCode))
            writeToFileIfNotExists extraFilePath (tableNameHaskell t ++ "Extra.hs") (show extraQueryFile)
    )
    tableDefs

mkDomainType :: AppConfigs -> StorageRead -> [TableDef] -> IO ()
mkDomainType appConfigs storageRead tableDefs = do
  let filePath = appConfigs ^. output . domainType
      extraFilePath = (replace "src-read-only" "src" filePath) </> "Extra"
      defaultImportsFromConfig = getGeneratorDefaultImports appConfigs DOMAIN_TYPE
      generateDomainType' = generateDomainType defaultImportsFromConfig storageRead
  mapM_
    ( \t -> do
        let genCode = generateDomainType' t
            defaultCode = domainTypeDefaultCode genCode
            extraCode = domainTypeExtraCode genCode
        writeToFile filePath (tableNameHaskell t ++ ".hs") (show $ defaultCode)
        case extraCode of
          Just code -> writeToFileIfNotExists extraFilePath (tableNameHaskell t ++ ".hs") (show code)
          Nothing -> return ()
    )
    tableDefs

mkSQLFile :: AppConfigs -> StorageRead -> [TableDef] -> IO ()
mkSQLFile appConfigs _storageRead tableDefs = do
  let filePathAndDatabase = appConfigs ^. output . sql
      sqlMapper = appConfigs ^. storageConfig . sqlTypeMapper
  mapM_
    ( \t -> do
        let filename = (tableNameSql t ++ ".sql")
        mapM_
          ( \(filePath', database') -> do
              mbOldMigrationFile <- getOldSqlFile sqlMapper database' $ filePath' </> filename
              writeToFile filePath' filename (generateSQL database' mbOldMigrationFile t)
          )
          filePathAndDatabase
    )
    tableDefs

mkServantAPI :: AppConfigs -> ApiRead -> Apis -> IO ()
mkServantAPI appConfigs apiRead apiDef = do
  let filePath = appConfigs ^. output . servantApi
      defaultImportsFromConfig = getGeneratorDefaultImports appConfigs SERVANT_API
      generateServantAPI' = generateServantAPI defaultImportsFromConfig apiRead
  writeToFile filePath (T.unpack (_moduleName apiDef) ++ ".hs") (show $ generateServantAPI' apiDef)

mkApiTypes :: AppConfigs -> ApiRead -> Apis -> IO ()
mkApiTypes appConfigs apiRead apiDef = do
  let filePath = appConfigs ^. output . apiRelatedTypes
      defaultImportsFromConfig = getGeneratorDefaultImports appConfigs API_TYPES
      generateApiTypes' = generateApiTypes defaultImportsFromConfig apiRead
  when (isApiExtraTypesPresent apiDef) $ writeToFile filePath (T.unpack (_moduleName apiDef) ++ ".hs") (show $ generateApiTypes' apiDef)

mkDomainHandler :: AppConfigs -> ApiRead -> Apis -> IO ()
mkDomainHandler appConfigs apiRead apiDef = do
  let fileName = T.unpack (_moduleName apiDef) ++ ".hs"
      filePath = appConfigs ^. output . domainHandler
      defaultImportsFromConfig = getGeneratorDefaultImports appConfigs DOMAIN_HANDLER
      generateDomainHandler' = generateDomainHandler defaultImportsFromConfig apiRead
  fileExists <- doesFileExist (filePath </> fileName)
  unless fileExists $ writeToFile filePath fileName (show $ generateDomainHandler' apiDef)

mkFrontendAPIIntegration :: AppConfigs -> ApiRead -> Apis -> IO ()
mkFrontendAPIIntegration appConfigs _apiRead apiDef = do
  let filePath = appConfigs ^. output . purescriptFrontend
  writeToFile filePath (T.unpack (_moduleName apiDef) ++ ".purs") (generateAPIIntegrationCode apiDef)
