{-# LANGUAGE QuasiQuotes #-}

module NammaDSL.App (module NammaDSL.App, module ReExport) where

import Control.Lens ((.~), (^.))
import Control.Monad (unless, when)
import Control.Monad.Extra (whenJust)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (isInfixOf)
import Data.List.Extra (replace)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import qualified Data.Text as T
import NammaDSL.Config
import NammaDSL.DSL.Parser.API
import NammaDSL.DSL.Parser.Storage
import NammaDSL.DSL.Parser.Storage.KVConstraints
import NammaDSL.DSL.Syntax.API
import NammaDSL.DSL.Syntax.Common as ReExport
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
version = "1.0.84"

runStorageGenerator :: FilePath -> FilePath -> IO ()
runStorageGenerator configPath yamlPath = do
  config <- fetchDhallConfig configPath
  fileStatus <- getFileState yamlPath
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
            defaultCachedQueryKeyPfx = config ^. storageConfig . defaultCachedQueryKeyPrefix,
            srcFileStatus = fileStatus,
            storagePackageMapping = config ^. packageMapping
          }
  tableDefs <- storageParser storageRead yamlPath
  checkKVConstraints yamlPath tableDefs
  let when' = \(t, f) -> when (elem t (config ^. generate)) $ f config storageRead tableDefs
  mapM_
    when'
    [ (SQL, mkSQLFile),
      (DOMAIN_TYPE, mkDomainType),
      (BEAM_TABLE, mkBeamTable),
      (BEAM_QUERIES, mkBeamQueries),
      (CACHED_QUERIES, mkCachedQueries)
    ]

runApiGenerator :: FilePath -> FilePath -> IO ()
runApiGenerator configPath yamlPath = do
  config <- fetchDhallConfig configPath
  let modulePrefix tp = haskellModuleNameFromFilePath (config ^. output . tp)
  let apiRead =
        ApiRead
          { apiTypesImportPrefix = modulePrefix apiRelatedTypes,
            extraApiTypesImportPrefix = modulePrefix extraApiRelatedTypes,
            extraApiCommonTypesImportPrefix = modulePrefix extraApiRelatedCommonTypes,
            apiServantImportPrefix = modulePrefix servantApi,
            apiServantDashboardImportPrefix = modulePrefix servantApiDashboard,
            apiDomainHandlerImportPrefix = modulePrefix domainHandler,
            apiDomainHandlerDashboardImportPrefix = modulePrefix domainHandlerDashboard,
            apiClientImportPrefix = modulePrefix servantApiClient,
            apiDefaultTypeImportMapper = config ^. defaultTypeImportMapper,
            apiServerName = config ^. serverName,
            apiReadKind = config ^. apiKind,
            apiEndpointPrefix = config ^. endpointPrefix,
            apiFolderName = config ^. folderName,
            apiMigrationParams = config ^. migrationParams,
            apiPackageMapping = config ^. packageMapping
          }
  (apiDef, apiDefApiTypes) <- apiParser' apiRead yamlPath
  let when' = \(t, f) -> when (elem t (config ^. generate)) $ f config apiRead (if t == API_TYPES then apiDefApiTypes else apiDef)
  mapM_
    when'
    [ (SQL, mkApiSQLFile),
      (SERVANT_API, mkServantAPI),
      (SERVANT_API_DASHBOARD, mkServantAPIDashboard),
      (API_TYPES, mkApiTypes),
      (DOMAIN_HANDLER, mkDomainHandler),
      (DOMAIN_HANDLER_DASHBOARD, mkDomainHandlerDashboard),
      (PURE_SCRIPT_FRONTEND, mkFrontendAPIIntegration)
    ]

runApiTreeGenerator :: FilePath -> [String] -> IO ()
runApiTreeGenerator configPath specModules = do
  config <- fetchDhallConfig configPath
  let modulePrefix tp = haskellModuleNameFromFilePath (config ^. output . tp)
  let apiRead =
        ApiRead
          { apiTypesImportPrefix = modulePrefix apiRelatedTypes,
            extraApiTypesImportPrefix = modulePrefix extraApiRelatedTypes,
            extraApiCommonTypesImportPrefix = modulePrefix extraApiRelatedCommonTypes,
            apiServantImportPrefix = modulePrefix servantApi,
            apiServantDashboardImportPrefix = modulePrefix servantApiDashboard,
            apiDomainHandlerImportPrefix = modulePrefix domainHandler,
            apiDomainHandlerDashboardImportPrefix = modulePrefix domainHandlerDashboard,
            apiClientImportPrefix = modulePrefix servantApiClient,
            apiDefaultTypeImportMapper = config ^. defaultTypeImportMapper,
            apiServerName = config ^. serverName,
            apiReadKind = config ^. apiKind,
            apiEndpointPrefix = config ^. endpointPrefix,
            apiFolderName = config ^. folderName,
            apiMigrationParams = config ^. migrationParams,
            apiPackageMapping = config ^. packageMapping
          }
  let when' = \(t, f) -> when (elem t (config ^. generate)) $ f config apiRead (ApiTree {specModules})
  mapM_
    when'
    [ (API_TREE, mkAPITree),
      (API_TREE_DASHBOARD, mkAPITreeDashboard),
      (API_TREE_COMMON, mkAPITreeCommon),
      (API_TREE_CLIENT, mkAPITreeClient)
    ]

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

mkCachedQueries :: AppConfigs -> StorageRead -> [TableDef] -> IO ()
mkCachedQueries appConfigs storageRead tableDefs = do
  let defaultFilePath = appConfigs ^. output . NammaDSL.Config.cachedQueries
      extraFilePath = appConfigs ^. output . extraCachedQueries
      defaultImportsFromConfig = getGeneratorDefaultImports appConfigs CACHED_QUERIES
  mapM_
    ( \t -> do
        let cachedQ' = generateCachedQueries defaultImportsFromConfig storageRead t
        whenJust cachedQ' $ \cachedQ ->
          case cachedQ of
            DefaultCachedQueryFile (DefaultCachedQueryCode {..}) -> do
              writeToFile defaultFilePath (tableNameHaskell t ++ ".hs") (show creadOnlyCode)
            WithExtraCachedQueryFile (ExtraCachedQueryCode {..}) -> do
              writeToFile defaultFilePath (tableNameHaskell t ++ ".hs") (show (creadOnlyCode cdefaultCode))
              writeToFileIfNotExists extraFilePath (tableNameHaskell t ++ "Extra.hs") (show cextraQueryFile)
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
              --print mbOldMigrationFile
              let contents = generateSQL database' mbOldMigrationFile t
              case contents of
                Right content -> unless (null content) $ writeToFile filePath' filename content
                Left err -> error err
          )
          filePathAndDatabase
    )
    tableDefs

data FilePathAndDatabase = FilePathAndDatabase
  { filePath :: FilePath,
    database :: String,
    fileName :: FilePath,
    isLocal :: Bool
  }

mkApiSQLFile :: AppConfigs -> ApiRead -> Apis -> IO ()
mkApiSQLFile appConfigs apiRead apiDef = do
  when (appConfigs ^. apiKind == DASHBOARD) $ do
    let folderName' = fromMaybe (error "Folder name required for api migrations") $ apiFolderName apiRead
    let fileName' = "API_" <> folderName' <> "_" <> T.unpack (apiDef ^. moduleName) ++ ".sql"
    let localFileName = "Local_" <> fileName'
    let filePathsAndDatabase =
          concat $
            (appConfigs ^. output . sql) <&> \(filePath, database) -> do
              [ FilePathAndDatabase {filePath, database, fileName = fileName', isLocal = False},
                FilePathAndDatabase {filePath, database, fileName = localFileName, isLocal = True}
                ]

    mapM_
      ( \FilePathAndDatabase {..} -> do
          mbOldMigrationFile <- getOldApiSqlFile $ filePath </> fileName
          let contents = generateApiSQL database mbOldMigrationFile isLocal apiRead apiDef
          case contents of
            Right content -> unless (null content) $ writeToFile filePath fileName content
            Left err -> error err
      )
      filePathsAndDatabase

mkServantAPI :: AppConfigs -> ApiRead -> Apis -> IO ()
mkServantAPI appConfigs apiRead apiDef = do
  let filePath = appConfigs ^. output . servantApi
      defaultImportsFromConfig = getGeneratorDefaultImports appConfigs SERVANT_API
      generateServantAPI' = generateServantAPI defaultImportsFromConfig apiRead
  writeToFile filePath (T.unpack (_moduleName apiDef) ++ ".hs") (show $ generateServantAPI' apiDef)

mkServantAPIDashboard :: AppConfigs -> ApiRead -> Apis -> IO ()
mkServantAPIDashboard appConfigs apiRead apiDef = do
  let filePath = appConfigs ^. output . servantApiDashboard
      defaultImportsFromConfig = getGeneratorDefaultImports appConfigs SERVANT_API_DASHBOARD
      generateServantAPIDashboard' = generateServantAPIDashboard defaultImportsFromConfig apiRead
  writeToFile filePath (T.unpack (_moduleName apiDef) ++ ".hs") (show $ generateServantAPIDashboard' apiDef)

mkAPITree :: AppConfigs -> ApiRead -> ApiTree -> IO ()
mkAPITree appConfigs apiRead apiTree = do
  let filePath = appConfigs ^. output . servantApi ++ ".hs"
      defaultImportsFromConfig = getGeneratorDefaultImports appConfigs API_TREE
      generateAPITree' = generateAPITree defaultImportsFromConfig apiRead
  writeToFile' filePath (show $ generateAPITree' apiTree)

mkAPITreeDashboard :: AppConfigs -> ApiRead -> ApiTree -> IO ()
mkAPITreeDashboard appConfigs apiRead apiTree = do
  let filePath = appConfigs ^. output . servantApiDashboard ++ ".hs"
      defaultImportsFromConfig = getGeneratorDefaultImports appConfigs API_TREE_DASHBOARD
      generateAPITreeDashboard' = generateAPITreeDashboard defaultImportsFromConfig apiRead
  writeToFile' filePath (show $ generateAPITreeDashboard' apiTree)

mkAPITreeCommon :: AppConfigs -> ApiRead -> ApiTree -> IO ()
mkAPITreeCommon appConfigs apiRead apiTree = do
  let filePath = appConfigs ^. output . apiRelatedTypes ++ ".hs"
      defaultImportsFromConfig = getGeneratorDefaultImports appConfigs API_TREE_COMMON
      generateAPITreeCommon' = generateAPITreeCommon defaultImportsFromConfig apiRead
  writeToFile' filePath (show $ generateAPITreeCommon' apiTree)

mkAPITreeClient :: AppConfigs -> ApiRead -> ApiTree -> IO ()
mkAPITreeClient appConfigs apiRead apiTree = do
  let filePath = appConfigs ^. output . servantApiClient ++ ".hs"
      defaultImportsFromConfig = getGeneratorDefaultImports appConfigs API_TREE_CLIENT
      generateAPITreeClient' = generateAPITreeClient defaultImportsFromConfig apiRead
  writeToFile' filePath (show $ generateAPITreeClient' apiTree)

mkApiTypes :: AppConfigs -> ApiRead -> Apis -> IO ()
mkApiTypes appConfigs apiRead apiDef = do
  let isDashboardGenerator = apiReadKind apiRead == DASHBOARD
      filePath = appConfigs ^. output . apiRelatedTypes <> if isDashboardGenerator then "/Endpoints" else ""
      reexportFilePath = appConfigs ^. output . apiRelatedTypes
      extraFilePath = appConfigs ^. output . extraApiRelatedTypes
      extraCommonFilePath = appConfigs ^. output . extraApiRelatedCommonTypes
      defaultImportsFromConfig = getGeneratorDefaultImports appConfigs API_TYPES
      ApiTypesCode {..} = generateApiTypes defaultImportsFromConfig apiRead apiDef
      fileName = T.unpack (_moduleName apiDef) ++ ".hs"
  when (isApiExtraTypesPresent apiDef || apiReadKind apiRead == DASHBOARD) $ do
    writeToFile filePath (T.unpack (_moduleName apiDef) ++ ".hs") (show apiTypesDefaultCode)
  whenJust reexportApiTypesCode $
    writeToFile reexportFilePath fileName . show
  whenJust apiTypesExtraCode $
    writeToFileIfNotExists extraFilePath fileName . show
  whenJust apiCommonTypesExtraCode $
    writeToFileIfNotExists extraCommonFilePath fileName . show

mkDomainHandler :: AppConfigs -> ApiRead -> Apis -> IO ()
mkDomainHandler appConfigs apiRead apiDef = do
  let fileName = T.unpack (_moduleName apiDef) ++ ".hs"
      filePath = appConfigs ^. output . domainHandler
      defaultImportsFromConfig = getGeneratorDefaultImports appConfigs DOMAIN_HANDLER
      generateDomainHandler' = generateDomainHandler defaultImportsFromConfig apiRead
  fileExists <- doesFileExist (filePath </> fileName)
  unless fileExists $ writeToFile filePath fileName (show $ generateDomainHandler' apiDef)

mkDomainHandlerDashboard :: AppConfigs -> ApiRead -> Apis -> IO ()
mkDomainHandlerDashboard appConfigs apiRead apiDef = do
  let fileName = T.unpack (_moduleName apiDef) ++ ".hs"
      filePath = appConfigs ^. output . domainHandlerDashboard
      defaultImportsFromConfig = getGeneratorDefaultImports appConfigs DOMAIN_HANDLER_DASHBOARD
      generateDomainHandlerDashboard' = generateDomainHandlerDashboard defaultImportsFromConfig apiRead
  fileExists <- doesFileExist (filePath </> fileName)
  unless fileExists $ writeToFile filePath fileName (show $ generateDomainHandlerDashboard' apiDef)
  let appendDomainHandler = True -- TODO make it optional
  when (fileExists && appendDomainHandler) $ do
    contents <- readFile (filePath </> fileName)
    let filteredApis = filter (not . fileContainsHandler contents) (apiDef ^. apis)
    let mbFilteredHandlersCode = mkCodeBodyDomainHandlerDashboard apiRead $ apiDef & apis .~ filteredApis
    whenJust mbFilteredHandlersCode $ \filteredHandlersCode -> do
      writeFile (filePath </> fileName) $ contents <> "\n\n--- << AUTOGENERATED Check this code, update export list and remove comment >> ---\n\n" <> filteredHandlersCode

fileContainsHandler :: String -> ApiTT -> Bool
fileContainsHandler contents apiTT = do
  let handlerName = T.unpack $ handlerFunctionText apiTT
  ("\n" <> handlerName <> " ::") `isInfixOf` contents

mkFrontendAPIIntegration :: AppConfigs -> ApiRead -> Apis -> IO ()
mkFrontendAPIIntegration appConfigs _apiRead apiDef = do
  let filePath = appConfigs ^. output . purescriptFrontend
  writeToFile filePath (T.unpack (_moduleName apiDef) ++ ".purs") (generateAPIIntegrationCode apiDef)
