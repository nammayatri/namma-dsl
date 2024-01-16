{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module NammaDSL.App where

import qualified Data.Text as T
import Kernel.Prelude
import NammaDSL.DSL.Parser.API
import NammaDSL.DSL.Parser.Storage
import NammaDSL.DSL.Syntax.API
import NammaDSL.DSL.Syntax.Storage
import NammaDSL.Generator.Haskell
import NammaDSL.Generator.Haskell.ApiTypes
import NammaDSL.Generator.Purs
import NammaDSL.Generator.SQL
import qualified NammaDSL.Types as NT
import NammaDSL.Utils
import System.Directory
import System.FilePath

mkBeamTable :: FilePath -> FilePath -> IO ()
mkBeamTable filePath yaml = do
  tableDef <- storageParser yaml
  mapM_ (\t -> writeToFile filePath (tableNameHaskell t ++ ".hs") (show $ generateBeamTable t)) tableDef

mkBeamQueries :: FilePath -> FilePath -> IO ()
mkBeamQueries filePath yaml = do
  tableDef <- storageParser yaml
  mapM_ (\t -> writeToFile filePath (tableNameHaskell t ++ ".hs") (show $ generateBeamQueries t)) tableDef

mkDomainType :: FilePath -> FilePath -> IO ()
mkDomainType filePath yaml = do
  tableDef <- storageParser yaml
  mapM_ (\t -> writeToFile filePath (tableNameHaskell t ++ ".hs") (show $ generateDomainType t)) tableDef

mkSQLFile :: FilePath -> FilePath -> IO ()
mkSQLFile filePath yaml = do
  tableDef <- storageParser yaml
  mapM_
    ( \t -> do
        let filename = (tableNameSql t ++ ".sql")
        mbOldMigrationFile <- getOldSqlFile $ filePath </> filename
        writeToFile filePath filename (generateSQL mbOldMigrationFile t)
    )
    tableDef

mkServantAPI :: FilePath -> FilePath -> IO ()
mkServantAPI filePath yaml = do
  apiDef <- apiParser yaml
  writeToFile filePath (T.unpack (_moduleName apiDef) ++ ".hs") (show $ generateServantAPI apiDef)

mkApiTypes :: FilePath -> FilePath -> IO ()
mkApiTypes filePath yaml = do
  apiDef <- apiParser yaml
  writeToFile filePath (T.unpack (_moduleName apiDef) ++ ".hs") (show $ generateApiTypes apiDef)

mkDomainHandler :: FilePath -> FilePath -> IO ()
mkDomainHandler filePath yaml = do
  apiDef <- apiParser yaml
  let fileName = T.unpack (_moduleName apiDef) ++ ".hs"
  fileExists <- doesFileExist (filePath </> fileName)
  unless fileExists $ writeToFile filePath fileName (show $ generateDomainHandler apiDef)

mkFrontendAPIIntegration :: FilePath -> FilePath -> IO ()
mkFrontendAPIIntegration filePath yaml = do
  apiDef <- apiParser yaml
  writeToFile filePath (T.unpack (_moduleName apiDef) ++ ".purs") (generateAPIIntegrationCode apiDef)

runGeneration :: NT.NammaDSLConfig -> IO ()
runGeneration nammaDslConfigs = do
  currentDir <- getCurrentDirectory
  maybeGitRoot <- findGitRoot currentDir
  let rootDir = fromMaybe (error "Could not find git root") maybeGitRoot
  let storageConfig = NT.storage nammaDslConfigs
  let apiConfig = NT.api nammaDslConfigs

  applyDirectory (rootDir </> ((NT.inputPath :: NT.ApiConfigs -> String) apiConfig)) (processAPIDSL rootDir apiConfig)
  applyDirectory (rootDir </> ((NT.inputPath :: NT.StorageConfigs -> String) storageConfig)) (processStorageDSL rootDir storageConfig)
  where
    processStorageDSL rootDir storageConfig inputFile = do
      mkBeamTable (rootDir </> ((NT.outputPath :: NT.StorageConfigs -> String) storageConfig) </> "Storage/Beam") inputFile
      mkBeamQueries (rootDir </> ((NT.outputPath :: NT.StorageConfigs -> String) storageConfig) </> "Storage/Queries") inputFile
      mkDomainType (rootDir </> ((NT.outputPath :: NT.StorageConfigs -> String) storageConfig) </> "Domain/Types") inputFile
    -- mkSQLFile (rootDir </> (NT.sqlOutputPath storageConfig)) inputFile

    processAPIDSL rootDir apiConfig inputFile = do
      -- NammaDSL.mkFrontendAPIIntegration (readOnlySrc </> "Domain/Action") inputFile
      mkServantAPI (rootDir </> ((NT.outputPath :: NT.ApiConfigs -> String) apiConfig) </> "API/Action/UI") inputFile
      mkApiTypes (rootDir </> ((NT.outputPath :: NT.ApiConfigs -> String) apiConfig) </> "API/Types/UI") inputFile
      mkDomainHandler (rootDir </> ((NT.outputPath :: NT.ApiConfigs -> String) apiConfig) </> "Domain/Action/UI") inputFile
