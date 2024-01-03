{-# LANGUAGE QuasiQuotes #-}

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
import NammaDSL.Utils
import System.Directory
import System.FilePath
import Text.Show.Pretty (ppShow)

mkBeamTable :: FilePath -> FilePath -> IO ()
mkBeamTable filePath yaml = do
  tableDef <- storageParser yaml
  print $ ppShow tableDef
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
