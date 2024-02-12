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
import System.Process (readProcess)

version :: String
version = "1.0.6"

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

mkBeamTable :: FilePath -> FilePath -> IO ()
mkBeamTable filePath yaml = do
  tableDef <- storageParser yaml
  mapM_ (\t -> writeToFile filePath (tableNameHaskell t ++ ".hs") (show $ generateBeamTable t)) tableDef

mkBeamQueries :: FilePath -> Maybe FilePath -> FilePath -> IO ()
mkBeamQueries defaultFilePath extraFilePath' yaml = do
  let extraFilePath = fromMaybe defaultFilePath extraFilePath'
  tableDef <- storageParser yaml
  mapM_
    ( \t -> do
        let beamQ = generateBeamQueries t
        case beamQ of
          DefaultQueryFile (DefaultQueryCode {..}) -> do
            writeToFile defaultFilePath (tableNameHaskell t ++ ".hs") (show readOnlyCode)
            when (isJust transformerCode) $ writeToFile (extraFilePath </> "Transformers") (tableNameHaskell t ++ ".hs") (show $ fromJust transformerCode)
          WithExtraQueryFile (ExtraQueryCode {..}) -> do
            writeToFile defaultFilePath (tableNameHaskell t ++ ".hs") (show (readOnlyCode defaultCode))
            writeToFile (defaultFilePath </> "OrphanInstances") (tableNameHaskell t ++ ".hs") (show instanceCode)
            when (isJust $ transformerCode defaultCode) $ writeToFile (extraFilePath </> "Transformers") (tableNameHaskell t ++ ".hs") (show $ fromJust (transformerCode defaultCode))
            let extraFileName = tableNameHaskell t ++ "Extra.hs"
            extraFileExists <- doesFileExist (extraFilePath </> extraFileName)
            unless extraFileExists $ writeToFile extraFilePath (tableNameHaskell t ++ "Extra.hs") (show extraQueryFile)
    )
    tableDef

mkDomainType :: FilePath -> FilePath -> IO ()
mkDomainType filePath yaml = do
  tableDef <- storageParser yaml
  mapM_ (\t -> writeToFile filePath (tableNameHaskell t ++ ".hs") (show $ generateDomainType t)) tableDef

mkSQLFile :: Maybe String -> FilePath -> FilePath -> IO ()
mkSQLFile db filePath yaml = do
  let database = fromMaybe "atlas_app" db
  tableDef <- storageParser yaml
  mapM_
    ( \t -> do
        let filename = (tableNameSql t ++ ".sql")
        mbOldMigrationFile <- getOldSqlFile $ filePath </> filename
        writeToFile filePath filename (generateSQL database mbOldMigrationFile t)
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
