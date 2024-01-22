{-# LANGUAGE QuasiQuotes #-}

module Main where

import Kernel.Prelude
import NammaDSL.App
import System.Directory (createDirectoryIfMissing)

storageYamlFilePath :: FilePath
storageYamlFilePath = "./tests/storage.yaml"

apiYamlFilePath :: FilePath
apiYamlFilePath = "./tests/api.yaml"

generateAllExample :: IO ()
generateAllExample = do
  mapM_ (createDirectoryIfMissing True) ["./output/Beam", "./output/src/Queries", "./output/Queries", "./output/Domain/Type", "./output/SQL"]
  mkBeamTable "./output/Beam" storageYamlFilePath
  mkBeamQueries "./output/Queries" (Just "./output/src/Queries") storageYamlFilePath
  mkDomainType "./output/Domain/Type" storageYamlFilePath
  mkSQLFile "./output/SQL" storageYamlFilePath
  mkServantAPI "./output" apiYamlFilePath
  mkDomainHandler "./output/Domain" apiYamlFilePath

main :: IO ()
main = pure ()
