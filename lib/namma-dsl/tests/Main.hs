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
  mapM_ (createDirectoryIfMissing True) ["./output/Storage/Beam", "./output/Storage/Queries", "./output/Domain/Types", "./output/Storage/SQL"]
  mkBeamTable "./output/Storage/Beam" storageYamlFilePath -- Beam Table
  mkBeamQueries "./output/Storage/Queries" storageYamlFilePath -- Beam Queries
  mkDomainType "./output/Domain/Types" storageYamlFilePath -- Domain Types
  mkSQLFile "./output/Storage/SQL" storageYamlFilePath -- SQL File
  mkServantAPI "./output/API/Servant" apiYamlFilePath -- Servant Logic Handler
  mkDomainHandler "./output/Domain" apiYamlFilePath -- Domain Logic Handler
  mkApiTypes "./output/API/Types" apiYamlFilePath -- API Types

main :: IO ()
main = generateAllExample
