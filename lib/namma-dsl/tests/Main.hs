{-# LANGUAGE QuasiQuotes #-}

module Main where

import Kernel.Prelude
import NammaDSL.App
import NammaDSL.Types as NT
import System.Directory (createDirectoryIfMissing)

storageYamlFilePath :: FilePath
storageYamlFilePath = "./tests/storage.yaml"

apiYamlFilePath :: FilePath
apiYamlFilePath = "./tests/api.yaml"

generateAllExample :: IO ()
generateAllExample = do
  mapM_ (createDirectoryIfMissing True) ["./output/Storage/Beam", "./output/Storage/Queries", "./output/Domain/Types", "./output/Storage/SQL", "./output/API/Servant", "./output/Domain", "./output/API/Types"]
  mkBeamTable "./output/Storage/Beam" storageYamlFilePath -- Beam Table
  mkBeamQueries "./output/Storage/Queries" storageYamlFilePath -- Beam Queries
  mkDomainType "./output/Domain/Types" storageYamlFilePath -- Domain Types
  mkServantAPI "./output/API/Servant" apiYamlFilePath -- Servant Logic Handler
  mkDomainHandler "./output/Domain" apiYamlFilePath -- Domain Logic Handler
  mkApiTypes "./output/API/Types" apiYamlFilePath -- API Types
  mkSQLFile "./output/Storage/SQL" storageYamlFilePath -- SQL File

main :: IO ()
main = pure ()

-- let config =
--       NT.NammaDSLConfig
--         { api =
--             NT.ApiConfigs
--               { inputPath = "/Users/nisarg.trivedi/juspay/namma-dsl/sample-yaml/API/",
--                 outputPath = "api/output",
--                 enabled = True
--               },
--           storage =
--             NT.StorageConfigs
--               { inputPath = "/Users/nisarg.trivedi/juspay/namma-dsl/sample-yaml/Storage/",
--                 outputPath = "storage/output",
--                 sqlOutputPath = "storage/sql",
--                 enabled = True
--               }
--         }
-- runGeneration config
