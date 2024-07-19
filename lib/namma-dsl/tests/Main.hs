{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import NammaDSL.App
import Prelude

--import qualified Data.Text as T

storageYamlFilePath :: FilePath
storageYamlFilePath = "./tests/storage.yaml"

apiYamlFilePath :: FilePath
apiYamlFilePath = "./tests/api.yaml"

generateAllExample :: IO ()
generateAllExample = do
  runStorageGenerator "./tests/dsl-config.dhall" storageYamlFilePath
  runApiGenerator "./tests/dsl-config.dhall" apiYamlFilePath
  runTechDesign "./tests/tech-design-config.yaml" "./tests/tech-design.yaml"

main :: IO ()
main = pure ()

-- runTechDesign "./tests/tech-design-config.dhall" "./tests/tech-design.yaml"
