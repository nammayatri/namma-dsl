{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import NammaDSL.App
import NammaDSL.DSL.Parser.Storage (SQL_MANIPULATION, sqlCleanedLineParser)
import Prelude

storageYamlFilePath :: FilePath
storageYamlFilePath = "./tests/storage.yaml"

apiYamlFilePath :: FilePath
apiYamlFilePath = "./tests/api.yaml"

dashboardApiYamlFilePath :: FilePath
dashboardApiYamlFilePath = "./tests/dashboard-api.yaml"

generateAllExample :: IO ()
generateAllExample = do
  runStorageGenerator "./tests/dsl-config.dhall" storageYamlFilePath
  runApiGenerator "./tests/dsl-config.dhall" apiYamlFilePath
  runApiGenerator "./tests/dsl-config.dhall" dashboardApiYamlFilePath

sql :: String -> SQL_MANIPULATION -- Just for quick testing
sql = sqlCleanedLineParser

main :: IO ()
main = pure () -- generateAllExample
