{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import NammaDSL.App
import NammaDSL.DSL.Parser.Storage (SQL_MANIPULATION, sqlCleanedLineParser)
import Prelude

--import qualified NammaDSL.Generator.Purs.CST as PCST

--import qualified Data.Text as T

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
  runTechDesign "./tests/tech-design-config.dhall" "./tests/tech-design.yaml"

sql :: String -> SQL_MANIPULATION -- Just for quick testing
sql = sqlCleanedLineParser

main :: IO ()
main = runFrontendGenerator "./tests/frontend-config.dhall" "./tests/frontend.yaml"

-- PCST.viewModule "/Users/anirbandas/work/nWork/nnn/nammayatri/Frontend/ui-customer/src/Components/ChooseYourRide/View.purs"
-- runFrontendGenerator "./tests/frontend-config.dhall" "./tests/frontend.yaml"
--runTechDesign "./tests/tech-design-config.dhall" "./tests/tech-design.yaml"

--pure () -- generateAllExample
