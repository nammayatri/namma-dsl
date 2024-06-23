{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad.State
import NammaDSL.App
import NammaDSL.DSL.Parser.Storage (SQL_MANIPULATION, sqlCleanedLineParser)
import NammaDSL.Lib.Extractor
import Prelude

storageYamlFilePath :: FilePath
storageYamlFilePath = "./tests/storage.yaml"

apiYamlFilePath :: FilePath
apiYamlFilePath = "./tests/api.yaml"

generateAllExample :: IO ()
generateAllExample = do
  runStorageGenerator "./tests/dsl-config.dhall" storageYamlFilePath
  runApiGenerator "./tests/dsl-config.dhall" apiYamlFilePath

runningTheAnalysis :: IO ()
runningTheAnalysis = do
  let initialState =
        AnalysisState
          { rootPathPrefix = ["/Users/anirbandas/work/nWork/namma-dsl/lib/namma-dsl/src2", "/Users/anirbandas/work/nWork/namma-dsl/lib/namma-dsl/src"],
            extImports = mempty,
            rootModule = "NammaDSL.Lib.Extractor",
            haskellImports = mempty,
            dTypes = [],
            primitives = mempty,
            remaining = [("NammaDSL.Lib.Types", "CodeTree")],
            result = []
          }
  rr <- evalStateT (deepAnalysis ("NammaDSL.DSL.Syntax.Storage", "Spaces")) initialState
  print rr

sql :: String -> SQL_MANIPULATION
sql = sqlCleanedLineParser

main :: IO ()
main = runningTheAnalysis
