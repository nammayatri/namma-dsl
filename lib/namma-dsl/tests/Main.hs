{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad.State
import Data.Aeson ()
import qualified Data.Aeson.KeyMap as KM
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
            extImports = KM.fromList [("A.B.BLA", "Kernel.Prelude.BBB2")],
            rootModule = "NammaDSL.Lib.Extractor",
            haskellImports = KM.fromList [("BLA", "Kernel.Prelude.Bla2")],
            dTypes = [],
            alreadyNoticedDeepA = mempty,
            currentQualifiedImports = [],
            primitives = KM.fromList [("Int", "Purs.Int"), ("String", "Purs.String"), ("Maybe", "Purs.Maybe")],
            remaining = [("NammaDSL.DSL.Syntax.Storage", "PROXY_API_TYPE_2"), ("NammaDSL.DSL.Syntax.Storage", "PROXY_API_TYPE")],
            result = []
          }
  rr <- execStateT deepAnalysis initialState
  print (result rr)

sql :: String -> SQL_MANIPULATION
sql = sqlCleanedLineParser

main :: IO ()
main = runningTheAnalysis
