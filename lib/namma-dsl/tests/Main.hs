{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import NammaDSL.App
import Prelude

storageYamlFilePath :: FilePath
storageYamlFilePath = "./tests/storage.yaml"

apiYamlFilePath :: FilePath
apiYamlFilePath = "./tests/api.yaml"

pursFilePath :: FilePath
pursFilePath = "./tests/purs.yaml"

generateAllExample :: IO ()
generateAllExample = do
  runStorageGenerator "./tests/dsl-config.dhall" storageYamlFilePath
  runApiGenerator "./tests/dsl-config.dhall" apiYamlFilePath

testGenerator :: IO ()
testGenerator = do
  runPursGenerator pursFilePath
  putStrLn "Testing this"

-- runningTheAnalysis :: IO ()
-- runningTheAnalysis = do
--   let initialState =
--         AnalysisState
--           { rootPathPrefix = ["/Users/anirbandas/work/nWork/namma-dsl/lib/namma-dsl/src"],
--             extImports = KM.fromList [("A.B.BLA", "Kernel.Prelude.BBB2")],
--             haskellImports = KM.fromList [("BLA", "Kernel.Prelude.Bla2")],
--             dTypes = [],
--             alreadyNoticedDeepA = mempty,
--             currentQualifiedImports = [],
--             primitives = pursTypePrimitive,
--             tpTinkerer = id,
--             remaining = [("NammaDSL.DSL.Syntax.Storage", "PROXY_API_TYPE_2"), ("NammaDSL.DSL.Syntax.Storage", "PROXY_API_TYPE")],
--             remainingEXT_TO = [],
--             result = []
--           }
--   rr <- execStateT deepAnalysis initialState
--   print (result rr)

-- sql :: String -> SQL_MANIPULATION
-- sql = sqlCleanedLineParser

main :: IO ()
main = testGenerator
