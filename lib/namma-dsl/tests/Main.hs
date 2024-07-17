{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import qualified Data.Text.IO as T
import Language.PureScript.CST as P
import Language.PureScript.CST.Types ()
import NammaDSL.App
import qualified NammaDSL.Generator.Purs.CST as PCST
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

checkHypothesis :: FilePath -> IO ()
checkHypothesis pursFilePath = do
  contents <- T.readFile pursFilePath
  case snd (P.parse contents) of
    Left err -> print err
    Right md -> do
      putStrLn (show md)
      let newTypeName = "Colors"
          funcSigToAddComment = "feedbackBasedOnRatingView"
          commentToAdd = [Comment "-- This is a comment2", Line LF]
          newField = "bca"
          tp = "Array Int"
          newImports =
            [ PCST.PImport "A.B.C" PCST.Qualified,
              PCST.PImport "B.C.A" PCST.Simple,
              PCST.PImport "C.D.E" PCST.Qualified
            ]
          newDecls = PCST.addCmtUpDeclSig funcSigToAddComment commentToAdd $ PCST.findAndAddFieldToDecl newTypeName newField tp (modDecls md)
          newMd = PCST.addImports newImports $ md {modDecls = newDecls}
      T.writeFile pursFilePath (P.printModule newMd)

-- not sure if its okay to use this dummy range
dummyRange :: SourceRange
dummyRange = SourceRange {srcStart = SourcePos 0 0, srcEnd = SourcePos 0 0}

-- addAFieldToAModule :: Module () -> Row () -> Module ()

main :: IO ()
main = checkHypothesis "/Users/anirbandas/work/nWork/namma-dsl/lib/namma-dsl/tests/src-read-only/UI/Frontend/Reels.purs"
