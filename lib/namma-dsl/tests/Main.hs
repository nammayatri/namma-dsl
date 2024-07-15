{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import qualified Data.Text.IO as T
import Language.PureScript.CST as P
import Language.PureScript.CST.Types ()
import Language.PureScript.Names
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
      let newTypeName = "animConfig"
          funcSigToAddComment = "feedbackBasedOnRatingView"
          commentToAdd = [Comment "-- This is a comment", Line LF]
          newField = "abc"
          tp = "PrestoAnim.Bezier 0.37 1.0"
          newDecls = PCST.addCmtUpDeclSig funcSigToAddComment commentToAdd $ PCST.findAndAddFieldToDecl newTypeName newField tp (modDecls md)
          newMd = md {modDecls = newDecls}
      -- let maybeDecl = PCST.findDeclWithName "Event" (modDecls md)
      -- case maybeDecl of
      --   Just decl -> putStrLn (show decl)
      --   Nothing -> putStrLn "Did not find the declaration"
      T.writeFile pursFilePath (P.printModule newMd)

addImport :: Module () -> ImportDecl () -> Module ()
addImport mod' importDecl = mod' {modImports = modImports mod' ++ [importDecl]}

-- A new import declaration for
-- import Cheking.The.Hypothesis --
newImport :: ImportDecl ()
newImport =
  ImportDecl
    { impAnn = (),
      impKeyword =
        SourceToken
          { tokAnn =
              TokenAnn
                { tokRange = dummyRange,
                  tokLeadingComments = [Line LF],
                  tokTrailingComments = [Space 1]
                },
            tokValue = TokLowerName [] "import"
          },
      impModule =
        Name
          { nameTok =
              SourceToken
                { tokAnn =
                    TokenAnn
                      { tokRange = dummyRange,
                        tokLeadingComments = [],
                        tokTrailingComments = []
                      },
                  tokValue = TokUpperName [] "Cheking.The.Hypothesis"
                },
            nameValue = ModuleName ""
          },
      impNames = Nothing,
      impQual = Just (SourceToken {tokAnn = TokenAnn {tokRange = SourceRange {srcStart = SourcePos {srcLine = 3, srcColumn = 19}, srcEnd = SourcePos {srcLine = 3, srcColumn = 21}}, tokLeadingComments = [Space 1], tokTrailingComments = [Space 1]}, tokValue = TokLowerName [] "as"}, Name {nameTok = SourceToken {tokAnn = TokenAnn {tokRange = SourceRange {srcStart = SourcePos {srcLine = 3, srcColumn = 22}, srcEnd = SourcePos {srcLine = 3, srcColumn = 28}}, tokLeadingComments = [], tokTrailingComments = []}, tokValue = TokUpperName [] "Maybe2"}, nameValue = ModuleName "Maybe2"})
    }

-- not sure if its okay to use this dummy range
dummyRange :: SourceRange
dummyRange = SourceRange {srcStart = SourcePos 0 0, srcEnd = SourcePos 0 0}

-- addAFieldToAModule :: Module () -> Row () -> Module ()

main :: IO ()
main = checkHypothesis "/Users/anirbandas/work/nWork/namma-dsl/lib/namma-dsl/tests/src-read-only/UI/Frontend/Reels.purs"
