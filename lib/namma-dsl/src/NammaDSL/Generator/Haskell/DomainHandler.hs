module NammaDSL.Generator.Haskell.DomainHandler (generateDomainHandler) where

-- import NammaDSL.DSL.Parser.API hiding (figureOutImports)

-- import NammaDSL.Utils

import Data.List (isInfixOf, nub)
import qualified Data.Text as T
import Kernel.Prelude hiding (replicateM)
import NammaDSL.DSL.Syntax.API
import NammaDSL.Generator.Haskell.Common (apiAuthTypeMapper)
import NammaDSL.Generator.Haskell.Servant (handlerFunctionText, handlerSignature)
import NammaDSL.GeneratorCore

generateDomainHandler :: Apis -> Code
generateDomainHandler input =
  generateCode generatorInput
  where
    generatorInput :: GeneratorInput
    generatorInput =
      GeneratorInput
        { _ghcOptions = ["-Wno-orphans", "-Wno-unused-imports"],
          _extensions = [],
          _moduleNm = "Domain.Action.UI." <> T.unpack (_moduleName input),
          _simpleImports = allSimpleImports,
          _qualifiedImports = allQualifiedImports,
          _codeBody = generateCodeBody mkCodeBody input
        }
    qualifiedModuleName = T.unpack ("Domain.Action.UI." <> _moduleName input)

    allSimpleImports :: [String]
    allSimpleImports =
      [ "EulerHS.Prelude hiding (id)",
        "Servant",
        "Tools.Auth",
        "Data.OpenApi (ToSchema)",
        "API.Types.UI." <> T.unpack (_moduleName input)
      ]

    allQualifiedImports :: [String]
    allQualifiedImports =
      nub $
        preventSameModuleImports $
          (T.unpack <$> _imports input)
            <> defaultQualifiedImport

    preventSameModuleImports :: [String] -> [String]
    preventSameModuleImports = filter (\x -> not (qualifiedModuleName `isInfixOf` x))

    defaultQualifiedImport :: [String]
    defaultQualifiedImport = ["Kernel.Prelude", "Domain.Types.Person", "Domain.Types.Merchant", "Environment", "Kernel.Types.Id"]

mkCodeBody :: ApisM ()
mkCodeBody = do
  input <- ask
  let seperator = newLine *> newLine
  onNewLine $
    intercalateA seperator (map handlerFunctionDef (_apis input))
  where
    handlerFunctionDef :: ApiTT -> ApisM ()
    handlerFunctionDef apiT =
      let functionName = handlerFunctionText apiT
          autoToType = apiAuthTypeMapper apiT
          allTypes = handlerSignature apiT
          showType = case filter (/= T.empty) (init allTypes) of
            [] -> T.empty
            ty -> " -> " <> T.intercalate " -> " ty
          handlerTypes = showType <> (if length allTypes > 1 then " -> " else " ") <> "Environment.Flow " <> last allTypes
       in tellM $
            T.unpack $
              functionName
                <> (maybe " ::" (" :: " <>) autoToType)
                <> handlerTypes
                <> "\n"
                <> functionName
                <> " = error \"Logic yet to be decided\""
