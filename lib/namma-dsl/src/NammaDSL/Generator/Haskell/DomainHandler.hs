module NammaDSL.Generator.Haskell.DomainHandler (generateDomainHandler) where

-- import NammaDSL.DSL.Parser.API hiding (figureOutImports)

-- import NammaDSL.Utils
import Control.Lens ((^.))
import Control.Monad.Reader (ask)
import Data.List (isInfixOf, nub)
import qualified Data.Text as T
import NammaDSL.Config (DefaultImports (..))
import NammaDSL.DSL.Syntax.API
import NammaDSL.Generator.Haskell.Common (apiAuthTypeMapperDomainHandler, checkForPackageOverrides)
import NammaDSL.Generator.Haskell.Servant (handlerFunctionText, handlerSignature)
import NammaDSL.GeneratorCore
import Prelude

generateDomainHandler :: DefaultImports -> ApiRead -> Apis -> Code
generateDomainHandler (DefaultImports qualifiedImp simpleImp _) apiRead input =
  generateCode generatorInput
  where
    domainHandlerModulePrefix = apiDomainHandlerImportPrefix apiRead ++ "."
    packageOverride :: [String] -> [String]
    packageOverride = checkForPackageOverrides (input ^. importPackageOverrides)

    generatorInput :: GeneratorInput
    generatorInput =
      GeneratorInput
        { _ghcOptions = ["-Wno-orphans", "-Wno-unused-imports"],
          _extensions = [],
          _moduleNm = domainHandlerModulePrefix <> T.unpack (_moduleName input),
          _simpleImports = packageOverride allSimpleImports,
          _qualifiedImports = packageOverride allQualifiedImports,
          _codeBody = generateCodeBody mkCodeBody input
        }
    qualifiedModuleName = T.unpack ((T.pack domainHandlerModulePrefix) <> _moduleName input)

    allSimpleImports :: [String]
    allSimpleImports =
      [ "EulerHS.Prelude hiding (id)",
        "Servant",
        "Tools.Auth",
        "Data.OpenApi (ToSchema)"
      ]
        <> simpleImp

    allQualifiedImports :: [String]
    allQualifiedImports =
      nub $
        preventSameModuleImports $
          (T.unpack <$> _imports input)
            <> defaultQualifiedImport
            <> ["Domain.Types.Merchant.MerchantOperatingCity" | ifProviderPlatform]
            <> qualifiedImp

    ifProviderPlatform :: Bool
    ifProviderPlatform =
      any
        ( \authType' -> do
            case authType' of
              Just (TokenAuth PROVIDER_TYPE) -> True
              _ -> False
        )
        (map _authType $ _apis input)

    preventSameModuleImports :: [String] -> [String]
    preventSameModuleImports = filter (\x -> not (qualifiedModuleName `isInfixOf` x))

    defaultQualifiedImport :: [String]
    defaultQualifiedImport = ["Kernel.Prelude", "Domain.Types.Person", "Domain.Types.Merchant", "Environment", "Kernel.Types.Id"] -- kept for backward compatibility for not, will remove after configuring the dhall configs

mkCodeBody :: ApisM ()
mkCodeBody = do
  input <- ask
  let separator = newLine *> newLine
  onNewLine $
    intercalateA separator (map handlerFunctionDef (_apis input))
  where
    handlerFunctionDef :: ApiTT -> ApisM ()
    handlerFunctionDef apiT =
      let functionName = handlerFunctionText apiT
          autoToType = maybe [] pure (apiAuthTypeMapperDomainHandler apiT)
          allTypes = handlerSignature apiT
          showType = filter (/= T.empty) (init allTypes)
          -- [] -> T.empty
          -- ty -> " -> " <> T.intercalate " -> " ty
          handlerTypes = autoToType <> showType <> ["Environment.Flow " <> last allTypes]
       in tellM $
            T.unpack $
              functionName
                <> " :: "
                <> T.intercalate " -> " handlerTypes
                <> "\n"
                <> functionName
                <> " = error \"Logic yet to be decided\""
