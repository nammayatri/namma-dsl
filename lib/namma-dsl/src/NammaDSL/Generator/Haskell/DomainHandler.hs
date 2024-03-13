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
import NammaDSL.Utils (removeUnusedQualifiedImports)
import Prelude

generateDomainHandler :: DefaultImports -> ApiRead -> Apis -> Code
generateDomainHandler (DefaultImports qualifiedImp simpleImp _) apiRead input =
  generateCode generatorInput
  where
    codeBody' = generateCodeBody mkCodeBody input
    domainHandlerModulePrefix = apiDomainHandlerImportPrefix apiRead ++ "."
    packageOverride :: [String] -> [String]
    packageOverride = checkForPackageOverrides (input ^. importPackageOverrides)

    generatorInput :: GeneratorInput
    generatorInput =
      GeneratorInput
        { _ghcOptions = ["-Wno-orphans", "-Wno-unused-imports"],
          _extensions = [],
          _moduleNm = domainHandlerModulePrefix <> T.unpack (_moduleName input),
          _simpleImports = packageOverride simpleImp,
          _qualifiedImports = packageOverride $ removeUnusedQualifiedImports codeBody' allQualifiedImports,
          _codeBody = codeBody'
        }
    qualifiedModuleName = T.unpack ((T.pack domainHandlerModulePrefix) <> _moduleName input)

    allQualifiedImports :: [String]
    allQualifiedImports =
      nub $
        preventSameModuleImports $
          (T.unpack <$> (_imports input))
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
