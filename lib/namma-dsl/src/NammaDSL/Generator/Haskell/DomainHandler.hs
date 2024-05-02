module NammaDSL.Generator.Haskell.DomainHandler (generateDomainHandler) where

import Control.Lens ((^.))
import Control.Monad.Reader (ask)
import Data.List (isInfixOf, nub)
import qualified Data.Text as T
import NammaDSL.Config (DefaultImports (..))
import NammaDSL.DSL.Syntax.API
import NammaDSL.Generator.Haskell.Common( apiAuthTypeMapperDomainHandler, checkForPackageOverrides )
import NammaDSL.Generator.Haskell.Servant (handlerFunctionText, handlerSignature)
import NammaDSL.GeneratorCore
import NammaDSL.Utils (removeUnusedQualifiedImports)
import NammaDSL.Lib hiding (Q, Writer)
import qualified NammaDSL.Lib.Types as TH
import Prelude
import Control.Monad (forM_)
import qualified NammaDSL.Lib.TH as TH
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)

type Writer w = TH.Writer Apis w

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
            <> qualifiedImp
            <> ["Domain.Types.MerchantOperatingCity" | ifProviderPlatform]

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
  tellM . fromMaybe mempty $ interpreter input $ do
    generateHandlerFunctions

generateHandlerFunctions :: Writer CodeUnit
generateHandlerFunctions = do
  input <- ask
  forM_ (_apis input) $ \apiT -> decsW $ do
    let functionName = handlerFunctionText apiT
        autoToType = maybe [] pure (apiAuthTypeMapperDomainHandler apiT)
        allTypes = handlerSignature apiT
        showType = cT . T.unpack <$> filter (/= T.empty) (init allTypes)
        handlerTypes = autoToType <> showType <> [cT "Environment.Flow" ~~ cT (T.unpack $ last allTypes)]
    TH.sigDW (mkNameT functionName) $ do
      TH.forallT [] [] $
        TH.appendInfixT "->" $ NE.fromList handlerTypes
    TH.funDW (mkNameT functionName) $ do
      TH.clauseW [] $
        TH.normalB $
          vE "error" ~ strE "Logic yet to be decided"
