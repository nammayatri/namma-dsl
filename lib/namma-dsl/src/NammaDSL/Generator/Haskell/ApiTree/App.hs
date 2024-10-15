module NammaDSL.Generator.Haskell.ApiTree.App (generateAPITree) where

import Control.Monad (when)
import Control.Monad.Reader (ask)
import Data.Functor ((<&>))
import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import NammaDSL.Config (ApiKind (..), DefaultImports (..))
import NammaDSL.DSL.Syntax.API
import NammaDSL.GeneratorCore
import NammaDSL.Lib hiding (Q, Writer)
import qualified NammaDSL.Lib.TH as TH
import qualified NammaDSL.Lib.Types as TH
import NammaDSL.Utils
import Prelude

type Writer w = TH.Writer ApiTree w

-- type Q w = TH.Q ApiTree w

generateAPITree :: DefaultImports -> ApiRead -> ApiTree -> Code
generateAPITree (DefaultImports qualifiedImp simpleImp _packageImports _) apiRead input = generateCode generatorInput
  where
    apiServantModulePrefix = apiServantImportPrefix apiRead ++ "."

    generatorInput :: GeneratorInput
    generatorInput =
      GeneratorInput
        { _ghcOptions = ["-Wno-unused-imports"],
          _extensions = [],
          _moduleNm = apiServantImportPrefix apiRead,
          _moduleExports = Just ["API", "handler"],
          _simpleImports = simpleImp,
          _qualifiedImports = removeUnusedQualifiedImports codeBody' allQualifiedImports,
          _packageImports,
          _codeBody = codeBody'
        }
    codeBody' = generateCodeBody (mkCodeBody apiRead) input

    allQualifiedImports :: [String]
    allQualifiedImports =
      nub $
        ((apiServantModulePrefix <>) <$> specModules input)
          <> qualifiedImp

mkCodeBody :: ApiRead -> ApiTreeM ()
mkCodeBody apiRead = do
  input <- ask
  tellM . fromMaybe mempty $
    interpreter input $ do
      when (apiReadKind apiRead == DASHBOARD) $ do
        generateAPIType apiRead
        generateAPIHandler apiRead

generateAPIType :: ApiRead -> Writer CodeUnit
generateAPIType apiRead = do
  input <- ask
  let apiServantModulePrefix = apiServantImportPrefix apiRead <> "."
  tySynDW "API" [] $ do
    let specApis =
          specModules input <&> \specModule -> do
            cT $ apiServantModulePrefix <> specModule #. "API"
    appendInfixT ":<|>" $ NE.fromList specApis

generateAPIHandler :: ApiRead -> Writer CodeUnit
generateAPIHandler apiRead = do
  input <- ask
  let apiServantModulePrefix = apiServantImportPrefix apiRead <> "."
  let specHandlers =
        specModules input <&> \specModule -> do
          vE (apiServantModulePrefix <> specModule #. "handler") ~* vE "merchantId" ~* vE "city"
  TH.decsW $ do
    TH.sigDW "handler" $
      (cT "Kernel.Types.Id.ShortId" ~~ cT "Domain.Types.Merchant.Merchant") --> cT "Kernel.Types.Beckn.Context.City" --> (cT "Environment.FlowServer" ~~ cT "API")
    TH.funDW "handler" $
      TH.clauseW [vP "merchantId", vP "city"] $
        TH.normalB $
          appendInfixE (vE ":<|>") $ NE.fromList specHandlers
