module NammaDSL.Generator.Haskell.ApiTree.Dashboard (generateAPITreeDashboard) where

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

generateAPITreeDashboard :: DefaultImports -> ApiRead -> ApiTree -> Code
generateAPITreeDashboard (DefaultImports qualifiedImp simpleImp _packageImports _) apiRead input = generateCode generatorInput
  where
    apiServantDashboardModulePrefix = apiServantDashboardImportPrefix apiRead ++ "."

    generatorInput :: GeneratorInput
    generatorInput =
      GeneratorInput
        { _ghcOptions = ["-Wno-unused-imports"],
          _extensions = [],
          _moduleNm = apiServantDashboardImportPrefix apiRead,
          _moduleExports = Nothing,
          _simpleImports = simpleImp,
          _qualifiedImports = removeUnusedQualifiedImports codeBody' allQualifiedImports,
          _packageImports,
          _codeBody = codeBody'
        }
    codeBody' = generateCodeBody (mkCodeBody apiRead) input

    allQualifiedImports :: [String]
    allQualifiedImports =
      nub $
        ((apiServantDashboardModulePrefix <>) <$> specModules input)
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
  let apiServantDashboardModulePrefix = apiServantDashboardImportPrefix apiRead ++ "."
  tySynDW "API" [] $ do
    let specApis =
          specModules input <&> \specModule -> do
            cT $ apiServantDashboardModulePrefix <> specModule #. "API"
    TH.parensT . appendInfixT ":<|>" $ NE.fromList specApis

generateAPIHandler :: ApiRead -> Writer CodeUnit
generateAPIHandler apiRead = do
  input <- ask
  let apiServantDashboardModulePrefix = apiServantDashboardImportPrefix apiRead ++ "."
  let specHandlers =
        specModules input <&> \specModule -> do
          vE (apiServantDashboardModulePrefix <> specModule #. "handler") ~* vE "merchantId" ~* vE "city"
  TH.decsW $ do
    TH.sigDW "handler" $
      (cT "Kernel.Types.Id.ShortId" ~~ cT "Domain.Types.Merchant.Merchant") --> cT "Kernel.Types.Beckn.Context.City" --> (cT "Environment.FlowServer" ~~ cT "API")
    TH.funDW "handler" $
      TH.clauseW [vP "merchantId", vP "city"] $
        TH.normalB $
          appendInfixE (vE ":<|>") $ NE.fromList specHandlers
