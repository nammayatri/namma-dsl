module NammaDSL.Generator.Haskell.ApiTree.Common (generateAPITreeCommon) where

import Control.Monad (when)
import Control.Monad.Reader (ask)
import Data.Foldable
import Data.Functor ((<&>))
import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import NammaDSL.Config (ApiKind (..), DefaultImports (..))
import NammaDSL.DSL.Syntax.API
import qualified NammaDSL.Generator.Haskell.Common as Common
import NammaDSL.GeneratorCore
import NammaDSL.Lib hiding (Q, Writer)
import qualified NammaDSL.Lib.TH as TH
import qualified NammaDSL.Lib.Types as TH
import NammaDSL.Utils
import Prelude

type Writer w = TH.Writer ApiTree w

-- type Q w = TH.Q ApiTree w

generateAPITreeCommon :: DefaultImports -> ApiRead -> ApiTree -> Code
generateAPITreeCommon (DefaultImports qualifiedImp simpleImp _packageImports _) apiRead input = generateCode generatorInput
  where
    apiTypesModulePrefix = apiTypesImportPrefix apiRead ++ "."

    generatorInput :: GeneratorInput
    generatorInput =
      GeneratorInput
        { _ghcOptions = ["-Wno-unused-imports"],
          _extensions = ["StandaloneKindSignatures" | apiReadKind apiRead == DASHBOARD],
          _moduleNm = apiTypesImportPrefix apiRead,
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
        ((apiTypesModulePrefix <>) <$> specModules input)
          <> qualifiedImp

mkCodeBody :: ApiRead -> ApiTreeM ()
mkCodeBody apiRead = do
  input <- ask
  tellM . fromMaybe mempty $
    interpreter input $ do
      when (apiReadKind apiRead == DASHBOARD) $ do
        generateUserActionType apiRead
        generateUserActionTypeShowInstance apiRead
        generateUserActionTypeReadInstance apiRead

generateUserActionType :: ApiRead -> Writer CodeUnit
generateUserActionType apiRead = do
  input <- ask
  let folderName' = fromMaybe (error "folderName required for dashboard api generation") $ apiFolderName apiRead
  dataOrNewtypeDW
    (TH.mkName $ folderName' <> "UserActionType")
    []
    do
      forM_ (specModules input) $ \specModule -> do
        normalCW (TH.mkName $ Common.screamingSnake specModule) [cT $ apiTypesImportPrefix apiRead #. specModule #. specModule <> "UserActionType"]
    do
      derivClauseW (Just TH.StockStrategy) $ ConT <$> ["Generic", "Eq", "Ord"]
      derivClauseW (Just TH.AnyclassStrategy) $ ConT <$> ["ToJSON", "FromJSON", "ToSchema"]

  spliceW (vE "Data.Singletons.TH.genSingletons" ~* listE [vE $ "''" <> folderName' <> "UserActionType"])

generateUserActionTypeShowInstance :: ApiRead -> Writer CodeUnit
generateUserActionTypeShowInstance apiRead = do
  input <- ask
  let folderName' = fromMaybe (error "folderName required for dashboard api generation") $ apiFolderName apiRead
  instanceDW (pure []) (cT "Text.Show.Show" ~~ cT (folderName' <> "UserActionType")) $ do
    TH.funDW "show" $ do
      TH.clauseW [] $
        TH.normalB $
          TH.lambdaCaseE $
            specModules input <&> \specModule -> do
              let moduleUserActionType = Common.screamingSnake specModule
              TH.matchWOD (cP moduleUserActionType [vP "e"]) $ do
                TH.normalB (TH.strE (moduleUserActionType <> "/") ~<> vE "show" ~* vE "e")

generateUserActionTypeReadInstance :: ApiRead -> Writer CodeUnit
generateUserActionTypeReadInstance apiRead = do
  input <- ask
  let folderName' = fromMaybe (error "folderName required for dashboard api generation") $ apiFolderName apiRead
  instanceDW (pure []) (cT "Text.Read.Read" ~~ cT (folderName' <> "UserActionType")) $ do
    TH.funDW "readsPrec" $ do
      TH.clauseWhereW
        [vP "d'"]
        do
          TH.normalB $
            vE "Text.Read.readParen"
              ~* (vE "d'" ~> vE "app_prec")
              ~* do
                lamEE [vP "r"] $
                  appendInfixE (vE "++") $
                    NE.fromList $
                      specModules input <&> \specModule -> do
                        compE $ do
                          let moduleUserActionType = Common.screamingSnake specModule
                          vP "r1" <-- vE "stripPrefix" ~* TH.strE (moduleUserActionType <> "/") ~* vE "r"
                          tupP [vP "v1", vP "r2"] <-- vE "Text.Read.readsPrec" ~* (vE "app_prec" ~+ intE 1) ~* vE "r1"
                          noBindSW (tupE [Just $ cE moduleUserActionType ~* vE "v1", Just $ vE "r2"])
        do
          -- TODO move to Common module instead of generate
          TH.valDW (vP "app_prec") $ TH.normalB (intE 10)
          TH.funDW (TH.mkName "stripPrefix") $
            TH.clauseW [vP "pref", vP "r"] $
              TH.normalB $
                vE "bool" ~* listE [] ~* listE [vE "Data.List.drop" ~* (vE "length" ~* vE "pref") ~* vE "r"] ~$ vE "Data.List.isPrefixOf" ~* vE "pref" ~* vE "r"
