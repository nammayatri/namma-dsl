module NammaDSL.Generator.Haskell.ApiTree.Client (generateAPITreeClient) where

import Control.Monad (when)
import Control.Monad.Reader (ask)
import Data.Foldable
import Data.Functor ((<&>))
import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
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

generateAPITreeClient :: DefaultImports -> ApiRead -> ApiTree -> Code
generateAPITreeClient (DefaultImports qualifiedImp simpleImp _packageImports _) apiRead input = generateCode generatorInput
  where
    apiTypesModulePrefix = apiTypesImportPrefix apiRead ++ "."

    generatorInput :: GeneratorInput
    generatorInput =
      GeneratorInput
        { _ghcOptions = ["-Wno-unused-imports"],
          _extensions = ["AllowAmbiguousTypes" | apiReadKind apiRead == DASHBOARD],
          _moduleNm = apiClientImportPrefix apiRead,
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
        generateClientsType apiRead
        generateMkClientsType apiRead
        generateCallClientFunc apiRead

generateClientsType :: ApiRead -> Writer CodeUnit
generateClientsType apiRead = do
  input <- ask
  let folderName' = fromMaybe (error "folderName required for dashboard api generation") $ apiFolderName apiRead
  let name = TH.mkName folderName' <> "APIs"
  case specModules input of
    [] -> pure ()
    specModules' -> do
      TH.dataOrNewtypeDW
        name
        []
        do
          recCW name $ do
            forM_ specModules' $ \specModule -> do
              let fieldName = TH.mkNameT $ headToLower (T.pack specModule) <> "DSL"
              let fieldType = cT $ apiTypesImportPrefix apiRead #. specModule #. specModule <> "APIs"
              TH.fieldDecW fieldName fieldType
        emptyDerive

generateMkClientsType :: ApiRead -> Writer CodeUnit
generateMkClientsType apiRead = do
  input <- ask
  let folderName' = fromMaybe (error "folderName required for dashboard api generation") $ apiFolderName apiRead
  let folderFuncName = TH.mkName $ "mk" <> folderName' <> "APIs"
  let typeName = folderName' <> "APIs"
  let specModules' = specModules input
  case NE.nonEmpty specModules' of
    Nothing -> pure ()
    Just specModulesNE -> do
      TH.decsW $ do
        TH.sigDW folderFuncName $
          cT "Tools.Auth.Merchant.CheckedShortId" ~~ cT "Domain.Types.Merchant.Merchant" --> cT "Kernel.Types.Beckn.City.City" --> cT "Text" --> cT typeName
        TH.funDW folderFuncName $
          TH.clauseWhereW
            [vP "merchantId", vP "city", vP "token"]
            do
              TH.normalB $
                TH.doEW $ do
                  forM_ specModules' $ \specModule -> do
                    let fieldName = TH.mkNameT $ headToLower (T.pack specModule) <> "DSL"
                    let moduleFuncName = apiTypesImportPrefix apiRead #. specModule #. "mk" <> specModule <> "APIs"
                    let clientName = T.unpack $ headToLower (T.pack specModule) <> "ClientDSL"
                    TH.letStmt fieldName $ vE moduleFuncName ~* vE clientName
                  TH.noBindSW $ wildRecordsE typeName
            do
              let clientNames = specModulesNE <&> (\specModule -> vP $ T.unpack $ headToLower (T.pack specModule) <> "ClientDSL")
              valDW (appendInfixP ":<|>" clientNames) $
                normalB $ do
                  let folderApiType = cT $ "API.Dashboard" #. folderName' <> "DSLAPI"
                  vE "Tools.Client.clientWithMerchantAndCity" ~* (sigE (cE "Proxy") $ cT "Proxy" ~~ folderApiType) ~* vE "merchantId" ~* vE "city" ~* vE "token"

generateCallClientFunc :: ApiRead -> Writer CodeUnit
generateCallClientFunc apiRead = do
  let folderName' = fromMaybe (error "folderName should be provided for dashboard api") $ apiFolderName apiRead
  let funcName = "call" <> folderName' <> "API"
  let apisTypeName = folderName' <> "APIs"
  let folderFuncName = "mk" <> folderName' <> "APIs"
  let serverName = fromMaybe (error "serverName should be provided for dashboard api") $ apiServerName apiRead
  TH.decsW $ do
    TH.sigDW (TH.mkName funcName) $ do
      let forallClause = [TH.PlainTV "m" TH.SpecifiedSpec, TH.PlainTV "r" TH.SpecifiedSpec, TH.PlainTV "b" TH.SpecifiedSpec, TH.PlainTV "c" TH.SpecifiedSpec]
      let constraint = [cT "Tools.Client.DashboardClient" ~~ cT apisTypeName ~~ vT "m" ~~ vT "r" ~~ vT "b" ~~ vT "c"]
      TH.forallT forallClause constraint $
        (cT "Tools.Auth.Merchant.CheckedShortId" ~~ cT "Domain.Types.Merchant.Merchant")
          --> cT "Kernel.Types.Beckn.City.City"
          --> parensT (cT apisTypeName --> vT "b")
          --> vT "c"
    TH.funDW (TH.mkName funcName) $
      clauseW [vP "merchantId", vP "city"] $
        TH.normalB $
          vE "Tools.Client.callServerAPI"
            `appTypeE` cT "_"
            `appTypeE` cT "m"
            `appTypeE` cT "r"
            ~* cE ("Domain.Types.ServerName" #. serverName)
            ~* parenE (vE folderFuncName ~* vE "merchantId" ~* vE "city")
            ~* strE funcName
