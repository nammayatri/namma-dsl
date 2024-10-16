module NammaDSL.Generator.Haskell.Servant (handlerFunctionText, generateServantAPI, handlerSignature, apiTTToText) where

import Control.Lens ((^.))
import Control.Monad (forM_, when)
import Control.Monad.Reader (ask)
import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import NammaDSL.Config (ApiKind (..), DefaultImports (..), GenerationType (SERVANT_API))
import NammaDSL.DSL.Syntax.API
import NammaDSL.Generator.Haskell.Common hiding (generateParamsExp, generateParamsPat)
import NammaDSL.GeneratorCore
import NammaDSL.Lib hiding (Q, Writer)
import qualified NammaDSL.Lib.TH as TH
import qualified NammaDSL.Lib.Types as TH
import NammaDSL.Utils
import Prelude

type Writer w = TH.Writer Apis w

type Q w = TH.Q Apis w

generateServantAPI :: DefaultImports -> ApiRead -> Apis -> Code
generateServantAPI (DefaultImports qualifiedImp simpleImp _packageImports _) apiRead input =
  generateCode generatorInput
  where
    codeBody' = generateCodeBody (mkCodeBody apiRead) input
    servantApiModulePrefix = apiServantImportPrefix apiRead ++ "."
    domainHandlerModulePrefix = apiDomainHandlerImportPrefix apiRead ++ "."
    packageOverride :: [String] -> [String]
    packageOverride = checkForPackageOverrides (input ^. importPackageOverrides)

    generatorInput :: GeneratorInput
    generatorInput =
      GeneratorInput
        { _ghcOptions = ["-Wno-orphans", "-Wno-unused-imports"],
          _extensions = [],
          _moduleNm = servantApiModulePrefix <> T.unpack (_moduleName input),
          _moduleExports = allModuleExports,
          _simpleImports = packageOverride allSimpleImports,
          _qualifiedImports = packageOverride $ removeUnusedQualifiedImports codeBody' allQualifiedImports,
          _packageImports,
          _codeBody = codeBody'
        }

    allQualifiedImports :: [String]
    allQualifiedImports =
      [ domainHandlerModulePrefix
          <> T.unpack (_moduleName input)
          <> " as "
          <> domainHandlerModulePrefix
          <> T.unpack (_moduleName input)
      ]
        <> nub (qualifiedImp <> figureOutImports allHandlersSignatures <> apiTypesImport)
        <> ["Domain.Types.MerchantOperatingCity" | ifProviderPlatform]

    allHandlersSignatures :: [String]
    allHandlersSignatures = case apiReadKind apiRead of
      UI -> T.unpack <$> concatMap handlerSignature (_apis input)
      DASHBOARD -> T.unpack <$> concatMap handlerSignatureHelper (_apis input)

    allSimpleImports :: [String]
    allSimpleImports =
      ["Storage.Beam.SystemConfigs ()" | ifNotDashboard]
        <> ["Tools.Auth.Webhook" | ifSafetyDashboard]
        <> simpleImp

    apiTypesImport :: [String]
    apiTypesImport = [apiTypesImportPrefix apiRead #. T.unpack (_moduleName input) | apiReadKind apiRead == DASHBOARD] -- we need API type for reexport
    ifNotDashboard :: Bool
    ifNotDashboard =
      any
        ( \authType' -> do
            case authType' of
              Just (DashboardAuth _) -> False
              Just (SafetyWebhookAuth _) -> False
              Just (ApiAuth {}) -> False
              Just (ApiAuthV2 {}) -> False
              Just NoAuth | apiReadKind apiRead == DASHBOARD -> False
              _ -> True
        )
        (map _authType $ _apis input)

    ifSafetyDashboard :: Bool
    ifSafetyDashboard =
      any
        ( \authType' -> do
            case authType' of
              Just (SafetyWebhookAuth _) -> True
              _ -> False
        )
        (map _authType $ _apis input)

    ifProviderPlatform :: Bool
    ifProviderPlatform =
      any
        ( \authType' -> do
            case authType' of
              Just (TokenAuth PROVIDER_TYPE) -> True
              _ -> False
        )
        (map _authType $ _apis input)

    allModuleExports = do
      let moduleName' = _moduleName input
      let apiTypeName = case apiReadKind apiRead of
            UI -> "API"
            DASHBOARD -> apiTypesImportPrefix apiRead #. T.unpack moduleName' #. "API"
      Just [apiTypeName, "handler"]

mkCodeBody :: ApiRead -> ApisM ()
mkCodeBody apiRead = do
  input <- ask
  tellM . fromMaybe mempty $
    interpreter input $ do
      when (apiReadKind apiRead == UI) $
        generateAPIType SERVANT_API apiRead
      generateAPIHandler apiRead

generateAPIHandler :: ApiRead -> Writer CodeUnit
generateAPIHandler apiRead = do
  input <- ask
  let allApis = _apis input
      moduleName' = _moduleName input

  TH.decsW $ do
    sigDW "handler" $ mkSign moduleName'
    funDW "handler" $
      TH.clauseW mkPat $
        TH.normalB $
          appendInfixE (vE ":<|>") (NE.fromList $ mkExp <$> allApis)
  forM_ allApis $ handlerFunctionDef moduleName'
  where
    domainHandlerModulePrefix = apiDomainHandlerImportPrefix apiRead ++ "."

    mkSign moduleName' = do
      let apiTypeName = case apiReadKind apiRead of
            UI -> "API"
            DASHBOARD -> apiTypesImportPrefix apiRead #. T.unpack moduleName' #. "API"
      let defSignature = cT "Environment.FlowServer" ~~ cT apiTypeName
      case apiReadKind apiRead of
        UI -> defSignature
        DASHBOARD -> _ShortId ~~ _Merchant --> cT "Kernel.Types.Beckn.Context.City" --> defSignature
    mkPat = case apiReadKind apiRead of
      UI -> []
      DASHBOARD -> [vP "merchantId", vP "city"]
    mkExp api = do
      let defExp = vE (T.unpack $ handlerFunctionText api)
      case apiReadKind apiRead of
        UI -> defExp
        DASHBOARD -> defExp ~* vE "merchantId" ~* vE "city"

    isAuthPresent :: ApiTT -> Bool
    isAuthPresent apiT = case _authType apiT of
      Just NoAuth -> False
      _ -> True

    isDashboardAuth :: ApiTT -> Bool
    isDashboardAuth apiT = case _authType apiT of
      Just (DashboardAuth _) -> True
      Just (SafetyWebhookAuth _) -> True
      _ -> False

    isApiTokenAuth :: ApiTT -> Bool
    isApiTokenAuth apiT = case _authType apiT of
      Just ApiTokenAuth -> True
      _ -> False

    generateParamsPat :: Int -> [Q TH.Pat]
    generateParamsPat 0 = []
    generateParamsPat n = vP ("a" <> show n) : generateParamsPat (n - 1)

    generateParamsExp :: Bool -> Int -> [Q TH.Exp]
    generateParamsExp _ 0 = []
    generateParamsExp useAuthWithTuple n =
      ( if useAuthWithTuple
          then vE "Control.Lens.over" ~* vE "Control.Lens._1" ~* cE "Kernel.Prelude.Just" ~* vE ("a" <> show n)
          else vE ("a" <> show n)
      ) :
      generateParamsExp False (n - 1)

    handlerFunctionDef :: Text -> ApiTT -> Writer CodeUnit
    handlerFunctionDef moduleName' apiT = do
      let functionName = handlerFunctionText apiT
          allTypes = case apiReadKind apiRead of
            UI -> handlerSignature apiT
            DASHBOARD -> handlerSignatureHelper apiT
          showType = cT . T.unpack <$> filter (/= T.empty) (init allTypes)
          handlerTypes = apiAuthTypeMapperServant SERVANT_API apiT <> showType <> [cT "Environment.FlowHandler" ~~ cT (T.unpack $ last allTypes)]
      TH.decsW $ do
        TH.sigDW (TH.mkNameT functionName) $ do
          TH.forallT [] [] $
            TH.appendArrow $ NE.fromList handlerTypes
        TH.funDW (TH.mkNameT functionName) $ do
          let paramsNumber = case apiReadKind apiRead of
                DASHBOARD -> length allTypes + 1
                UI | isAuthPresent apiT -> length allTypes
                UI -> length allTypes - 1
          let pats = generateParamsPat paramsNumber
          TH.clauseW pats $
            TH.normalB $
              generateWithFlowHandlerAPI (isDashboardAuth apiT) $
                TH.appendE $
                  vE (domainHandlerModulePrefix <> T.unpack moduleName' #. T.unpack functionName)
                    NE.:| generateParamsExp (isAuthPresent apiT && (not $ isApiTokenAuth apiT) && not (isDashboardAuth apiT) && (apiReadKind apiRead /= DASHBOARD)) paramsNumber
