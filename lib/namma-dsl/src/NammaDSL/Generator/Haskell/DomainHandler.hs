module NammaDSL.Generator.Haskell.DomainHandler (generateDomainHandler) where

import Control.Lens ((^.))
import Control.Monad (forM_)
import Control.Monad.Extra (whenJust)
import Control.Monad.Reader (ask)
import Data.List (isInfixOf, nub)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import NammaDSL.Config (ApiKind (..), DefaultImports (..), GenerationType (DOMAIN_HANDLER))
import NammaDSL.DSL.Syntax.API
import NammaDSL.Generator.Haskell.Common
import NammaDSL.GeneratorCore
import NammaDSL.Lib hiding (Q, Writer)
import qualified NammaDSL.Lib.TH as TH
import qualified NammaDSL.Lib.Types as TH
import NammaDSL.Utils (removeUnusedQualifiedImports)
import Prelude

type Writer w = TH.Writer Apis w

type Q w = TH.Q Apis w

generateDomainHandler :: DefaultImports -> ApiRead -> Apis -> Code
generateDomainHandler (DefaultImports qualifiedImp simpleImp _packageImports _) apiRead input =
  generateCode generatorInput
  where
    generationType = DOMAIN_HANDLER
    codeBody' = generateCodeBody (mkCodeBody $ apiReadKind apiRead) input
    domainHandlerModulePrefix = apiDomainHandlerImportPrefix apiRead ++ "."
    packageOverride :: [String] -> [String]
    packageOverride = checkForPackageOverrides generationType (apiPackageMapping apiRead) (input ^. importPackageOverrides)

    generatorInput :: GeneratorInput
    generatorInput =
      GeneratorInput
        { _ghcOptions = ["-Wwarn=unused-imports"],
          _extensions = [],
          _moduleNm = domainHandlerModulePrefix <> T.unpack (_moduleName input),
          _moduleExports = Just $ T.unpack . handlerFunctionText <$> input ^. apis,
          _simpleImports = packageOverride simpleImp,
          _qualifiedImports = packageOverride $ removeUnusedQualifiedImports codeBody' allQualifiedImports,
          _packageImports,
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
            <> ["Kernel.Utils.Validation" | ifValidationRequired]

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

    ifValidationRequired :: Bool
    ifValidationRequired = apiReadKind apiRead == DASHBOARD && any (\apiT -> isJust $ apiT ^. requestValidation) (input ^. apis)

mkCodeBody :: ApiKind -> ApisM ()
mkCodeBody apiKind = do
  input <- ask
  tellM . fromMaybe mempty $
    interpreter input $ do
      forM_ (_apis input) $ generateHandlerFunction apiKind

generateHandlerFunction :: ApiKind -> ApiTT -> Writer CodeUnit
generateHandlerFunction apiKind apiT = do
  let functionName = handlerFunctionText apiT
      authToType = apiAuthTypeMapperDomainHandler apiT
      signatureUnits = case apiKind of
        DASHBOARD -> mkApiSignatureUnitsHelper apiT
        UI -> mkApiSignatureUnits apiT
      allTypes = map apiSignatureType signatureUnits
      apiUnits = map apiSignatureUnit signatureUnits
      showType = cT . T.unpack <$> filter (/= T.empty) (init allTypes)
      handlerTypes = authToType <> showType <> [cT "Environment.Flow" ~~ cT (T.unpack $ last allTypes)]
  delimiterComment $ T.unpack functionName
  decsW $ do
    TH.sigDW (mkNameT functionName) $ do
      TH.forallT [] [] $
        TH.appendInfixT "->" $ NE.fromList handlerTypes
    TH.funDW (mkNameT functionName) $ do
      let pats = case apiKind of
            UI -> []
            DASHBOARD -> vP "_merchantShortId" : vP "_opCity" : generateParamsPat apiUnits
      TH.clauseW pats $
        TH.normalB $
          TH.doEW $ do
            case apiKind of
              UI -> noBindSW $ vE "error" ~* strE "Logic yet to be decided"
              DASHBOARD -> do
                whenJust (apiT ^. requestValidation) $ \validationFunc -> do
                  let reqParam = case findRequest apiUnits of
                        Just paramText -> vE paramText
                        Nothing -> error $ "Did not found request for validation: " <> T.unpack functionName
                  TH.noBindSW $ vE "Kernel.Utils.Validation.runRequestValidation" ~* vE (T.unpack validationFunc) ~* reqParam
                noBindSW $ appendE $ vE "error" NE.:| strE "Logic yet to be decided" : generateParamsExp apiUnits -- just for avoid unused vars error

_ShortId :: Q TH.Type
_ShortId = cT "Kernel.Types.Id.ShortId"

_Merchant :: Q TH.Type
_Merchant = cT "Domain.Types.Merchant.Merchant"
