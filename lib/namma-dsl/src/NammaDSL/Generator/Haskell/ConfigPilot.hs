{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module NammaDSL.Generator.Haskell.ConfigPilot (generateConfigPilot) where

import Control.Monad.Reader (ask)
import Data.List (find, intercalate, isPrefixOf)
import Data.Maybe (fromMaybe, isJust)
import NammaDSL.Config (DefaultImports (..), GenerationType (CONFIG_PILOT))
import NammaDSL.DSL.Syntax.Storage
import NammaDSL.Generator.Haskell.Common (checkForPackageOverrides)
import NammaDSL.GeneratorCore
import NammaDSL.Utils (capitalize, removeUnusedQualifiedImports)
import Prelude

generateConfigPilot :: DefaultImports -> StorageRead -> TableDef -> Maybe Code
generateConfigPilot (DefaultImports qualifiedImp simpleImp _packageImports _) storageRead tableDef =
  case configPilot tableDef of
    Nothing -> Nothing
    Just cpDef ->
      let codeBody' = generateCodeBody (mkCodeBody storageRead cpDef) tableDef
       in if codeBody' == mempty
            then Nothing
            else
              Just $
                generateCode
                  GeneratorInput
                    { _ghcOptions = ["-Wno-orphans", "-Wno-deprecations", "-Wno-unused-imports"],
                      _extensions = [],
                      _moduleNm = moduleName,
                      _moduleExports = Just [dimsName ++ " (..)"],
                      _simpleImports = packageOverride allSimpleImports,
                      _qualifiedImports = packageOverride $ removeUnusedQualifiedImports codeBody' allQualifiedImports,
                      _packageImports = _packageImports,
                      _codeBody = codeBody'
                    }
  where
    generationType = CONFIG_PILOT
    cpPrefix = storageRead.configPilotModulePrefix ++ "."
    moduleName = cpPrefix ++ tableNameHaskell tableDef
    tblName = tableNameHaskell tableDef
    dimsName = tblName ++ "Dimensions"
    domainTypeModulePrefix = storageRead.domainTypeModulePrefix ++ "."

    packageOverride :: [String] -> [String]
    packageOverride = checkForPackageOverrides generationType (storagePackageMapping storageRead) (importPackageOverrides tableDef)

    -- Determine query module: use explicit queryModule from YAML if provided,
    -- otherwise use cachedQueries if fetchQuery matches a cached query, else use queries
    queryModulePrefix =
      case configPilot tableDef >>= cpQueryModule of
        Just _ -> "" -- when explicit queryModule is set, we use the full path directly
        Nothing ->
          let cqPrefix = storageRead.cachedQueryModulePrefix ++ "."
              qPrefix = storageRead.queryModulePrefix ++ "."
           in if any (\cq -> cQueryName cq == fromMaybe "" (cpFetchQuery <$> configPilot tableDef)) (cachedQueries tableDef)
                then cqPrefix
                else qPrefix

    queryImport =
      case configPilot tableDef >>= cpQueryModule of
        Just qm -> qm ++ " as SQ"
        Nothing -> queryModulePrefix ++ tblName ++ " as SQ"

    allSimpleImports :: [String]
    allSimpleImports =
      [ "Kernel.Prelude",
        "Kernel.Types.Id",
        "Lib.ConfigPilot.Interface.Types",
        "Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))",
        "Storage.Beam.Yudhishthira ()"
      ]
        <> simpleImp

    allQualifiedImports :: [String]
    allQualifiedImports =
      [ domainTypeModulePrefix ++ tblName ++ " as DT",
        "Lib.ConfigPilot.Interface.Getter as LCP",
        "Lib.Yudhishthira.Types as LYT",
        queryImport
      ]
        <> imports tableDef
        <> qualifiedImp

mkCodeBody :: StorageRead -> ConfigPilotDef -> StorageM ()
mkCodeBody storageRead cpDef = do
  tableDef <- ask
  let tblName = tableNameHaskell tableDef
      dimsName = tblName ++ "Dimensions"
      cfgType = cpConfigType cpDef
      fetchQ = cpFetchQuery cpDef
      retType = cpReturnType cpDef
      filterDims = cpFilterDimensions cpDef
      configDomain = fromMaybe (storageRead.configPilotConfigDomain) (cpConfigDomain cpDef)
      fetchQArgs = maybe "" (" " ++) (cpFetchQueryArgs cpDef)

      -- Look up field types for filter dimensions from TableDef fields
      lookupFieldType fName =
        case find (\f -> fieldName f == fName) (fields tableDef) of
          Just f -> haskellType f
          Nothing -> "TODO_TYPE"

      isMaybeType t = take 6 t == "Maybe " || "Kernel.Prelude.Maybe " `isPrefixOf` t

      -- Build dimension record fields
      -- If domain type is already Maybe, keep it; otherwise wrap in Maybe
      dimFields =
        ("merchantOperatingCityId", "Text") :
          [ let ft = lookupFieldType d
             in (d, if isMaybeType ft then ft else "Maybe " ++ ft)
            | d <- filterDims
          ]

  -- Data declaration
  onNewLine $ tellM $ "data " ++ dimsName ++ " = " ++ dimsName
  onNewLine $ tellM "  { "
  let fieldLines = [fn ++ " :: " ++ ft | (fn, ft) <- dimFields]
  tellM $ intercalate ",\n    " fieldLines
  onNewLine $ tellM "  }"
  onNewLine $ tellM $ "  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)"

  -- ConfigTypeInfo instance
  onNewLine $ newLine
  onNewLine $ tellM $ "instance ConfigTypeInfo '" ++ cfgType ++ " where"
  onNewLine $ tellM $ "  type DimensionsFor '" ++ cfgType ++ " = " ++ dimsName
  onNewLine $ tellM $ "  configTypeValue = " ++ cfgType
  onNewLine $ tellM $ "  sConfigType = S" ++ cfgType

  -- ConfigDimensions instance
  onNewLine $ newLine
  onNewLine $ tellM $ "instance ConfigDimensions " ++ dimsName ++ " where"
  onNewLine $ tellM $ "  type ConfigTypeOf " ++ dimsName ++ " = '" ++ cfgType
  case retType of
    CPList -> onNewLine $ tellM $ "  type ConfigValueTypeOf " ++ dimsName ++ " = [DT." ++ tblName ++ "]"
    CPMaybe -> onNewLine $ tellM $ "  type ConfigValueTypeOf " ++ dimsName ++ " = Maybe DT." ++ tblName
  onNewLine $ tellM $ "  getConfigType _ = " ++ cfgType

  -- getConfigList
  case retType of
    CPMaybe -> do
      onNewLine $ tellM "  getConfigList a ="
      onNewLine $ tellM "    listToMaybe"
      onNewLine $ tellM "      <$> LCP.resolveConfigList"
      onNewLine $ tellM $ "        a"
      onNewLine $ tellM $ "        (LYT." ++ configDomain ++ " " ++ cfgType ++ ")"
      onNewLine $ tellM "        (Id a.merchantOperatingCityId)"
      onNewLine $ tellM $ "        (maybeToList <$> SQ." ++ fetchQ ++ " (Id a.merchantOperatingCityId)" ++ fetchQArgs ++ ")"
      onNewLine $ tellM $ "        (" ++ buildMatchers dimsName tblName lookupFieldType filterDims ++ ")"
      onNewLine $ tellM "        Nothing"
    CPList -> do
      onNewLine $ tellM "  getConfigList a ="
      onNewLine $ tellM "    LCP.resolveConfigList"
      onNewLine $ tellM $ "      a"
      onNewLine $ tellM $ "      (LYT." ++ configDomain ++ " " ++ cfgType ++ ")"
      onNewLine $ tellM "      (Id a.merchantOperatingCityId)"
      onNewLine $ tellM $ "      (SQ." ++ fetchQ ++ " (Id a.merchantOperatingCityId)" ++ fetchQArgs ++ ")"
      onNewLine $ tellM $ "      " ++ buildMatchers dimsName tblName lookupFieldType filterDims
      onNewLine $ tellM "      Nothing"

buildMatchers :: String -> String -> (String -> String) -> [String] -> String
buildMatchers dimsName tblName _ [] =
  "([] :: [LCP.DimMatcher " ++ dimsName ++ " DT." ++ tblName ++ "])"
buildMatchers _ _ lookupFieldType filterDims =
  "[ " ++ intercalate ",\n        " (map mkMatcher filterDims) ++ "\n      ]"
  where
    isMaybeType t = take 6 t == "Maybe " || "Kernel.Prelude.Maybe " `isPrefixOf` t
    mkMatcher d =
      let cfgExtractor =
            if isMaybeType (lookupFieldType d)
              then "(." ++ d ++ ")" -- field is already Maybe, use directly
              else "(Just . (." ++ d ++ "))" -- plain field, wrap in Just
       in "LCP.DimMatcher (." ++ d ++ ") " ++ cfgExtractor ++ " (==)"
