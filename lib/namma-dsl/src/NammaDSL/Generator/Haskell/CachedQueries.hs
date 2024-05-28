{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module NammaDSL.Generator.Haskell.CachedQueries where

import Control.Lens ((%~), (.~), (^.))
import Control.Monad (forM_, when)
import Control.Monad.Extra (whenJust)
import qualified Data.Text as T
import Control.Monad.Reader (ask)
import Data.Bifunctor (first)
import Data.Bool
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (find, intercalate, isInfixOf, isPrefixOf, nub)
import qualified Data.List.Extra as L
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, isNothing)
import qualified Data.Text as Text
import NammaDSL.Config (DefaultImports (..))
import NammaDSL.DSL.Syntax.Storage
import NammaDSL.Generator.Haskell.Common (checkForPackageOverrides)
import NammaDSL.GeneratorCore
import NammaDSL.Lib hiding (Writer, Q)
import qualified NammaDSL.Lib.TH as TH
import qualified NammaDSL.Lib.Types as TH
import NammaDSL.Utils
import Prelude

type Writer w = TH.Writer TableDef w

type Q w = TH.Q TableDef w

data CachedQueryCode = DefaultCachedQueryFile DefaultCachedQueryCode | WithExtraCachedQueryFile ExtraCachedQueryCode

data DefaultCachedQueryCode = DefaultCachedQueryCode
  { creadOnlyCode :: Code
  }
  deriving (Show)

data ExtraCachedQueryCode = ExtraCachedQueryCode
  { cdefaultCode :: DefaultCachedQueryCode,
    cextraQueryFile :: Code
  }
  deriving (Show)


generateCachedQueries :: DefaultImports -> StorageRead -> TableDef -> Maybe CachedQueryCode
generateCachedQueries (DefaultImports qualifiedImp simpleImp _) storageRead tableDef =
  if EXTRA_CACHED_QUERY_FILE `elem` extraOperations tableDef
    then
      Just $
        WithExtraCachedQueryFile $
          ExtraCachedQueryCode
            { cdefaultCode =
                DefaultCachedQueryCode
                  { creadOnlyCode = do
                      generateCode $
                        commonGeneratorInput
                          & moduleNm .~ readOnlyCodeModuleName ++ " (module " ++ readOnlyCodeModuleName ++ ", module ReExport)"
                          & codeBody .~ generateCodeBody (mkCodeBody storageRead) tableDef
                          & simpleImports %~ (++ ([readOnlyCodeModuleName ++ "Extra as ReExport"]))
                          & ghcOptions %~ (++ ["-Wno-dodgy-exports"])
                          & \cgi -> cgi & qualifiedImports %~ makeProperQualifiedImports (cgi ^. codeBody)
                  },
              cextraQueryFile =
                generateCode $
                  commonGeneratorInput
                    & moduleNm .~ readOnlyCodeModuleName ++ "Extra"
                    & codeBody .~ generateCodeBody extraFileCodeBody tableDef
                    & \cgi -> cgi & qualifiedImports %~ makeProperQualifiedImports (cgi ^. codeBody)
            }
    else
      let codeBody' = generateCodeBody (mkCodeBody storageRead) tableDef
       in
        if codeBody' == mempty then Nothing
        else
          Just $ DefaultCachedQueryFile $
            DefaultCachedQueryCode
              { creadOnlyCode = do
                  generateCode $
                    commonGeneratorInput
                      & moduleNm .~ readOnlyCodeModuleName
                      & codeBody .~ generateCodeBody (mkCodeBody storageRead) tableDef
                      & ghcOptions %~ (++ ["-Wno-dodgy-exports"])
                      & \cgi -> cgi & qualifiedImports %~ makeProperQualifiedImports (cgi ^. codeBody)
              }
  where
    beamTypeModulePrefix = storageRead.beamTypeModulePrefix ++ "."
    domainTypeModulePrefix = storageRead.domainTypeModulePrefix ++ "."
    queryModulePrefix = storageRead.queryModulePrefix ++ "."
    cachedQueryModulePrefix = storageRead.cachedQueryModulePrefix ++ "."
    readOnlyCodeModuleName = cachedQueryModulePrefix ++ (capitalize $ tableNameHaskell tableDef)

    packageOverride :: [String] -> [String]
    packageOverride = checkForPackageOverrides (importPackageOverrides tableDef)

    allQualifiedImports :: [String]
    allQualifiedImports =
      [
        domainTypeModulePrefix ++ tableNameHaskell tableDef,
        beamTypeModulePrefix ++ tableNameHaskell tableDef,
        queryModulePrefix ++ tableNameHaskell tableDef ++ " as Queries"
      ]
       <> concatMap getAllCachedQueryRelatedImports (cachedQueries tableDef)
       <> imports tableDef
       <> qualifiedImp

    getAllCachedQueryRelatedImports :: CachedQueryDef -> [String]
    getAllCachedQueryRelatedImports cachedQuery =
        (paramRelatedImports cachedQuery
          ++ [cachedQuery.dbQuery]
          ++ maybe [] pure cachedQuery.ctypeConstraint
          ++ maybe [] pure cachedQuery.keyMaker)
           & figureOutImports
           & nub

    paramRelatedImports :: CachedQueryDef -> [String]
    paramRelatedImports cdf = concatMap (\param ->
            case param of
              Constant vl tp -> case tp of
                PImportedData -> [vl]
                _ -> []
              Variable _ tp -> [tp]) (cdf.dbQueryParams <> cdf.keyParams)

    commonGeneratorInput :: GeneratorInput
    commonGeneratorInput =
        GeneratorInput
        { _ghcOptions = ["-Wno-orphans", "-Wno-unused-imports"],
          _extensions = [],
          _moduleNm = mempty,
          _simpleImports = packageOverride simpleImp,
          _qualifiedImports = allQualifiedImports,
          _codeBody = mempty
        }
    makeProperQualifiedImports :: Code -> [String] -> [String]
    makeProperQualifiedImports cd = packageOverride . (removeUnusedQualifiedImports cd)

extraFileCodeBody :: StorageM ()
extraFileCodeBody = do
  onNewLine $ tellM "-- Extra code goes here -- "


mkCodeBody :: StorageRead -> StorageM ()
mkCodeBody storageRead = do
   tableDef <- ask
   tellM . fromMaybe mempty $ interpreter tableDef $ do
    forM_ (cachedQueries tableDef) $
      \cachedQuery -> generateCachedQuery storageRead tableDef cachedQuery

generateCachedQuery :: StorageRead -> TableDef -> CachedQueryDef -> Writer CodeUnit
generateCachedQuery storageRead tableDef cachedQuery = do
  let _funcSign = maybe ([_EsqDBFlow, _MonadFlow, _CacheFlow]) (pure . vT) (cachedQuery.ctypeConstraint)
      domainTypeModulePrefix = storageRead.domainTypeModulePrefix <> "."
      dType = cT $ domainTypeModulePrefix ++ tableDef.tableNameHaskell ++ "." ++ tableDef.tableNameHaskell
      _functionParamFields = sortParamsAccordingToOrder $ filter (\x -> case x of
          Constant _ _ -> False
          Variable _ _ -> True) (L.nubBy nubByParamComparator $ keyParams cachedQuery <> dbQueryParams cachedQuery)
  TH.decsW $ do
    TH.sigDW (TH.mkName cachedQuery.cQueryName) $ do
      TH.forallT [] _funcSign $ do
        let returnType =  vT "m" ~~ generateCachedQueryReturnType
        let typeParams = cT . getVarParamType <$> _functionParamFields
        TH.appendInfixT "->" $ NE.fromList (typeParams <> [returnType])
    TH.funDW (TH.mkName cachedQuery.cQueryName) $ do
      let patParams = vP . getParamName <$> _functionParamFields
      TH.clauseW patParams $
        TH.normalB $
          TH.doEW cachedQueryStmts
  where
  sortParamsAccordingToOrder :: [Param] -> [Param]
  sortParamsAccordingToOrder params =
    if isNothing cachedQuery.paramsOrder then params
     else L.sortBy (\p1 p2 -> compare (getIndex p1) (getIndex p2)) params
    where
        getIndex param = maybe maxBound id $ L.elemIndex (getParamName param) (fromMaybe [] cachedQuery.paramsOrder)

  cachedQueryStmts :: Writer TH.Stmt
  cachedQueryStmts = noBindSW (hedisCrossAppRedis ~>>= lambdaCaseE hedisMatches)

  findQueryExpr :: Q TH.Exp
  findQueryExpr = do
    let qualifiedQuery = if '.' `elem` cachedQuery.dbQuery then vE cachedQuery.dbQuery else vE ("Queries." ++ cachedQuery.dbQuery)
    appendInfixE (vE " ") $ NE.fromList $ [qualifiedQuery] ++ map directPassParamToExpr (dbQueryParams cachedQuery)

  hedisMatches :: [Q TH.Match]
  hedisMatches = [
     matchWOD (vP "Just a") (normalB $ hedisCacheHitBody),
     matchWOD (vP "Nothing") (normalB (hedisCacheMissBody ~/=<< findQueryExpr))
   ]
  hedisCacheHitBody :: Q TH.Exp
  hedisCacheHitBody = case cachedQuery.cacheDataType of
    COne -> vE "pure" ~ (vE "Just" ~ vE "a")
    CArray -> vE "pure" ~ vE "a"

  hedisCacheMissBody :: Q TH.Exp
  hedisCacheMissBody = case cachedQuery.cacheDataType of
    CArray -> hedisQueryAndCacheBody
    COne -> vE "flip whenJust" ~ hedisQueryAndCacheBody

  hedisQueryAndCacheBody :: Q TH.Exp
  hedisQueryAndCacheBody = TH.lamEE [vP "dataToBeCached"] (TH.doEW $ do
    vP "expTime" <-- vE "fromIntegral" ~<$> (vE "asks" ~ vE ".cacheConfig.configsExpTime")
    noBindSW $ hedisSetExpr
    )
  hedisSetExpr :: Q TH.Exp
  hedisSetExpr = do
    let setExp = vE "Hedis.setExp" ~ (parenE makeKeyExpr) ~ vE "dataToBeCached" ~ vE "expTime"
    if cachedQuery.withCrossAppRedis then
      vE "Hedis.withCrossAppRedis" ~$ setExp
    else setExp

  directPassParamToExpr :: Param -> Q TH.Exp
  directPassParamToExpr = \case
    Constant vl tp
      | tp == PString -> strE vl
      | tp == PInt -> vE vl
      | tp == PBool -> vE vl
      | tp == PDouble -> vE vl
      | tp == PImportedData -> vE vl
      | otherwise -> vE vl
    Variable vl _ -> vE vl

  hedisCrossAppRedis :: Q TH.Exp
  hedisCrossAppRedis = if cachedQuery.withCrossAppRedis then
     vE "Hedis.withCrossAppRedis" ~ hedisGetExpr
    else hedisGetExpr

  hedisGetExpr :: Q TH.Exp
  hedisGetExpr = parenE (vE "Hedis.safeGet" ~$ makeKeyExpr)

  nubByParamComparator :: Param -> Param -> Bool
  nubByParamComparator p1 p2
    | Constant x _ <- p1, Constant y _ <- p2 = x == y
    | Variable x _ <- p1, Variable y _ <- p2 = x == y
    | otherwise = False

  generateCachedQueryReturnType :: Q TH.Type
  generateCachedQueryReturnType = do
    let domainTypeModulePrefix = storageRead.domainTypeModulePrefix <> "."
        dType = cT $ domainTypeModulePrefix ++ tableDef.tableNameHaskell ++ "." ++ tableDef.tableNameHaskell
    if cachedQuery.cacheDataType == CArray || "All" `L.isInfixOf` cachedQuery.cQueryName then
        listT ~~ dType
    else
      cT "Kernel.Prelude.Maybe" ~~ dType

  keyParamHeader :: Param -> Q TH.Exp
  keyParamHeader = \case
    Constant _ _ -> strE ":Constant-"
    Variable vl _ -> strE (":" ++ capitalize vl ++ "-")

  keyParamToTextExpr :: Param -> Q TH.Exp
  keyParamToTextExpr = \case
    Constant vl tp
      | tp == PString -> strE vl
      | tp == PInt -> _Show ~ (vE vl)
      | tp == PBool -> _Show ~ (vE vl)
      | tp == PDouble -> _Show ~ (vE vl)
      | tp == PImportedData -> _Show ~ (vE vl)
      | otherwise -> vE vl
    Variable vl tp
      | "Kernel.Types.Id.Id " `L.isPrefixOf` tp -> _getId ~ vE vl
      | "Kernel.Types.Id.Id " `L.isInfixOf` tp -> _Show ~ (_getId ~<$> vE vl)
      | "Kernel.Types.Id.ShortId " `L.isPrefixOf` tp -> _getShortId ~ vE vl
      | "Kernel.Types.Id.ShortId " `L.isInfixOf` tp -> _Show ~ (_getShortId ~<$> vE vl)
      | otherwise -> _Show ~ vE vl

  keyParamMaker :: Param -> [Q TH.Exp]
  keyParamMaker prm = [keyParamHeader prm, keyParamToTextExpr prm]

  defaultCachedQueryKeyPrefix :: Q TH.Exp
  defaultCachedQueryKeyPrefix = strE (storageRead.defaultCachedQueryKeyPfx ++ "CachedQueries:" ++ tableDef.tableNameHaskell ++ ":")

  makeKeyExpr :: Q TH.Exp
  makeKeyExpr = maybe
    (appendInfixE (vE "<>") $ NE.fromList (defaultCachedQueryKeyPrefix : concatMap keyParamMaker (keyParams cachedQuery)))
    (\km -> appendInfixE (vE " ") $ NE.fromList $ [vE km] ++ map directPassParamToExpr (keyParams cachedQuery)) cachedQuery.keyMaker


_EsqDBFlow :: Q TH.Type
_EsqDBFlow = cT "EsqDBFlow" ~~ vT "m" ~~ vT "r"

_MonadFlow :: Q TH.Type
_MonadFlow = cT "MonadFlow" ~~ vT "m"

_CacheFlow :: Q TH.Type
_CacheFlow = cT "CacheFlow" ~~ vT "m" ~~ vT "r"

_HasSchemaName :: String -> Q TH.Type
_HasSchemaName tableName = cT "HasSchemaName" ~~ cT ("Beam." <> tableName <> "T")

_UnitType :: Q TH.Type
_UnitType = pure $ TH.TupleT 0

_getId :: Q TH.Exp
_getId = vE "Kernel.Types.Id.getId"

_getShortId :: Q TH.Exp
_getShortId = vE "Kernel.Types.Id.getShortId"

_Show :: Q TH.Exp
_Show = vE "show"