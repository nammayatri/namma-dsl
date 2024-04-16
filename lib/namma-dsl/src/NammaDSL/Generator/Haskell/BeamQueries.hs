{-# LANGUAGE LambdaCase #-}

module NammaDSL.Generator.Haskell.BeamQueries (generateBeamQueries, BeamQueryCode (..), DefaultQueryCode (..), ExtraQueryCode (..)) where

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
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import qualified Data.Text as Text
import NammaDSL.Config (DefaultImports (..))
import NammaDSL.DSL.Syntax.Storage
import NammaDSL.Generator.Haskell.Common (checkForPackageOverrides)
import NammaDSL.GeneratorCore
import NammaDSL.Lib hiding (Writer, Q)
import qualified NammaDSL.Lib.TH as TH
import qualified NammaDSL.Lib.Types as TH
import NammaDSL.Utils
-- import qualified Debug.Trace as DT
import Prelude

type Writer w = TH.Writer TableDef w

type Q w = TH.Q TableDef w

data BeamQueryCode = DefaultQueryFile DefaultQueryCode | WithExtraQueryFile ExtraQueryCode

data DefaultQueryCode = DefaultQueryCode
  { readOnlyCode :: Code,
    transformerCode :: Maybe Code
  }
  deriving (Show)

data ExtraQueryCode = ExtraQueryCode
  { defaultCode :: DefaultQueryCode,
    instanceCode :: Code,
    extraQueryFile :: Code
  }
  deriving (Show)

generateBeamQueries :: DefaultImports -> StorageRead -> TableDef -> BeamQueryCode
generateBeamQueries (DefaultImports qualifiedImp simpleImp _) storageRead tableDef =
  if EXTRA_QUERY_FILE `elem` extraOperations tableDef
    then
      WithExtraQueryFile $
        ExtraQueryCode
          { defaultCode =
              DefaultQueryCode
                { readOnlyCode = do
                    generateCode $
                      commonGeneratorInput
                        & moduleNm .~ readOnlyCodeModuleName ++ " (module " ++ readOnlyCodeModuleName ++ ", module ReExport)"
                        & codeBody .~ generateCodeBody (mkCodeBody storageRead) tableDef
                        & simpleImports %~ (++ ([readOnlyCodeModuleName ++ "Extra as ReExport"] ++ (if transformerCode' == mempty then [] else [extraTransformerModulePrefix ++ (capitalize $ tableNameHaskell tableDef)])))
                        & ghcOptions %~ (++ ["-Wno-dodgy-exports"])
                        & \cgi -> cgi & qualifiedImports %~ makeProperQualifiedImports (cgi ^. codeBody),
                  transformerCode =
                    if transformerCode' == mempty
                      then Nothing
                      else
                        Just $
                          generateCode $
                            commonGeneratorInput
                              & moduleNm .~ extraTransformerModulePrefix ++ (capitalize $ tableNameHaskell tableDef)
                              & codeBody .~ transformerCode'
                              & \cgi -> cgi & qualifiedImports %~ makeProperQualifiedImports (cgi ^. codeBody)
                },
            instanceCode = do
              generateCode $
                commonGeneratorInput
                  & moduleNm .~ orphanInstancesModulePrefix ++ (capitalize $ tableNameHaskell tableDef)
                  & codeBody .~ generateCodeBody (mkTTypeInstance storageRead) tableDef
                  & simpleImports %~ (++ (if transformerCode' == mempty then [] else [extraTransformerModulePrefix ++ (capitalize $ tableNameHaskell tableDef)]))
                  & \cgi -> cgi & qualifiedImports %~ makeProperQualifiedImports (cgi ^. codeBody),
            extraQueryFile =
              generateCode $
                commonGeneratorInput
                  & moduleNm .~ readOnlyCodeModuleName ++ "Extra"
                  & codeBody .~ generateCodeBody extraFileCodeBody tableDef
                  & simpleImports %~ (++ [orphanInstancesModulePrefix ++ (capitalize $ tableNameHaskell tableDef)])
                  & \cgi -> cgi & qualifiedImports %~ makeProperQualifiedImports (cgi ^. codeBody)
          }
    else
      DefaultQueryFile $
        DefaultQueryCode
          { readOnlyCode = do
              generateCode $
                commonGeneratorInput
                  & moduleNm .~ readOnlyCodeModuleName
                  & codeBody .~ generateCodeBody (mkCodeBody storageRead) tableDef
                  & simpleImports %~ (++ (if transformerCode' == mempty then [] else ["Storage.Queries.Transformers." ++ (capitalize $ tableNameHaskell tableDef)]))
                  & ghcOptions %~ (++ ["-Wno-dodgy-exports"])
                  & \cgi -> cgi & qualifiedImports %~ makeProperQualifiedImports (cgi ^. codeBody),
            transformerCode =
              if transformerCode' == mempty
                then Nothing
                else
                  Just $
                    generateCode $
                      commonGeneratorInput
                        & moduleNm .~ extraTransformerModulePrefix ++ (capitalize $ tableNameHaskell tableDef)
                        & codeBody .~ transformerCode'
                        & \cgi -> cgi & qualifiedImports %~ makeProperQualifiedImports (cgi ^. codeBody)
          }
  where
    beamTypeModulePrefix = storageRead.beamTypeModulePrefix ++ "."
    domainTypeModulePrefix = storageRead.domainTypeModulePrefix ++ "."
    queryModulePrefix = storageRead.queryModulePrefix ++ "."
    orphanInstancesModulePrefix = queryModulePrefix ++ "OrphanInstances."
    extraTransformerModulePrefix = queryModulePrefix ++ "Transformers."

    transformerCode' :: Code
    transformerCode' = generateCodeBody mkTransformerCodeBody tableDef

    readOnlyCodeModuleName = queryModulePrefix ++ (capitalize $ tableNameHaskell tableDef)

    packageOverride :: [String] -> [String]
    packageOverride = checkForPackageOverrides (importPackageOverrides tableDef)

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

    allQualifiedImports :: [String]
    allQualifiedImports =
      [ domainTypeModulePrefix ++ tableNameHaskell tableDef,
        beamTypeModulePrefix ++ capitalize (tableNameHaskell tableDef) ++ " as Beam"
      ]
        <> imports tableDef
        <> getAllFunctionImports
        <> concatMap getStorageRelationImports tableDef.fields
        <> qualifiedImp

    getAllFunctionImports :: [String]
    getAllFunctionImports = fromTTypeFuncImports ++ toTTypeFuncImports
      where
        fromTTypeFuncImports :: [String]
        fromTTypeFuncImports =
          (tableDef.fields
            & map (fmap tfName . fromTType)
            & getAllJust
            & figureOutImports) <>
           (tableDef.intermediateTransformers.getFromTTypes
             & map (\(ITransformer _ fnc) -> tfName fnc)
             & figureOutImports
           )

        toTTypeFuncImports :: [String]
        toTTypeFuncImports =
          ((concatMap beamFields (tableDef.fields))
            & map (fmap tfName . bToTType)
            & getAllJust
            & figureOutImports) <>
            (tableDef.intermediateTransformers.getToTTypes
              & map (\(ITransformer _ fnc) -> tfName fnc)
              & figureOutImports
            )

    getStorageRelationImports :: FieldDef -> [String]
    getStorageRelationImports fieldDef =
      case rel of
        Just (WithId _ isCached, query) -> [getModule isCached ++ query]
        Just (WithIdStrict _ isCached, query) -> [getModule isCached ++ query]
        Just (_, query) -> [queryModulePrefix ++ query]
        Nothing -> []
      where
        getModule isFromCached = bool queryModulePrefix "Storage.CachedQueries." isFromCached -- What about cached query ?? -- TODO
        rel =
          if isJust (relation fieldDef) && isWithIdRelation (fromJust (relation fieldDef))
            then (\(_, b) -> (fromJust (relation fieldDef), b)) <$> getFieldRelationAndHaskellType (fieldDef.haskellType <> "|WithId")
            else getFieldRelationAndHaskellType fieldDef.haskellType

extraFileCodeBody :: StorageM ()
extraFileCodeBody = do
  onNewLine $ tellM "-- Extra code goes here -- "

-- FIXME add StorageRead to env
mkCodeBody :: StorageRead -> StorageM ()
mkCodeBody storageRead = do
  tableDef <- ask
  let excludedQueries = excludedDefaultQueries tableDef
  let isDefault = EXTRA_QUERY_FILE `notElem` extraOperations tableDef
  tellM . fromMaybe mempty $ interpreter tableDef $ do
    when ("create" `notElem` excludedQueries) $ generateDefaultCreateQuery storageRead
    when ("createMany" `notElem` excludedQueries) $ generateDefaultCreateManyQuery storageRead
    beamQueries storageRead
  when isDefault $ mkTTypeInstance storageRead

mkTTypeInstance :: StorageRead -> StorageM ()
mkTTypeInstance storageRead = do
  tableDef <- ask
  tellM . fromMaybe mempty $ interpreter tableDef $ do
    fromTTypeInstance storageRead
    toTTypeInstance storageRead

mkTransformerCodeBody :: StorageM ()
mkTransformerCodeBody = do
  tableDef <- ask
  tellM . fromMaybe mempty $ interpreter tableDef $ do
    generateToTTypeFuncs
    generateFromTypeFuncs

generateDefaultCreateQuery :: StorageRead -> Writer CodeUnit
generateDefaultCreateQuery storageRead = do
  tableDef <- ask
  let isHasSchemaNameRequired' = isHasSchemaNameRequired tableDef
  let name = tableNameHaskell tableDef
  let withIdFields = getAllFieldsWithIdRelation (fields tableDef)
  let dName = domainTypeModulePrefix ++ name ++ "." ++ name
  TH.decsW $ do
    TH.sigDW "create" $ do
      TH.forallT [] ([_EsqDBFlow, _MonadFlow, _CacheFlow] <> [_HasSchemaName name | isHasSchemaNameRequired' ]) $
        cT dName --> vT "m" ~~ _UnitType
    if null withIdFields
      then do
        TH.funDW "create" $ do
          TH.clauseW [] $
            TH.normalB $
              TH.vE "createWithKV"
      else do
        TH.funDW "create" $ do
          TH.clauseW [vP "tbl"] $
            TH.normalB $
              TH.doEW do
                forM_ withIdFields makeCreateWithIdFunctionLine
                TH.noBindSW $ vE "createWithKV" ~ vE "tbl"
  where
    domainTypeModulePrefix = storageRead.domainTypeModulePrefix <> "."
    queriesModulePrefix = storageRead.queryModulePrefix <> "."
    makeCreateWithIdFunctionLine :: FieldDef -> Writer TH.Stmt
    makeCreateWithIdFunctionLine field = do
      let fieldExpr = vE ("tbl" #. fieldName field)
      let createFunc cache = getModule cache ++ snd (fromJust (getFieldRelationAndHaskellType (haskellType field <> "|WithId"))) ++ ".create"
      case fromJust (relation field) of
        WithId True cache -> do
          noBindSW $ vE "Kernel.Prelude.whenJust" ~ fieldExpr ~ vE (createFunc cache)
        WithIdStrict True cache -> do
          noBindSW $ vE (createFunc cache) ~ fieldExpr
        _ -> pure ()
      where
        getModule isFromCached = bool queriesModulePrefix "Storage.CachedQueries." isFromCached -- Cached queries ?? -- TODO
    getAllFieldsWithIdRelation :: [FieldDef] -> [FieldDef]
    getAllFieldsWithIdRelation = filter (\f -> isJust (relation f) && isWithIdRelation (fromJust (relation f)))

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


generateDefaultCreateManyQuery :: StorageRead -> Writer CodeUnit
generateDefaultCreateManyQuery storageRead = do
  def <- ask
  let name = tableNameHaskell def
  let isHasSchemaNameRequired' = isHasSchemaNameRequired def
  let domainTypeModulePrefix = storageRead.domainTypeModulePrefix <> "."
  let dName = domainTypeModulePrefix ++ name ++ "." ++ name
  TH.decsW $ do
    TH.sigDW "createMany" $ do
      TH.forallT [] ([_EsqDBFlow, _MonadFlow, _CacheFlow] <> [_HasSchemaName name | isHasSchemaNameRequired']) $
        TH.listT ~~ cT dName
          --> vT "m" ~~ _UnitType
    TH.funDW "createMany" $ do
      TH.clauseW [] $
        TH.normalB $
          vE "traverse_" ~ vE "create"

-- hack for record wild cards
wildRecordsP :: String -> Q TH.Pat
wildRecordsP name = vP $ name <> " {..}"

fromTTypeInstance :: StorageRead -> Writer CodeUnit
fromTTypeInstance storageRead = do
  let domainTypeModulePrefix = storageRead.domainTypeModulePrefix <> "."
  tableDef <- ask
  let beamType = "Beam." <> tableNameHaskell tableDef
  let beamTypeT = "Beam." ++ tableNameHaskell tableDef ++ "T"
  let domainType = domainTypeModulePrefix ++ tableNameHaskell tableDef ++ "." ++ tableNameHaskell tableDef
  TH.instanceDW (pure []) (TH.cT "FromTType'" ~~ cT beamType ~~ cT domainType) $ do
    TH.funDW "fromTType'" $ do
      TH.clauseW [wildRecordsP beamTypeT] $
        TH.normalB $
          TH.doEW $ do
            intermediateTransformerCode (tableDef.intermediateTransformers.getFromTTypes)
            monadicFromTTypeTransformerCode
            forM_ (fields tableDef) \field -> do
              fromTTypeMConversionFunction storageRead (tableNameHaskell tableDef) (haskellType field) (fieldName field) (relation field)

            TH.noBindSW $ TH.vE "pure" ~$ TH.vE "Just" ~ TH.recConEW (TH.mkName domainType) (forM_ (fields tableDef) fromField)
  where
    getFromTTypeParams :: FieldDef -> String
    getFromTTypeParams hfield = unwords $ map bFieldName (beamFields hfield)

    fromField field =
      -- if isEncrypted field
      --   then do
      --     TH.fieldExpW (TH.mkName $ fieldName field) do
      --       let mapOperator = if isMaybeType (haskellType field) then (~<$>) else (~)
      --       let applicativeOperator = if isMaybeType (haskellType field) then (~<*>) else (~)
      --       cE "EncryptedHashed"
      --         `mapOperator` (cE "Encrypted" `mapOperator` vE (fieldName field <> "Encrypted"))
      --         `applicativeOperator` vE (fieldName field ++ "Hash")
      --   else do
          TH.fieldExpW (TH.mkName $ fieldName field) do
            fromTTypeConversionFunction (fromTType field) (haskellType field) (getFromTTypeParams field) (relation field) (fieldName field)

monadicFromTTypeTransformerCode :: Writer TH.Stmt
monadicFromTTypeTransformerCode = do
  tableDef <- ask
  forM_ (fields tableDef) \hfield -> do
    whenJust (fromTType hfield) \tf ->
      case tfType tf of
        MonadicT -> do
          let transformerExp =
                if tfIsEmbeddedArgs tf then vE (tfName tf)
                else TH.appendE $ vE (tfName tf) NE.:| map (vE . bFieldName) (beamFields hfield)
          vP (fieldName hfield <> "'") <-- transformerExp
        PureT -> pure ()


intermediateTransformerCode :: [ITransformer] -> Writer TH.Stmt
intermediateTransformerCode itfs = do
  forM_ itfs (\(ITransformer outputVarName tf) ->
    case tfType tf of
      MonadicT -> do
          let transformerExp = vE (tfName tf)
          vP (outputVarName) <-- transformerExp
      PureT -> do
          let transformerExp = TH.VarE $ TH.mkName (tfName tf)
          letStmt (mkNameT $ T.pack outputVarName) transformerExp
   )

-- Is it correct? toTType' is not monadic function
monadicToTTypeTransformerCode :: Maybe [String] -> Writer TH.Stmt
monadicToTTypeTransformerCode specificFields = do
  tableDef <- ask
  let fieldsToTransform =
        if isJust specificFields
          then filter (\f -> fieldName f `elem` fromJust specificFields) (fields tableDef)
          else fields tableDef
  forM_ fieldsToTransform \hfield -> do
    forM_ (beamFields hfield) \field -> do
      whenJust (bToTType field) \tf -> do
        case tfType tf of
          MonadicT -> do
            let transformerExp =
                  if tfIsEmbeddedArgs tf then vE (tfName tf)
                  else vE (tfName tf) ~ toTTypeExtractorTH (makeExtractorFunctionTH $ bfieldExtractor field) (fieldName hfield)
            vP (bFieldName field <> "'") <-- transformerExp
          PureT -> pure ()

toTTypeInstance :: StorageRead -> Writer CodeUnit
toTTypeInstance storageRead = do
  let domainTypeModulePrefix = storageRead.domainTypeModulePrefix <> "."
  tableDef <- ask
  let beamType = "Beam." <> tableNameHaskell tableDef
  let beamTypeT = "Beam." <> tableNameHaskell tableDef <> "T"
  let domainType = domainTypeModulePrefix <> tableNameHaskell tableDef <> "." <> tableNameHaskell tableDef
  TH.instanceDW (pure []) (cT "ToTType'" ~~ cT beamType ~~ cT domainType) $ do
    TH.funDW "toTType'" $ do
      TH.clauseW [wildRecordsP domainType] $
        TH.normalB $
          TH.doEW $ do
            intermediateTransformerCode (tableDef.intermediateTransformers.getToTTypes)
            monadicToTTypeTransformerCode Nothing
            let fs = filter (removeBeamFieldsWRTRelation . relation) (fields tableDef)
            TH.noBindSW $
              TH.recConEW (TH.mkName beamTypeT) $
                forM_ fs $ \hfield -> do
                  forM_ (beamFields hfield) $ \field -> do
                    -- if bIsEncrypted field
                    --   then do
                    --     let mapOperator = if isMaybeType (bFieldType field) then (~<&>) else (~&)
                    --     if bFieldName field == fieldName hfield <> "Encrypted" then
                    --       TH.fieldExpW (TH.mkName $ "Beam." <> bFieldName field) $
                    --         vE (fieldName hfield) `mapOperator` (vE "unEncrypted" ~. vE "encrypted")
                    --     else
                    --       TH.fieldExpW (TH.mkName $ "Beam." <> bFieldName field) $
                    --         vE (fieldName hfield) `mapOperator` vE "hash"
                    --     pure ()
                    --   else do
                        TH.fieldExpW (TH.mkName $ "Beam." <> bFieldName field) $
                          toTTypeConversionFunction (bToTType field) (haskellType hfield) (toTTypeExtractor (makeExtractorFunction $ bfieldExtractor field) (fieldName hfield)) (bFieldName field)

beamQueries :: StorageRead -> Writer CodeUnit
beamQueries storageRead = do
  tableDef <- ask
  let excludedQueries = excludedDefaultQueries tableDef
  forM_ (queries tableDef ++ defaultQueryDefs tableDef) $
    \queryDef -> when (queryName queryDef `notElem` excludedQueries) $ generateBeamQuery storageRead tableDef.fields tableDef.tableNameHaskell queryDef

_Id :: Q TH.Exp
_Id = cE "Kernel.Types.Id.Id"

_ShortId :: Q TH.Exp
_ShortId = cE "Kernel.Types.Id.ShortId"

_getId :: Q TH.Exp
_getId = vE "Kernel.Types.Id.getId"

_getShortId :: Q TH.Exp
_getShortId = vE "Kernel.Types.Id.getShortId"


toTTypeConversionFunction :: Maybe TransformerFunction -> String -> String -> String -> Q TH.Exp
toTTypeConversionFunction transformer haskellType fieldName beamFieldName
  | isJust transformer =
    if tfType (fromJust transformer) == MonadicT
      then vE $ beamFieldName <> "'"
      else if tfIsEmbeddedArgs (fromJust transformer) then
        vE (tfName $ fromJust transformer)
      else vE (tfName $ fromJust transformer) ~ vE fieldName
  | "Kernel.Types.Id.Id " `Text.isPrefixOf` Text.pack haskellType = _getId ~ vE fieldName
  | "Kernel.Types.Id.Id " `Text.isInfixOf` Text.pack haskellType = _getId ~<$> vE fieldName
  | "Kernel.Types.Id.ShortId " `Text.isPrefixOf` Text.pack haskellType = _getShortId ~ vE fieldName
  | "Kernel.Types.Id.ShortId " `Text.isInfixOf` Text.pack haskellType = _getShortId ~<$> vE fieldName
  | otherwise = vE fieldName

fromTTypeConversionFunction :: Maybe TransformerFunction -> String -> String -> Maybe FieldRelation -> String -> Q TH.Exp
fromTTypeConversionFunction fromTTypeFunc haskellType fieldName relation dFieldName
  | isJust fromTTypeFunc =
      if tfType (fromJust fromTTypeFunc) == MonadicT then vE $ dFieldName ++ "'"
      else if tfIsEmbeddedArgs (fromJust fromTTypeFunc) then
        vE (tfName $ fromJust fromTTypeFunc)
      else vE (tfName $ fromJust fromTTypeFunc) ~ vE fieldName
  | isJust relation = if isWithIdRelation (fromJust relation) then vE $ dFieldName ++ "'" else vE $ fieldName ++ "'"
  | "Kernel.Types.Id.Id " `Text.isPrefixOf` Text.pack haskellType = _Id ~ vE fieldName
  | "Kernel.Types.Id.Id " `Text.isInfixOf` Text.pack haskellType = _Id ~<$> vE fieldName
  | "Kernel.Types.Id.ShortId " `Text.isPrefixOf` Text.pack haskellType = _ShortId ~ vE fieldName
  | "Kernel.Types.Id.ShortId " `Text.isInfixOf` Text.pack haskellType = _ShortId ~<$> vE fieldName
  | otherwise = vE fieldName

_fromMaybeM :: Q TH.Exp -> Q TH.Exp
_fromMaybeM = (vE "fromMaybeM" ~)

_InternalError :: String -> Q TH.Exp
_InternalError str = cE "InternalError" ~ strE str

fromTTypeMConversionFunction :: StorageRead -> String -> String -> String -> Maybe FieldRelation -> Writer TH.Stmt
fromTTypeMConversionFunction storageRead tableNameHaskell haskellType fieldName = \case
  Just relation -> do
    let fieldP = vP (fieldName <> "'")
    case relation of
      OneToOne -> do
        let moduleName = queriesModulePrefix <> snd (fromJust $ getFieldRelationAndHaskellType haskellType)
            funcExp = vE $ moduleName <> ".findBy" <> tableNameHaskell <> "Id"
        fieldP <-- (funcExp ~ (_Id ~ vE "id") ~>>= _fromMaybeM (_InternalError $ "Failed to get " <> fieldName <> "."))
      MaybeOneToOne -> do
        let moduleName = queriesModulePrefix <> snd (fromJust $ getFieldRelationAndHaskellType haskellType)
            funcExp = vE $ moduleName <> ".findBy" <> tableNameHaskell <> "Id"
        fieldP <-- (funcExp ~ (_Id ~ vE "id"))
      OneToMany -> do
        let moduleName = queriesModulePrefix <> snd (fromJust $ getFieldRelationAndHaskellType haskellType)
            funcExp = vE $ moduleName <> ".findAllBy" <> tableNameHaskell <> "Id"
        fieldP <-- (funcExp ~ (_Id ~ vE "id"))
      WithIdStrict _ isCached -> do
        let moduleName = getModule isCached <> snd (fromJust $ getFieldRelationAndHaskellType (haskellType <> "|WithId"))
            funcExp = vE $ moduleName <> ".findById"
        fieldP <-- (funcExp ~ (_Id ~ vE (fieldName <> "Id")))
      WithId _ isCached -> do
        let moduleName = getModule isCached <> snd (fromJust $ getFieldRelationAndHaskellType (haskellType <> "|WithId"))
            funcExp = vE $ moduleName <> ".findById"
        fieldP <-- (vE "maybe" ~ (vE "pure" ~ cE "Nothing") ~ (funcExp ~. _Id) ~ vE (fieldName <> "Id"))
  Nothing -> pure ()
  where
    queriesModulePrefix = storageRead.queryModulePrefix ++ "."
    getModule isFromCached = bool queriesModulePrefix "Storage.CachedQueries." isFromCached -- Chached query ?? --TODO

toTTypeExtractor :: Maybe String -> String -> String
toTTypeExtractor extractor field = maybe field (\x -> x ++ " " ++ field) extractor

toTTypeExtractorTH :: Maybe (Q TH.Exp) -> String -> Q TH.Exp
toTTypeExtractorTH extractor field = maybe (vE field) (\x -> x ~ vE field) extractor

generateBeamQuery :: StorageRead -> [FieldDef] -> String -> QueryDef -> Writer CodeUnit
generateBeamQuery storageRead allHaskellFields tableNameHaskell query = do
  let paramFieldNames = nub $ map (fst . fst) (params query) <> (fst <$> getWhereClauseFieldNamesAndTypes (whereClause query))
  withFunctionSignature storageRead query tableNameHaskell $ do
    monadicToTTypeTransformerCode (Just paramFieldNames)
    let queryParams = generateQueryParams allHaskellFields (query.params)
    generateBeamFunctionCall query.kvFunction $ queryParams <> [TH.listE genWhereClause] <> orderAndLimit query
  where
    genWhereClause = generateClause allHaskellFields query.takeFullObjectAsInput query.whereClause

orderAndLimit :: QueryDef -> [Q TH.Exp]
orderAndLimit query = do
  if query.kvFunction `elem` ["findAllWithOptionsKV", "findAllWithOptionsKV'", "findAllWithOptionsKVScheduler", "findAllWithOptionsDb"]
    then do
      let order = cE ("Se." <> show (snd query.orderBy)) ~ vE ("Beam." <> (fst query.orderBy))
      [order, vE "limit", vE "offset"]
    else []

ignoreEncryptionFlag :: ((String, String), Bool) -> (String, String)
ignoreEncryptionFlag ((field, tp), _) = (field, tp)

withFunctionSignature :: StorageRead -> QueryDef -> String -> Writer TH.Stmt -> Writer CodeUnit
withFunctionSignature storageRead query tableNameHaskell stmts = do
  def <- ask
  let isHasSchemaNameRequired' = isHasSchemaNameRequired def
      qParams = filter ((/= "updatedAt") . fst) $
        map getIdsOut $
          nub (map ignoreEncryptionFlag (params query) ++ addLimitParams query ++ getWhereClauseFieldNamesAndTypes (whereClause query))
  let domainTypeModulePrefix = storageRead.domainTypeModulePrefix <> "."
  TH.decsW $ do
    TH.sigDW (TH.mkName query.queryName) $ do
      TH.forallT [] ([_EsqDBFlow, _MonadFlow, _CacheFlow] <> [ (_HasSchemaName tableNameHaskell) | isHasSchemaNameRequired']) $ do
        let returnType = vT "m" ~~ generateQueryReturnType storageRead query.kvFunction tableNameHaskell
        let typeParams = if query.takeFullObjectAsInput
              then [cT (domainTypeModulePrefix <> tableNameHaskell <> "." <> tableNameHaskell)]
              else cT . snd <$> qParams
        TH.appendInfixT "->" $ NE.fromList (typeParams <> [returnType])
    TH.funDW (TH.mkName query.queryName) $ do
      let patParams = if query.takeFullObjectAsInput
            then [wildRecordsP $ domainTypeModulePrefix <> tableNameHaskell <> "." <> tableNameHaskell]
            else vP . fst <$> qParams
      TH.clauseW patParams $
        TH.normalB $
          TH.doEW stmts

addLimitParams :: QueryDef -> [(String, String)]
addLimitParams query =
  if query.kvFunction `elem` ["findAllWithOptionsKV", "findAllWithOptionsKV'", "findAllWithOptionsKVScheduler", "findAllWithOptionsDb"]
    then [("limit", "Maybe Int"), ("offset", "Maybe Int")]
    else []

getIdsOut :: (String, String) -> (String, String)
getIdsOut (k, t)
  | "Kernel.Types.Id.Id " `isPrefixOf` t = ("(Kernel.Types.Id.Id " ++ k ++ ")", t)
  | "Kernel.Types.Id.ShortId " `isPrefixOf` t = ("(Kernel.Types.Id.ShortId " ++ k ++ ")", t)
  | otherwise = (k, t)

generateQueryReturnType :: StorageRead -> String -> String -> Q TH.Type
generateQueryReturnType storageRead kvFunction tableNameHaskell = do
  let domainTypeModulePrefix = storageRead.domainTypeModulePrefix <> "."
  let dType = cT $ domainTypeModulePrefix ++ tableNameHaskell ++ "." ++ tableNameHaskell
  if kvFunction `elem` ["findOneWithKV", "findOneWithKVScheduler", "findOneWithDb"]
    then cT "Maybe" ~~ dType
    else
      if kvFunction `elem` ["findAllWithKV", "findAllWithKVScheduler", "findAllWithOptionsKV", "findAllWithOptionsKV'", "findAllWithOptionsKVScheduler", "findAllWithDb", "findAllWithOptionsDb", "findAllWithKVAndConditionalDB"]
        then TH.listT ~~ dType
        else _UnitType

getWhereClauseFieldNamesAndTypes :: WhereClause -> [(String, String)]
getWhereClauseFieldNamesAndTypes EmptyWhere = []
getWhereClauseFieldNamesAndTypes (Leaf (field, _type, op)) = if op == Just In then [(field, "[" <> _type <> "]")] else [(field, _type)]
getWhereClauseFieldNamesAndTypes (Query (_, clauses)) = concatMap getWhereClauseFieldNamesAndTypes clauses

generateBeamFunctionCall :: String -> [Q TH.Exp] -> Writer TH.Stmt
generateBeamFunctionCall kvFunction params = do
  when ("update" `isPrefixOf` kvFunction) $ do
    vP "_now" <-- vE "getCurrentTime"
  noBindSW $ TH.appendE $ vE kvFunction NE.:| params

generateQueryParams :: [FieldDef] -> [((String, String), Bool)] -> [Q TH.Exp]
generateQueryParams _ [] = []
generateQueryParams allFields params = pure @[] . TH.listEW $ forM_ params \((field, tp), _encrypted) -> do
  let fieldDef = fromMaybe (error "Param not found in data type") $ find (\f -> fieldName f == field) allFields
  forM_ (fieldDef.beamFields) \bField -> do
    if isJust fieldDef.relation
      then case fromJust fieldDef.relation of
        WithIdStrict _ _ -> TH.itemW $ cE "Se.Set" ~ vE ("Beam." <> bFieldName bField) ~ correctSetField field tp bField
        WithId _ _ -> TH.itemW $ cE "Se.Set" ~ vE ("Beam." <> bFieldName bField) ~ correctSetField field tp bField
        _ -> pure ()
      else
        -- if encrypted
        --   then do
        --     let mapOperator = if isMaybeType tp then (~<&>) else (~&)
        --     if "Encrypted" `isSuffixOf` (bFieldName bField) then
        --       TH.itemW $ cE "Se.Set" ~ vE ("Beam." <> bFieldName bField) ~$ vE field `mapOperator` (vE "unEncrypted" ~. vE "encrypted")
        --     else
        --       TH.itemW $ cE "Se.Set" ~ vE ("Beam." <> bFieldName bField) ~$ vE field `mapOperator` vE "hash"
        --  else
        TH.itemW $ cE "Se.Set" ~ vE ("Beam." <> bFieldName bField) ~ correctSetField field tp bField

correctSetField :: String -> String -> BeamField -> Q TH.Exp
correctSetField field tp beamField
  | isJust (bToTType beamField) =
      let tf = fromJust (bToTType beamField)
       in if tfType tf == MonadicT
          then vE $ bFieldName beamField <> "'"
          else if (tfIsEmbeddedArgs tf) then
            vE (tfName tf)
          else
            vE (tfName tf) ~ toTTypeExtractorTH (makeExtractorFunctionTH $ bfieldExtractor beamField) field
  | "Kernel.Types.Id.Id " `isInfixOf` tp && not ("Kernel.Types.Id.Id " `isPrefixOf` tp) = _getId ~<$> vE field
  | "Kernel.Types.Id.ShortId " `isInfixOf` tp && not ("Kernel.Types.Id.ShortId " `isPrefixOf` tp) = _getShortId ~<$> vE field
  | "Kernel.Types.Id.Id " `isPrefixOf` tp = _getId ~ vE field
  | "Kernel.Types.Id.ShortId " `isPrefixOf` tp = _getShortId ~ vE field
  | field == "updatedAt" && isNothing (bToTType beamField) = if "Kernel.Prelude.Maybe " `isPrefixOf` tp then vE "Just" ~ vE "_now" else vE "_now"
  | otherwise = toTTypeExtractorTH (makeExtractorFunctionTH $ bfieldExtractor beamField) field

correctEqField :: String -> String -> BeamField -> Q TH.Exp
correctEqField field tp beamField
  | isJust (bToTType beamField) =
      let tf = fromJust (bToTType beamField)
       in if tfType tf == MonadicT
          then vE $ bFieldName beamField <> "'"
          else if tfIsEmbeddedArgs tf then
            vE (tfName tf)
          else
            vE (tfName tf) ~ toTTypeExtractorTH (makeExtractorFunctionTH $ bfieldExtractor beamField) field
  | "Kernel.Types.Id.Id " `isInfixOf` tp && not ("Kernel.Types.Id.Id " `isPrefixOf` tp) = _getId ~<$> vE field
  | "Kernel.Types.Id.ShortId " `isInfixOf` tp && not ("Kernel.Types.Id.ShortId " `isPrefixOf` tp) = _getShortId ~<$> vE field
  | otherwise = toTTypeExtractorTH (makeExtractorFunctionTH $ bfieldExtractor beamField) field

-- Function to process each clause
generateClause :: [FieldDef] -> Bool -> WhereClause -> [Q TH.Exp]
generateClause _ _ EmptyWhere = []
generateClause allFields isFullObjInp (Leaf (field, tp, op)) = do
  let fieldDef = fromMaybe (error "Param not found in data type") $ find (\f -> fieldName f == field) allFields
  beamFields fieldDef <&> \bfield ->
    cE "Se.Is"
      ~ vE ("Beam." <> bFieldName bfield)
      ~$ operator (fromMaybe Eq op)
      ~ (if isFullObjInp then correctSetField field tp bfield else correctEqField field tp bfield)
generateClause allFields isFullObjInp (Query (op, clauses)) = do
  let expList = concat (generateClause allFields isFullObjInp <$> clauses)  -- is it correct concat?
  if op `elem` comparisonOperator
    then expList
    else [operator op ~ TH.listE expList]

generateToTTypeFuncs :: Writer CodeUnit
generateToTTypeFuncs  = do
  def <- ask
  forM_ (fields def) \field -> do
    forM_ (beamFields field) $ \bfield ->
      whenJust  (bToTType bfield) $ \tf ->
          if '.' `elem` tfName tf || tfIsEmbeddedArgs tf
            then pure ()
            else case tfType tf of
              PureT -> do
                TH.decsW $ do
                  TH.sigDW (TH.mkName $ tfName tf) $ do
                    TH.forallT [] [] $ cT (hFieldType bfield) --> cT (bFieldType bfield)
                  TH.funDW (TH.mkName $ tfName tf) $ do
                    TH.clauseW [] $ TH.normalB $ vE "error" ~ strE "TODO"
              MonadicT -> do
                TH.decsW $ do
                  TH.sigDW (TH.mkName $ tfName tf) $ do
                    TH.forallT [] [_MonadFlow] $ cT (hFieldType bfield) --> vT "m" ~~ cT ("(" ++ bFieldType bfield ++ ")")
                  TH.funDW (TH.mkName $ tfName tf) $ do
                    TH.clauseW [] $ TH.normalB $ vE "error" ~ strE "TODO"

generateFromTypeFuncs :: Writer CodeUnit
generateFromTypeFuncs = do
  def <- ask
  forM_ (fields def) $ \field -> do
    let (params, types) = first (map ("_" ++)) $ unzip $ map (\bfield -> (bFieldName bfield, bFieldType bfield)) (beamFields field)
        funcType = TH.appendInfixT "->" . NE.fromList $ cT <$> (types <> [haskellType field])
        funcTypeM = TH.forallT [] [_MonadFlow] $ TH.appendInfixT "->" . NE.fromList $ ((cT <$> types) <> [vT "m" ~~ cT ("(" ++ haskellType field ++ ")")])
    whenJust (fromTType field) $ \tf ->
      if '.' `elem` tfName tf || tfIsEmbeddedArgs tf
        then pure ()
        else case tfType tf of
          PureT -> TH.decsW $ do
            TH.sigDW (TH.mkName $ tfName tf) funcType
            TH.funDW (TH.mkName $ tfName tf) $ do
              TH.clauseW (vP <$> params) $ TH.normalB $ vE "error" ~ strE "TODO"
          MonadicT -> TH.decsW $ do
            TH.sigDW (TH.mkName $ tfName tf) funcTypeM
            TH.funDW (TH.mkName $ tfName tf) $ do
              TH.clauseW (vP <$> params) $ TH.normalB $ vE "error" ~ strE "TODO"

operator :: Operator -> Q TH.Exp
operator = cE . ("Se." <>) . show

defaultQueryDefs :: TableDef -> [QueryDef]
defaultQueryDefs tableDef =
  [ QueryDef "findByPrimaryKey" "findOneWithKV" [] findByPrimaryKeyWhereClause defaultOrderBy False,
    QueryDef "updateByPrimaryKey" "updateWithKV" (getAllFieldNamesWithTypesExcludingPks (primaryKey tableDef) (fields tableDef)) findByPrimaryKeyWhereClause defaultOrderBy True
  ]
  where
    getAllFieldNamesWithTypesExcludingPks :: [String] -> [FieldDef] -> [((String, String), Bool)]
    getAllFieldNamesWithTypesExcludingPks pks fieldDefs = map (\fieldDef -> ((fieldName fieldDef, haskellType fieldDef), isEncrypted fieldDef)) $ filter (\fieldDef -> fieldName fieldDef `notElem` pks) fieldDefs

    getAllPrimaryKeyWithTypes :: [String] -> [FieldDef] -> [(String, String, Maybe Operator)]
    getAllPrimaryKeyWithTypes pks fieldDefs = map (\fieldDef -> (fieldName fieldDef, haskellType fieldDef, Nothing)) $ filter (\fieldDef -> fieldName fieldDef `elem` pks) fieldDefs

    primaryKeysAndTypes :: [(String, String, Maybe Operator)]
    primaryKeysAndTypes = getAllPrimaryKeyWithTypes (primaryKey tableDef) (fields tableDef)

    findByPrimaryKeyWhereClause :: WhereClause
    findByPrimaryKeyWhereClause = Query (And, Leaf <$> primaryKeysAndTypes)

makeExtractorFunctionTH :: [String] -> Maybe (Q TH.Exp)
makeExtractorFunctionTH funcs = case vE <$> funcs of
  [] -> Nothing
  v : vs -> Just $ TH.appendInfixE (vE ".") $ v NE.:| vs

makeExtractorFunction :: [String] -> Maybe String
makeExtractorFunction funcs =
  if null funcs then Nothing else Just $ if length funcs > 1 then "( " ++ extractor ++ " )" else extractor
  where
    extractor = intercalate " . " funcs

isWithIdRelation :: FieldRelation -> Bool
isWithIdRelation = \case
  WithId _ _ -> True
  WithIdStrict _ _ -> True
  _ -> False


isHasSchemaNameRequired :: TableDef -> Bool
isHasSchemaNameRequired _def = any (\case
      MakeTableInstancesGenericSchema -> True
      Custom nm _ _ -> "mkTableInstancesGenericSchema" `isInfixOf` nm
      _ -> False
  ) (beamTableInstance _def)