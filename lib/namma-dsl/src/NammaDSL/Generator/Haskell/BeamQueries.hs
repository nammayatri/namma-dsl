module NammaDSL.Generator.Haskell.BeamQueries (generateBeamQueries, BeamQueryCode (..), DefaultQueryCode (..), ExtraQueryCode (..)) where

import Control.Lens ((%~), (.~))
import Control.Monad (when)
import Control.Monad.Reader (ask)
import Data.Bifunctor (first)
import Data.Bool
import Data.Function ((&))
import Data.List (find, intercalate, isInfixOf, isPrefixOf, nub)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import qualified Data.Text as Text
import NammaDSL.Config (DefaultImports (..))
import NammaDSL.DSL.Syntax.Storage
import NammaDSL.Generator.Haskell.Common (checkForPackageOverrides)
import NammaDSL.GeneratorCore
import NammaDSL.Utils
import Prelude

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
                { readOnlyCode =
                    generateCode $
                      commonGeneratorInput
                        & moduleNm .~ readOnlyCodeModuleName ++ " (module " ++ readOnlyCodeModuleName ++ ", module ReExport)"
                        & codeBody .~ generateCodeBody (mkCodeBody storageRead) tableDef
                        & simpleImports %~ (++ ([readOnlyCodeModuleName ++ "Extra as ReExport"] ++ (if transformerCode' == mempty then [] else [extraTransformerModulePrefix ++ (capitalize $ tableNameHaskell tableDef)])))
                        & ghcOptions %~ (++ ["-Wno-dodgy-exports"]),
                  transformerCode =
                    if transformerCode' == mempty
                      then Nothing
                      else
                        Just $
                          generateCode $
                            commonGeneratorInput
                              & moduleNm .~ extraTransformerModulePrefix ++ (capitalize $ tableNameHaskell tableDef)
                              & codeBody .~ transformerCode'
                },
            instanceCode =
              generateCode $
                commonGeneratorInput
                  & moduleNm .~ orphanInstancesModulePrefix ++ (capitalize $ tableNameHaskell tableDef)
                  & codeBody .~ generateCodeBody (mkTTypeInstance storageRead) tableDef
                  & simpleImports %~ (++ (if transformerCode' == mempty then [] else [extraTransformerModulePrefix ++ (capitalize $ tableNameHaskell tableDef)])),
            extraQueryFile =
              generateCode $
                commonGeneratorInput
                  & moduleNm .~ readOnlyCodeModuleName ++ "Extra"
                  & codeBody .~ generateCodeBody extraFileCodeBody tableDef
                  & simpleImports %~ (++ [orphanInstancesModulePrefix ++ (capitalize $ tableNameHaskell tableDef)])
          }
    else
      DefaultQueryFile $
        DefaultQueryCode
          { readOnlyCode =
              generateCode $
                commonGeneratorInput
                  & moduleNm .~ readOnlyCodeModuleName
                  & codeBody .~ generateCodeBody (mkCodeBody storageRead) tableDef
                  & simpleImports %~ (++ (if transformerCode' == mempty then [] else ["Storage.Queries.Transformers." ++ (capitalize $ tableNameHaskell tableDef)]))
                  & ghcOptions %~ (++ ["-Wno-dodgy-exports"]),
            transformerCode =
              if transformerCode' == mempty
                then Nothing
                else
                  Just $
                    generateCode $
                      commonGeneratorInput
                        & moduleNm .~ extraTransformerModulePrefix ++ (capitalize $ tableNameHaskell tableDef)
                        & codeBody .~ transformerCode'
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
          _simpleImports = packageOverride allSimpleImports,
          _qualifiedImports = packageOverride allQualifiedImports,
          _codeBody = mempty
        }
    allSimpleImports :: [String]
    allSimpleImports =
      [ "Kernel.Beam.Functions",
        "Kernel.Prelude",
        "Kernel.External.Encryption",
        "Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)",
        "Kernel.Types.Error"
      ]
        <> simpleImp
    allQualifiedImports :: [String]
    allQualifiedImports =
      [ domainTypeModulePrefix ++ tableNameHaskell tableDef,
        beamTypeModulePrefix ++ capitalize (tableNameHaskell tableDef) ++ " as Beam",
        "Sequelize as Se"
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
          tableDef.fields
            & map (fmap tfName . fromTType)
            & getAllJust
            & figureOutImports

        toTTypeFuncImports :: [String]
        toTTypeFuncImports =
          (concatMap beamFields (tableDef.fields))
            & map (fmap tfName . bToTType)
            & getAllJust
            & figureOutImports

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

mkCodeBody :: StorageRead -> StorageM ()
mkCodeBody storageRead = do
  tableDef <- ask
  let isDefault = EXTRA_QUERY_FILE `notElem` extraOperations tableDef
  generateDefaultCreateQuery storageRead
  generateDefaultCreateManyQuery storageRead
  beamQueries storageRead
  when isDefault (mkTTypeInstance storageRead)

mkTTypeInstance :: StorageRead -> StorageM ()
mkTTypeInstance storageRead = do
  fromTTypeInstance storageRead
  toTTypeInstance storageRead

mkTransformerCodeBody :: StorageM ()
mkTransformerCodeBody = do
  generateToTTypeFuncs
  generateFromTypeFuncs

generateDefaultCreateQuery :: StorageRead -> StorageM ()
generateDefaultCreateQuery storageRead = do
  tableDef <- ask
  let name = tableNameHaskell tableDef
  let withIdFields = getAllFieldsWithIdRelation (fields tableDef)
  let qname = domainTypeModulePrefix ++ name ++ "." ++ name
  onNewLine $
    tellM $
      "create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => " ++ qname ++ "-> m ()\n"
        ++ if null withIdFields then "create = createWithKV\n\n" else makeCreateWithIdFunction withIdFields
  where
    domainTypeModulePrefix = storageRead.domainTypeModulePrefix <> "."
    queriesModulePrefix = storageRead.queryModulePrefix <> "."
    makeCreateWithIdFunction :: [FieldDef] -> String
    makeCreateWithIdFunction withIdFields =
      "create tbl = do\n"
        ++ intercalate "\n" (map makeCreateWithIdFunctionLine withIdFields)
        ++ "\n  createWithKV tbl\n\n"

    makeCreateWithIdFunctionLine :: FieldDef -> String
    makeCreateWithIdFunctionLine field =
      case fromJust (relation field) of
        WithId True cache -> "  Kernel.Prelude.whenJust tbl." ++ fieldName field ++ " " ++ getModule cache ++ fromJust (snd <$> getFieldRelationAndHaskellType ((haskellType field) <> "|WithId")) ++ ".create"
        WithIdStrict True cache -> "  " ++ getModule cache ++ fromJust (snd <$> getFieldRelationAndHaskellType ((haskellType field) <> "|WithId")) ++ ".create tbl." ++ fieldName field
        _ -> ""
      where
        getModule isFromCached = bool queriesModulePrefix "Storage.CachedQueries." isFromCached -- Cached queries ?? -- TODO
    getAllFieldsWithIdRelation :: [FieldDef] -> [FieldDef]
    getAllFieldsWithIdRelation = filter (\f -> isJust (relation f) && isWithIdRelation (fromJust (relation f)))

generateDefaultCreateManyQuery :: StorageRead -> StorageM ()
generateDefaultCreateManyQuery storageRead = do
  name <- tableNameHaskell <$> ask
  let domainTypeModulePrefix = storageRead.domainTypeModulePrefix <> "."
  let qname = domainTypeModulePrefix ++ name ++ "." ++ name
  onNewLine $
    tellM $
      "createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => " ++ "[" ++ qname ++ "]" ++ "-> m ()\n"
        ++ "createMany = traverse_ create\n\n"

fromTTypeInstance :: StorageRead -> StorageM ()
fromTTypeInstance storageRead = do
  let domainTypeModulePrefix = storageRead.domainTypeModulePrefix <> "."
  tableDef <- ask
  onNewLine $
    tellM $
      "instance FromTType' Beam." ++ tableNameHaskell tableDef ++ " " ++ domainTypeModulePrefix ++ tableNameHaskell tableDef ++ "." ++ tableNameHaskell tableDef ++ " where\n"
        ++ "  fromTType' Beam."
        ++ tableNameHaskell tableDef
        ++ "T {..} = do\n"
        ++ (show $ generateCodeBody monadicFromTTypeTransformerCode tableDef)
        ++ "\n    "
        ++ intercalate "\n    " (filter (/= "") (map (\field -> fromTTypeMConversionFunction storageRead (tableNameHaskell tableDef) (haskellType field) (fieldName field) (relation field)) (fields tableDef)))
        ++ "\n"
        ++ "    pure $\n"
        ++ "      Just\n"
        ++ "        "
        ++ domainTypeModulePrefix
        ++ tableNameHaskell tableDef
        ++ "."
        ++ tableNameHaskell tableDef
        ++ "\n"
        ++ "          { "
        ++ intercalate ",\n           " (map fromField (fields tableDef))
        ++ "\n          }\n\n"
  where
    getFromTTypeParams :: FieldDef -> String
    getFromTTypeParams hfield = unwords $ map bFieldName (beamFields hfield)

    fromField field =
      if isEncrypted field
        then do
          let mapOperator = if isMaybeType (haskellType field) then " <$> " else " "
          let applicativeOperator = if isMaybeType (haskellType field) then " <*> " else " "
          fieldName field ++ " = EncryptedHashed" ++ mapOperator ++ "(Encrypted" ++ mapOperator ++ fieldName field ++ "Encrypted)" ++ applicativeOperator ++ fieldName field ++ "Hash"
        else fieldName field ++ " = " ++ fromTTypeConversionFunction (fromTType field) (haskellType field) (getFromTTypeParams field) (relation field) (fieldName field)

monadicFromTTypeTransformerCode :: StorageM ()
monadicFromTTypeTransformerCode = do
  tableDef <- ask
  tellM $
    map monadicTransformerLine (fields tableDef)
      & filter isJust
      & sequence
      & fromJust
      & intercalate "\n    "
      & ("    " ++)
  where
    getFromTTypeParams :: FieldDef -> String
    getFromTTypeParams hfield = unwords $ map bFieldName (beamFields hfield)

    monadicTransformerLine hfield =
      fromTType hfield >>= \tf ->
        case tfType tf of
          MonadicT -> Just $ fieldName hfield ++ "' <- " ++ tfName tf ++ " " ++ getFromTTypeParams hfield
          PureT -> Nothing

monadicToTTypeTransformerCode :: Maybe [String] -> Spaces -> StorageM ()
monadicToTTypeTransformerCode specificFields spaceCount = do
  tableDef <- ask
  let spaces' = replicate spaceCount ' '
  let fieldsToTransform =
        if isJust specificFields
          then filter (\f -> fieldName f `elem` fromJust specificFields) (fields tableDef)
          else fields tableDef
  tellM $
    if null fieldsToTransform
      then mempty
      else
        concatMap monadicTransformerLines fieldsToTransform
          & filter isJust
          & sequence
          & fromJust
          & intercalate ("\n" <> spaces')
          & (spaces' ++)
  where
    monadicTransformerLines hfield =
      map
        ( \field ->
            bToTType field >>= \tf ->
              case tfType tf of
                MonadicT -> Just $ bFieldName field ++ "' <- " ++ tfName tf ++$ toTTypeExtractor (makeExtractorFunction $ bfieldExtractor field) (fieldName hfield)
                PureT -> Nothing
        )
        (beamFields hfield)

toTTypeInstance :: StorageRead -> StorageM ()
toTTypeInstance storageRead = do
  let domainTypeModulePrefix = storageRead.domainTypeModulePrefix <> "."
  tableDef <- ask
  onNewLine $
    tellM $
      "instance ToTType' Beam." ++ tableNameHaskell tableDef ++ " " ++ domainTypeModulePrefix ++ tableNameHaskell tableDef ++ "." ++ tableNameHaskell tableDef ++ " where\n"
        ++ "  toTType' "
        ++ domainTypeModulePrefix
        ++ tableNameHaskell tableDef
        ++ "."
        ++ tableNameHaskell tableDef
        ++ " {..} = do\n"
        ++ (show $ generateCodeBody (monadicToTTypeTransformerCode Nothing 4) tableDef)
        ++ "\n    Beam."
        ++ tableNameHaskell tableDef
        ++ "T\n"
        ++ "      { "
        ++ intercalate ",\n        " (concatMap toField $ filter (removeBeamFieldsWRTRelation . relation) (fields tableDef))
        ++ "\n      }\n\n"
  where
    toField hfield =
      map
        ( \field ->
            if bIsEncrypted field
              then do
                let mapOperator = if isMaybeType (bFieldType field) then " <&> " else " & "
                let encryptedField = "Beam." ++ bFieldName field ++ "Encrypted = " ++ bFieldName field ++ mapOperator ++ "unEncrypted . (.encrypted)"
                let hashField = "Beam." ++ bFieldName field ++ "Hash = " ++ bFieldName field ++ mapOperator ++ "(.hash)"
                encryptedField ++ ",\n        " ++ hashField
              else "Beam." ++ bFieldName field ++ " = " ++ toTTypeConversionFunction (bToTType field) (haskellType hfield) (toTTypeExtractor (makeExtractorFunction $ bfieldExtractor field) (fieldName hfield)) (bFieldName field)
        )
        (beamFields hfield)

beamQueries :: StorageRead -> StorageM ()
beamQueries storageRead = do
  tableDef <- ask
  onNewLine $
    intercalateA newLine $
      map (generateBeamQuery storageRead tableDef.fields tableDef.tableNameHaskell) (queries tableDef ++ defaultQueryDefs tableDef)

toTTypeConversionFunction :: Maybe TransformerFunction -> String -> String -> String -> String
toTTypeConversionFunction transformer haskellType fieldName beamFieldName
  | isJust transformer = if tfType (fromJust transformer) == MonadicT then beamFieldName ++ "'" else tfName (fromJust transformer) ++$ fieldName
  | "Kernel.Types.Id.Id " `Text.isPrefixOf` Text.pack haskellType = "Kernel.Types.Id.getId " ++ fieldName
  | "Kernel.Types.Id.Id " `Text.isInfixOf` Text.pack haskellType = "Kernel.Types.Id.getId <$> " ++ fieldName
  | "Kernel.Types.Id.ShortId " `Text.isPrefixOf` Text.pack haskellType = "Kernel.Types.Id.getShortId " ++ fieldName
  | "Kernel.Types.Id.ShortId " `Text.isInfixOf` Text.pack haskellType = "Kernel.Types.Id.getShortId <$> " ++ fieldName
  | otherwise = fieldName

fromTTypeConversionFunction :: Maybe TransformerFunction -> String -> String -> Maybe FieldRelation -> String -> String
fromTTypeConversionFunction fromTTypeFunc haskellType fieldName relation dFieldName
  | isJust fromTTypeFunc = if tfType (fromJust fromTTypeFunc) == MonadicT then dFieldName ++ "'" else tfName (fromJust fromTTypeFunc) ++ " " ++ fieldName
  | isJust relation = if isWithIdRelation (fromJust relation) then dFieldName ++ "'" else fieldName ++ "'"
  | "Kernel.Types.Id.Id " `Text.isPrefixOf` Text.pack haskellType = "Kernel.Types.Id.Id " ++ fieldName
  | "Kernel.Types.Id.Id " `Text.isInfixOf` Text.pack haskellType = "Kernel.Types.Id.Id <$> " ++ fieldName
  | "Kernel.Types.Id.ShortId " `Text.isPrefixOf` Text.pack haskellType = "Kernel.Types.Id.ShortId " ++ fieldName
  | "Kernel.Types.Id.ShortId " `Text.isInfixOf` Text.pack haskellType = "Kernel.Types.Id.ShortId <$> " ++ fieldName
  | otherwise = fieldName

fromTTypeMConversionFunction :: StorageRead -> String -> String -> String -> Maybe FieldRelation -> String
fromTTypeMConversionFunction storageRead tableNameHaskell haskellType fieldName = \case
  Just relation -> case relation of
    OneToOne -> fieldName ++ "' <- " ++ queriesModulePrefix ++ fromJust (snd <$> getFieldRelationAndHaskellType haskellType) ++ ".findBy" ++ tableNameHaskell ++ "Id (Kernel.Types.Id.Id id) >>= fromMaybeM (InternalError \"Failed to get " ++ fieldName ++ ".\")"
    MaybeOneToOne -> fieldName ++ "' <- " ++ queriesModulePrefix ++ fromJust (snd <$> getFieldRelationAndHaskellType haskellType) ++ ".findBy" ++ tableNameHaskell ++ "Id (Kernel.Types.Id.Id id)"
    OneToMany -> fieldName ++ "' <- " ++ queriesModulePrefix ++ fromJust (snd <$> getFieldRelationAndHaskellType haskellType) ++ ".findAllBy" ++ tableNameHaskell ++ "Id (Kernel.Types.Id.Id id)"
    WithIdStrict _ isCached -> fieldName ++ "' <- " ++ getModule isCached ++ fromJust (snd <$> getFieldRelationAndHaskellType (haskellType <> "|WithId")) ++ ".findById (Kernel.Types.Id.Id " ++ fieldName ++ "Id)" ++ " >>= fromMaybeM (InternalError \"Failed to get " ++ fieldName ++ ".\")"
    WithId _ isCached -> fieldName ++ "' <- maybe (pure Nothing) (" ++ getModule isCached ++ fromJust (snd <$> getFieldRelationAndHaskellType (haskellType <> "|WithId")) ++ ".findById . Kernel.Types.Id.Id) " ++ fieldName ++ "Id"
  Nothing -> ""
  where
    queriesModulePrefix = storageRead.queryModulePrefix ++ "."
    getModule isFromCached = bool queriesModulePrefix "Storage.CachedQueries." isFromCached -- Chached query ?? --TODO

toTTypeExtractor :: Maybe String -> String -> String
toTTypeExtractor extractor field = maybe field (\x -> x ++ " " ++ field) extractor

generateBeamQuery :: StorageRead -> [FieldDef] -> String -> QueryDef -> StorageM ()
generateBeamQuery storageRead allHaskellFields tableNameHaskell query = do
  tableDef <- ask
  let paramFieldNames = nub $ map (fst . fst) (params query) <> (fst <$> getWhereClauseFieldNamesAndTypes (whereClause query))
  tellM $
    generateFunctionSignature
      storageRead
      query
      tableNameHaskell
      ++ (show $ generateCodeBody (monadicToTTypeTransformerCode (Just paramFieldNames) 3) tableDef)
      ++ generateBeamFunctionCall query.kvFunction
      ++ generateQueryParams allHaskellFields (query.params)
      ++ "    ["
      ++ genWhereClause
      ++ (if genWhereClause == "" then "" else "\n    ")
      ++ "]\n"
      ++ orderAndLimit query
  where
    genWhereClause = generateClause allHaskellFields query.takeFullObjectAsInput 6 0 query.whereClause

orderAndLimit :: QueryDef -> String
orderAndLimit query = do
  if query.kvFunction `elem` ["findAllWithOptionsKV", "findAllWithOptionsKV'", "findAllWithOptionsKVScheduler", "findAllWithOptionsDb"]
    then
      "    (Se." ++ show (snd query.orderBy) ++ " Beam." ++ (fst query.orderBy) ++ ")\n"
        ++ "    limit\n"
        ++ "    offset\n\n"
    else "\n"

ignoreEncryptionFlag :: ((String, String), Bool) -> (String, String)
ignoreEncryptionFlag ((field, tp), _) = (field, tp)

generateFunctionSignature :: StorageRead -> QueryDef -> String -> String
generateFunctionSignature storageRead query tableNameHaskell =
  let qparams = filter ((/= "updatedAt") . fst) $ map getIdsOut $ nub (map ignoreEncryptionFlag (params query) ++ addLimitParams query ++ getWhereClauseFieldNamesAndTypes (whereClause query))
      domainTypeModulePrefix = storageRead.domainTypeModulePrefix <> "."
   in query.queryName
        ++ " :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => "
        ++ bool (foldMap ((++ " -> ") . snd) qparams) (domainTypeModulePrefix ++ tableNameHaskell ++ "." ++ tableNameHaskell ++ " -> ") query.takeFullObjectAsInput
        ++ "m ("
        ++ generateQueryReturnType storageRead query.kvFunction tableNameHaskell
        ++ ")\n"
        ++ query.queryName
        ++ " "
        ++ bool (foldMap ((++ " ") . fst) qparams) (domainTypeModulePrefix ++ tableNameHaskell ++ "." ++ tableNameHaskell ++ " {..} ") query.takeFullObjectAsInput
        ++ "= do\n"

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

generateQueryReturnType :: StorageRead -> String -> String -> String
generateQueryReturnType storageRead kvFunction tableNameHaskell = do
  let domainTypeModulePrefix = storageRead.domainTypeModulePrefix <> "."
  if kvFunction `elem` ["findOneWithKV", "findOneWithKVScheduler", "findOneWithDb"]
    then "Maybe (" ++ domainTypeModulePrefix ++ tableNameHaskell ++ "." ++ tableNameHaskell ++ ")"
    else
      if kvFunction `elem` ["findAllWithKV", "findAllWithKVScheduler", "findAllWithOptionsKV", "findAllWithOptionsKV'", "findAllWithOptionsKVScheduler", "findAllWithDb", "findAllWithOptionsDb", "findAllWithKVAndConditionalDB"]
        then "[" ++ domainTypeModulePrefix ++ tableNameHaskell ++ "." ++ tableNameHaskell ++ "]"
        else ""

getWhereClauseFieldNamesAndTypes :: WhereClause -> [(String, String)]
getWhereClauseFieldNamesAndTypes EmptyWhere = []
getWhereClauseFieldNamesAndTypes (Leaf (field, _type, op)) = if op == Just In then [(field, "[" <> _type <> "]")] else [(field, _type)]
getWhereClauseFieldNamesAndTypes (Query (_, clauses)) = concatMap getWhereClauseFieldNamesAndTypes clauses

generateBeamFunctionCall :: String -> String
generateBeamFunctionCall kvFunction =
  (if "update" `isPrefixOf` kvFunction then "\n   " ++ "_now <- getCurrentTime" else "") ++ "\n   " ++ kvFunction ++ "\n"

generateQueryParams :: [FieldDef] -> [((String, String), Bool)] -> String
generateQueryParams _ [] = ""
generateQueryParams allFields params = "    [ " ++ intercalate ",\n      " (filter (/= "") $ map (generateQueryParam allFields) params) ++ "\n    ]\n"

generateQueryParam :: [FieldDef] -> ((String, String), Bool) -> String
generateQueryParam allFields ((field, tp), encrypted) =
  let fieldDef = fromMaybe (error "Param not found in data type") $ find (\f -> fieldName f == field) allFields
   in intercalate ",\n      " $
        filter (/= "") $
          map
            ( \bField ->
                if isJust fieldDef.relation
                  then case fromJust fieldDef.relation of
                    WithIdStrict _ _ -> "Se.Set Beam." ++ bFieldName bField ++$ correctSetField field tp bField
                    WithId _ _ -> "Se.Set Beam." ++ bFieldName bField ++$ correctSetField field tp bField
                    _ -> ""
                  else
                    if encrypted
                      then do
                        let mapOperator = if isMaybeType tp then " <&> " else " & "
                        let encryptedField = "Se.Set Beam." ++ field ++ "Encrypted $ " ++ field ++ mapOperator ++ "unEncrypted . (.encrypted)"
                        let hashField = "Se.Set Beam." ++ field ++ "Hash $ " ++ field ++ mapOperator ++ "(.hash)"
                        encryptedField ++ ",\n      " ++ hashField
                      else "Se.Set Beam." ++ bFieldName bField ++$ correctSetField field tp bField
            )
            fieldDef.beamFields

correctSetField :: String -> String -> BeamField -> String
correctSetField field tp beamField
  | "Kernel.Types.Id.Id " `isInfixOf` tp && not ("Kernel.Types.Id.Id " `isPrefixOf` tp) = "(Kernel.Types.Id.getId <$> " ++ field ++ ")"
  | "Kernel.Types.Id.ShortId " `isInfixOf` tp && not ("Kernel.Types.Id.ShortId " `isPrefixOf` tp) = "(Kernel.Types.Id.getShortId <$> " ++ field ++ ")"
  | "Kernel.Types.Id.Id " `isPrefixOf` tp = "(Kernel.Types.Id.getId " ++ field ++ ")"
  | "Kernel.Types.Id.ShortId " `isPrefixOf` tp = "(Kernel.Types.Id.getShortId " ++ field ++ ")"
  | field == "updatedAt" && isNothing (bToTType beamField) = "_now"
  | otherwise =
    fromMaybe (toTTypeExtractor (makeExtractorFunction $ bfieldExtractor beamField) field) $
      bToTType beamField >>= \tf ->
        pure $
          if tfType tf == MonadicT
            then bFieldName beamField ++ "'"
            else tfName tf ++$ toTTypeExtractor (makeExtractorFunction $ bfieldExtractor beamField) field

correctEqField :: String -> String -> BeamField -> String
correctEqField field tp beamField
  | "Kernel.Types.Id.Id " `isInfixOf` tp && not ("Kernel.Types.Id.Id " `isPrefixOf` tp) = "(Kernel.Types.Id.getId <$> " ++ field ++ ")"
  | "Kernel.Types.Id.ShortId " `isInfixOf` tp && not ("Kernel.Types.Id.ShortId " `isPrefixOf` tp) = "(Kernel.Types.Id.getShortId <$> " ++ field ++ ")"
  | otherwise =
    fromMaybe (toTTypeExtractor (makeExtractorFunction $ bfieldExtractor beamField) field) $
      bToTType beamField >>= \tf ->
        pure $
          if tfType tf == MonadicT
            then bFieldName beamField ++ "'"
            else tfName tf ++$ toTTypeExtractor (makeExtractorFunction $ bfieldExtractor beamField) field

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f = zipWith f [0 ..]

-- Function to process each clause
generateClause :: [FieldDef] -> Bool -> Int -> Int -> WhereClause -> String
generateClause _ _ _ _ EmptyWhere = ""
generateClause allFields isFullObjInp n i (Leaf (field, tp, op)) =
  let fieldDef = fromMaybe (error "Param not found in data type") $ find (\f -> fieldName f == field) allFields
   in intercalate " , " $ filter (/= "") $ map (\bfield -> (if i == 0 then " " else spaces n) ++ "Se.Is Beam." ++ bFieldName bfield ++$ operator (fromMaybe Eq op) ++$ (if isFullObjInp then correctSetField field tp bfield else correctEqField field tp bfield)) fieldDef.beamFields
generateClause allFields isFullObjInp n i (Query (op, clauses)) =
  (if i == 0 then " " else spaces n)
    ++ ( if op `elem` comparisonOperator
           then ""
           else
             operator op
               ++ "\n"
               ++ spaces (n + 2)
               ++ "["
       )
    ++ intercalate ",\n" (filter (/= "") $ mapWithIndex (generateClause allFields isFullObjInp (n + 4)) clauses)
    ++ if op `elem` comparisonOperator
      then ""
      else
        "\n"
          ++ spaces (n + 2)
          ++ "]"

generateToTTypeFuncs :: StorageM ()
generateToTTypeFuncs = do
  def <- ask
  let code = intercalate "\n" $ filter (not . null) $ map generateToTTypeFunc (fields def)
  case code of
    "" -> tellM mempty
    _ -> onNewLine $ tellM code
  where
    generateToTTypeFunc :: FieldDef -> String
    generateToTTypeFunc field =
      intercalate "\n" $
        filter (not . null) $
          map
            ( \bfield ->
                fromMaybe "" $
                  bToTType bfield >>= \tf ->
                    if '.' `elem` tfName tf
                      then Nothing
                      else pure $ case tfType tf of
                        PureT -> tfName tf ++ " :: " ++ hFieldType bfield ++ " -> " ++ bFieldType bfield ++ "\n" ++ tfName tf ++ " = error \"TODO\""
                        MonadicT -> tfName tf ++ " :: MonadFlow m => " ++ hFieldType bfield ++ " -> m ( " ++ bFieldType bfield ++ " )\n" ++ tfName tf ++ " = error \"TODO\""
            )
            (beamFields field)

generateFromTypeFuncs :: StorageM ()
generateFromTypeFuncs = do
  def <- ask
  let code = intercalate "\n" $ filter (not . null) $ map generateFromTTypeFunc (fields def)
  case code of
    "" -> tellM mempty
    _ -> onNewLine $ tellM code
  where
    generateFromTTypeFunc :: FieldDef -> String
    generateFromTTypeFunc field =
      let (params, types) = first (map ("_" ++)) $ unzip $ map (\bfield -> (bFieldName bfield, bFieldType bfield)) (beamFields field)
          funcType = " :: " ++ intercalate " -> " types ++ " -> " ++ haskellType field
          funcTypeM = " :: MonadFlow m => " ++ intercalate " -> " types ++ " -> m ( " ++ haskellType field ++ " )"
       in fromMaybe "" $
            fromTType field >>= \tf ->
              if '.' `elem` tfName tf
                then Nothing
                else pure $ case tfType tf of
                  PureT -> tfName tf ++ funcType ++ "\n" ++ tfName tf ++ " " ++ unwords params ++ " = error \"TODO\""
                  MonadicT -> tfName tf ++ funcTypeM ++ "\n" ++ tfName tf ++ " " ++ unwords params ++ " = error \"TODO\""

-- Helper to determine the operator
operator :: Operator -> String
operator = ("Se." <>) . show

spaces :: Int -> String
spaces n = replicate n ' '

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
