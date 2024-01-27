module NammaDSL.Generator.Haskell.BeamQueries (generateBeamQueries, BeamQueryCode (..), DefaultQueryCode (..), ExtraQueryCode (..)) where

import Control.Lens ((%~), (.~))
import Data.List (intercalate, isInfixOf, isPrefixOf, nub)
import qualified Data.Text as Text
import Kernel.Prelude
import NammaDSL.DSL.Syntax.Storage
import NammaDSL.Generator.Haskell.Common (checkForPackageOverrides)
import NammaDSL.GeneratorCore
import NammaDSL.Utils

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

generateBeamQueries :: TableDef -> BeamQueryCode
generateBeamQueries tableDef =
  if EXTRA_QUERY_FILE `elem` extaOperations tableDef
    then
      WithExtraQueryFile $
        ExtraQueryCode
          { defaultCode =
              DefaultQueryCode
                { readOnlyCode =
                    generateCode $
                      commonGeneratorInput
                        & moduleNm .~ readOnlyCodeModuleName ++ " (module " ++ readOnlyCodeModuleName ++ ", module ReExport)"
                        & codeBody .~ generateCodeBody mkCodeBody tableDef
                        & simpleImports %~ (++ ([readOnlyCodeModuleName ++ "Extra as ReExport"] ++ (if transformerCode' == mempty then [] else ["Storage.Queries.Transformers." ++ (capitalize $ tableNameHaskell tableDef)])))
                        & ghcOptions %~ (++ ["-Wno-dodgy-exports"]),
                  transformerCode =
                    if transformerCode' == mempty
                      then Nothing
                      else
                        Just $
                          generateCode $
                            commonGeneratorInput
                              & moduleNm .~ "Storage.Queries.Transformers." ++ (capitalize $ tableNameHaskell tableDef)
                              & codeBody .~ transformerCode'
                },
            instanceCode =
              generateCode $
                commonGeneratorInput
                  & moduleNm .~ "Storage.Queries.OrphanInstances." ++ (capitalize $ tableNameHaskell tableDef)
                  & codeBody .~ generateCodeBody mkTTypeInstance tableDef
                  & simpleImports %~ (++ (if transformerCode' == mempty then [] else ["Storage.Queries.Transformers." ++ (capitalize $ tableNameHaskell tableDef)])),
            extraQueryFile =
              generateCode $
                commonGeneratorInput
                  & moduleNm .~ readOnlyCodeModuleName ++ "Extra"
                  & codeBody .~ generateCodeBody extraFileCodeBody tableDef
                  & simpleImports %~ (++ ["Storage.Queries.OrphanInstances." ++ (capitalize $ tableNameHaskell tableDef)])
          }
    else
      DefaultQueryFile $
        DefaultQueryCode
          { readOnlyCode =
              generateCode $
                commonGeneratorInput
                  & moduleNm .~ readOnlyCodeModuleName
                  & codeBody .~ generateCodeBody mkCodeBody tableDef
                  & simpleImports %~ (++ (if transformerCode' == mempty then [] else ["Storage.Queries.Transformers." ++ (capitalize $ tableNameHaskell tableDef)]))
                  & ghcOptions %~ (++ ["-Wno-dodgy-exports"]),
            transformerCode =
              if transformerCode' == mempty
                then Nothing
                else
                  Just $
                    generateCode $
                      commonGeneratorInput
                        & moduleNm .~ "Storage.Queries.Transformers." ++ (capitalize $ tableNameHaskell tableDef)
                        & codeBody .~ transformerCode'
          }
  where
    transformerCode' :: Code
    transformerCode' = generateCodeBody mkTransformerCodeBody tableDef

    readOnlyCodeModuleName = "Storage.Queries." ++ (capitalize $ tableNameHaskell tableDef)

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
        <> concatMap getStorageRelationImports tableDef.fields
    allQualifiedImports :: [String]
    allQualifiedImports =
      [ "Domain.Types." ++ tableNameHaskell tableDef,
        "Storage.Beam." ++ (capitalize $ tableNameHaskell tableDef) ++ " as Beam",
        "Sequelize as Se"
      ]
        <> imports tableDef
    getStorageRelationImports :: FieldDef -> [String]
    getStorageRelationImports fieldDef =
      case getFieldRelationAndHaskellType fieldDef.haskellType of
        Just (_, query) -> ["Storage.Queries." ++ query]
        Nothing -> []

extraFileCodeBody :: StorageM ()
extraFileCodeBody = do
  onNewLine $ tellM "-- Extra code goes here -- "

mkCodeBody :: StorageM ()
mkCodeBody = do
  tableDef <- ask
  let isDefault = EXTRA_QUERY_FILE `notElem` extaOperations tableDef
  generateDefaultCreateQuery
  generateDefaultCreateManyQuery
  beamQueries
  when isDefault mkTTypeInstance

mkTTypeInstance :: StorageM ()
mkTTypeInstance = do
  fromTTypeInstance
  toTTypeInstance

mkTransformerCodeBody :: StorageM ()
mkTransformerCodeBody = do
  generateToTTypeFuncs
  generateFromTypeFuncs

generateDefaultCreateQuery :: StorageM ()
generateDefaultCreateQuery = do
  name <- tableNameHaskell <$> ask
  let qname = "Domain.Types." ++ name ++ "." ++ name
  onNewLine $
    tellM $
      "create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => " ++ qname ++ "-> m ()\n"
        ++ "create = createWithKV\n\n"

generateDefaultCreateManyQuery :: StorageM ()
generateDefaultCreateManyQuery = do
  name <- tableNameHaskell <$> ask
  let qname = "Domain.Types." ++ name ++ "." ++ name
  onNewLine $
    tellM $
      "createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => " ++ "[" ++ qname ++ "]" ++ "-> m ()\n"
        ++ "createMany = traverse_ createWithKV\n\n"

fromTTypeInstance :: StorageM ()
fromTTypeInstance = do
  tableDef <- ask
  onNewLine $
    tellM $
      "instance FromTType' Beam." ++ tableNameHaskell tableDef ++ " Domain.Types." ++ tableNameHaskell tableDef ++ "." ++ tableNameHaskell tableDef ++ " where\n"
        ++ "  fromTType' Beam."
        ++ tableNameHaskell tableDef
        ++ "T {..} = do\n"
        ++ "    "
        ++ intercalate ",\n         " (filter (/= "") (map (\field -> fromTTypeMConversionFunction (tableNameHaskell tableDef) (haskellType field) (fieldName field) (relation field)) (fields tableDef)))
        ++ "\n"
        ++ "    pure $\n"
        ++ "      Just\n"
        ++ "        "
        ++ "Domain.Types."
        ++ (tableNameHaskell tableDef)
        ++ "."
        ++ (tableNameHaskell tableDef)
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
        else fieldName field ++ " = " ++ fromTTypeConversionFunction (fromTType field) (haskellType field) (getFromTTypeParams field) (relation field)

toTTypeInstance :: StorageM ()
toTTypeInstance = do
  tableDef <- ask
  onNewLine $
    tellM $
      "instance ToTType' Beam." ++ tableNameHaskell tableDef ++ " Domain.Types." ++ tableNameHaskell tableDef ++ "." ++ tableNameHaskell tableDef ++ " where\n"
        ++ "  toTType' "
        ++ "Domain.Types."
        ++ (tableNameHaskell tableDef)
        ++ "."
        ++ (tableNameHaskell tableDef)
        ++ " {..} = do\n"
        ++ "    Beam."
        ++ tableNameHaskell tableDef
        ++ "T\n"
        ++ "      { "
        ++ intercalate ",\n        " (concatMap toField $ filter (isNothing . relation) (fields tableDef))
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
              else "Beam." ++ bFieldName field ++ " = " ++ toTTypeConversionFunction (bToTType field) (haskellType hfield) (toTTypeExtractor (makeExtractorFunction $ bfieldExtractor field) (fieldName hfield))
        )
        (beamFields hfield)

beamQueries :: StorageM ()
beamQueries = do
  tableDef <- ask
  onNewLine $
    intercalateA newLine $
      map (generateBeamQuery tableDef.fields tableDef.tableNameHaskell) (queries tableDef ++ defaultQueryDefs tableDef)

toTTypeConversionFunction :: Maybe String -> String -> String -> String
toTTypeConversionFunction transformer haskellType fieldName
  | isJust transformer = fromJust transformer ++ " ( " ++ fieldName ++ " )"
  | "Kernel.Types.Id.Id " `Text.isPrefixOf` Text.pack haskellType = "Kernel.Types.Id.getId " ++ fieldName
  | "Kernel.Types.Id.Id " `Text.isInfixOf` Text.pack haskellType = "Kernel.Types.Id.getId <$> " ++ fieldName
  | "Kernel.Types.Id.ShortId " `Text.isPrefixOf` Text.pack haskellType = "Kernel.Types.Id.getShortId " ++ fieldName
  | "Kernel.Types.Id.ShortId " `Text.isInfixOf` Text.pack haskellType = "Kernel.Types.Id.getShortId <$> " ++ fieldName
  | otherwise = fieldName

fromTTypeConversionFunction :: Maybe String -> String -> String -> Maybe FieldRelation -> String
fromTTypeConversionFunction fromTTypeFunc haskellType fieldName relation
  | isJust fromTTypeFunc = fromJust fromTTypeFunc ++ " " ++ fieldName
  | isJust relation = fieldName ++ "'"
  | "Kernel.Types.Id.Id " `Text.isPrefixOf` Text.pack haskellType = "Kernel.Types.Id.Id " ++ fieldName
  | "Kernel.Types.Id.Id " `Text.isInfixOf` Text.pack haskellType = "Kernel.Types.Id.Id <$> " ++ fieldName
  | "Kernel.Types.Id.ShortId " `Text.isPrefixOf` Text.pack haskellType = "Kernel.Types.Id.ShortId " ++ fieldName
  | "Kernel.Types.Id.ShortId " `Text.isInfixOf` Text.pack haskellType = "Kernel.Types.Id.ShortId <$> " ++ fieldName
  | otherwise = fieldName

fromTTypeMConversionFunction :: String -> String -> String -> Maybe FieldRelation -> String
fromTTypeMConversionFunction tableNameHaskell haskellType fieldName relation
  | isJust relation =
    case fromJust relation of
      OneToOne -> fieldName ++ "' <- Storage.Queries." ++ fromJust (snd <$> getFieldRelationAndHaskellType haskellType) ++ ".findBy" ++ tableNameHaskell ++ "Id (Kernel.Types.Id.Id id) >>= fromMaybeM (InternalError \"Failed to get " ++ fieldName ++ ".\")"
      MaybeOneToOne -> fieldName ++ "' <- Storage.Queries." ++ fromJust (snd <$> getFieldRelationAndHaskellType haskellType) ++ ".findBy" ++ tableNameHaskell ++ "Id (Kernel.Types.Id.Id id)"
      OneToMany -> fieldName ++ "' <- Storage.Queries." ++ fromJust (snd <$> getFieldRelationAndHaskellType haskellType) ++ ".findAllBy" ++ tableNameHaskell ++ "Id (Kernel.Types.Id.Id id)"
  | otherwise = ""

toTTypeExtractor :: Maybe String -> String -> String
toTTypeExtractor extractor field
  | isJust extractor = fromJust extractor ++ " (" ++ field ++ " )"
  | otherwise = field

generateBeamQuery :: [FieldDef] -> String -> QueryDef -> StorageM ()
generateBeamQuery allHaskellFields tableNameHaskell query =
  tellM $
    generateFunctionSignature
      query
      tableNameHaskell
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

generateFunctionSignature :: QueryDef -> String -> String
generateFunctionSignature query tableNameHaskell =
  let qparams = filter ((/= "updatedAt") . fst) $ map getIdsOut $ nub (map ignoreEncryptionFlag (params query) ++ addLimitParams query ++ getWhereClauseFieldNamesAndTypes (whereClause query))
   in query.queryName
        ++ " :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => "
        ++ bool (foldMap ((++ " -> ") . snd) qparams) ("Domain.Types." ++ tableNameHaskell ++ "." ++ tableNameHaskell ++ " -> ") query.takeFullObjectAsInput
        ++ "m ("
        ++ generateQueryReturnType query.kvFunction tableNameHaskell
        ++ ")\n"
        ++ query.queryName
        ++ " "
        ++ bool (foldMap ((++ " ") . fst) qparams) ("Domain.Types." ++ tableNameHaskell ++ "." ++ tableNameHaskell ++ " {..} ") query.takeFullObjectAsInput
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

generateQueryReturnType :: String -> String -> String
generateQueryReturnType kvFunction tableNameHaskell = do
  if kvFunction `elem` ["findOneWithKV", "findOneWithKVScheduler", "findOneWithDb"]
    then "Maybe (Domain.Types." ++ tableNameHaskell ++ "." ++ tableNameHaskell ++ ")"
    else
      if kvFunction `elem` ["findAllWithKV", "findAllWithKVScheduler", "findAllWithOptionsKV", "findAllWithOptionsKV'", "findAllWithOptionsKVScheduler", "findAllWithDb", "findAllWithOptionsDb", "findAllWithKVAndConditionalDB"]
        then "[" ++ "Domain.Types." ++ tableNameHaskell ++ "." ++ tableNameHaskell ++ "]"
        else ""

getWhereClauseFieldNamesAndTypes :: WhereClause -> [(String, String)]
getWhereClauseFieldNamesAndTypes EmptyWhere = []
getWhereClauseFieldNamesAndTypes (Leaf (field, _type, op)) = if op == Just In then [(field, "[" <> _type <> "]")] else [(field, _type)]
getWhereClauseFieldNamesAndTypes (Query (_, clauses)) = concatMap getWhereClauseFieldNamesAndTypes clauses

generateBeamFunctionCall :: String -> String
generateBeamFunctionCall kvFunction =
  (if "update" `isPrefixOf` kvFunction then "   " ++ "now <- getCurrentTime\n" else "") ++ "   " ++ kvFunction ++ "\n"

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
                  then ""
                  else
                    if encrypted
                      then do
                        let mapOperator = if isMaybeType tp then " <&> " else " & "
                        let encryptedField = "Se.Set Beam." ++ field ++ "Encrypted $ " ++ field ++ mapOperator ++ "unEncrypted . (.encrypted)"
                        let hashField = "Se.Set Beam." ++ field ++ "Hash $ " ++ field ++ mapOperator ++ "(.hash)"
                        encryptedField ++ ",\n      " ++ hashField
                      else "Se.Set Beam." ++ bFieldName bField ++ " $ " ++ correctSetField field tp bField
            )
            fieldDef.beamFields

correctSetField :: String -> String -> BeamField -> String
correctSetField field tp beamField
  | "Kernel.Types.Id.Id " `isInfixOf` tp && not ("Kernel.Types.Id.Id " `isPrefixOf` tp) = "(Kernel.Types.Id.getId <$> " ++ field ++ ")"
  | "Kernel.Types.Id.ShortId " `isInfixOf` tp && not ("Kernel.Types.Id.ShortId " `isPrefixOf` tp) = "(Kernel.Types.Id.getShortId <$> " ++ field ++ ")"
  | "Kernel.Types.Id.Id " `isPrefixOf` tp = "(Kernel.Types.Id.getId " ++ field ++ ")"
  | "Kernel.Types.Id.ShortId " `isPrefixOf` tp = "(Kernel.Types.Id.getShortId " ++ field ++ ")"
  | field == "updatedAt" = "now"
  | otherwise = maybe "" (++ " $ ") (bToTType beamField) ++ toTTypeExtractor (makeExtractorFunction $ bfieldExtractor beamField) field

correctEqField :: String -> String -> BeamField -> String
correctEqField field tp beamField
  | "Kernel.Types.Id.Id " `isInfixOf` tp && not ("Kernel.Types.Id.Id " `isPrefixOf` tp) = "(Kernel.Types.Id.getId <$> " ++ field ++ ")"
  | "Kernel.Types.Id.ShortId " `isInfixOf` tp && not ("Kernel.Types.Id.ShortId " `isPrefixOf` tp) = "(Kernel.Types.Id.getShortId <$> " ++ field ++ ")"
  | otherwise = maybe "" (++ " $ ") (bToTType beamField) ++ toTTypeExtractor (makeExtractorFunction $ bfieldExtractor beamField) field

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f = zipWith f [0 ..]

-- Function to process each clause
generateClause :: [FieldDef] -> Bool -> Int -> Int -> WhereClause -> String
generateClause _ _ _ _ EmptyWhere = ""
generateClause allFields isFullObjInp n i (Leaf (field, tp, op)) =
  let fieldDef = fromMaybe (error "Param not found in data type") $ find (\f -> fieldName f == field) allFields
   in intercalate " , " $ filter (/= "") $ map (\bfield -> (if i == 0 then " " else spaces n) ++ "Se.Is Beam." ++ bFieldName bfield ++ " $ " ++ operator (fromMaybe Eq op) ++ " " ++ (if isFullObjInp then correctSetField field tp bfield else correctEqField field tp bfield)) fieldDef.beamFields
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
                case bToTType bfield of
                  Just func -> func ++ " :: " ++ hFieldType bfield ++ " -> " ++ bFieldType bfield ++ "\n" ++ func ++ " = error \"TODO\""
                  Nothing -> ""
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
       in case fromTType field of
            Just func -> func ++ funcType ++ "\n" ++ func ++ " " ++ unwords params ++ " = error \"TODO\""
            Nothing -> ""

-- Helper to determine the operator
operator :: Operator -> String
operator And = "Se.And"
operator Or = "Se.Or"
operator In = "Se.In"
operator Eq = "Se.Eq"
operator LessThan = "Se.LessThan"
operator LessThanOrEq = "Se.LessThanOrEq"
operator GreaterThan = "Se.GreaterThan"
operator GreaterThanOrEq = "Se.GreaterThanOrEq"

spaces :: Int -> String
spaces n = replicate n ' '

defaultQueryDefs :: TableDef -> [QueryDef]
defaultQueryDefs tableDef =
  [ QueryDef "findByPrimaryKey" "findOneWithKV" [] findByPrimayKeyWhereClause defaultOrderBy False,
    QueryDef "updateByPrimaryKey" "updateWithKV" (getAllFieldNamesWithTypesExcludingPks (primaryKey tableDef) (fields tableDef)) findByPrimayKeyWhereClause defaultOrderBy True
  ]
  where
    getAllFieldNamesWithTypesExcludingPks :: [String] -> [FieldDef] -> [((String, String), Bool)]
    getAllFieldNamesWithTypesExcludingPks pks fieldDefs = map (\fieldDef -> ((fieldName fieldDef, haskellType fieldDef), isEncrypted fieldDef)) $ filter (\fieldDef -> fieldName fieldDef `notElem` pks) fieldDefs

    getAllPrimaryKeyWithTypes :: [String] -> [FieldDef] -> [(String, String, Maybe Operator)]
    getAllPrimaryKeyWithTypes pks fieldDefs = map (\fieldDef -> (fieldName fieldDef, haskellType fieldDef, Nothing)) $ filter (\fieldDef -> fieldName fieldDef `elem` pks) fieldDefs

    primaryKeysAndTypes :: [(String, String, Maybe Operator)]
    primaryKeysAndTypes = getAllPrimaryKeyWithTypes (primaryKey tableDef) (fields tableDef)

    findByPrimayKeyWhereClause :: WhereClause
    findByPrimayKeyWhereClause = Query (And, Leaf <$> primaryKeysAndTypes)

makeExtractorFunction :: [String] -> Maybe String
makeExtractorFunction funcs = if null funcs then Nothing else Just $ "( " ++ intercalate " . " funcs ++ " )"
