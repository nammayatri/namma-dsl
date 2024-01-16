{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module NammaDSL.Generator.Haskell.BeamQueries (generateBeamQueries) where

import Data.List (intercalate, isInfixOf, isPrefixOf, nub)
import Data.String.Interpolate (i, __i)
import qualified Data.Text as Text
import Kernel.Prelude
import NammaDSL.DSL.Syntax.Storage
import NammaDSL.GeneratorCore
import NammaDSL.Utils

generateBeamQueries :: TableDef -> Code
generateBeamQueries tableDef = do
  generateCode generatorInput
  where
    generatorInput :: GeneratorInput
    generatorInput =
      GeneratorInput
        { _ghcOptions = ["-Wno-orphans", "-Wno-unused-imports"],
          _extensions = [],
          _moduleNm = "Storage.Queries." ++ (capitalize $ tableNameHaskell tableDef),
          _simpleImports = allSimpleImports,
          _qualifiedImports = allQualifiedImports,
          _codeBody = generateCodeBody mkCodeBody tableDef
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
      [ "Storage.Beam." ++ (capitalize $ tableNameHaskell tableDef) ++ " as Beam",
        "Sequelize as Se"
      ]
        <> imports tableDef
    getStorageRelationImports :: FieldDef -> [String]
    getStorageRelationImports fieldDef =
      case getFieldRelationAndHaskellType fieldDef.haskellType of
        Just (_, query) -> ["Storage.Queries." ++ query]
        Nothing -> []

mkCodeBody :: StorageM ()
mkCodeBody = do
  generateDefaultCreateQuery
  generateDefaultCreateManyQuery
  beamQueries
  fromTTypeInstance
  toTTypeInstance
  generateToTTypeFuncs
  generateFromTypeFuncs

generateDefaultCreateQuery :: StorageM ()
generateDefaultCreateQuery = do
  name <- tableNameHaskell <$> ask
  let qname = "Domain.Types." ++ name ++ "." ++ name
  onNewLine $
    tellM $
      [__i|
        create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => #{qname} -> m ()
        create = createWithKV
      |]

generateDefaultCreateManyQuery :: StorageM ()
generateDefaultCreateManyQuery = do
  name <- tableNameHaskell <$> ask
  let qname = "Domain.Types." ++ name ++ "." ++ name
  onNewLine $
    tellM $
      [__i|
        createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [#{qname}] -> m ()
        createMany = traverse_ createWithKV
      |]

fromTTypeInstance :: StorageM ()
fromTTypeInstance = do
  tableDef <- ask
  onNewLine $
    tellM $
      [__i|
        instance FromTType' Beam.#{tableNameHaskell tableDef} Domain.Types.#{tableNameHaskell tableDef}.#{tableNameHaskell tableDef} where
          fromTType' Beam.#{tableNameHaskell tableDef}T {..} = do
            #{intercalate ",\n         " (filter (/= "") (map (\field -> fromTTypeMConversionFunction (tableNameHaskell tableDef) (haskellType field) (fieldName field) (relation field)) (fields tableDef)))}
            pure $
              Just
                Domain.Types.#{tableNameHaskell tableDef}.#{tableNameHaskell tableDef}
                  { #{intercalate ",\n            " (map fromField (fields tableDef))}
                  }
      |]
  where
    getFromTTypeParams :: FieldDef -> String
    getFromTTypeParams hfield = unwords $ map bFieldName (beamFields hfield)

    fromField :: FieldDef -> String
    fromField field =
      [__i|
        #{fieldName field} = #{if isEncrypted field then encryptedConversion else nonEncryptedConversion}
      |]
      where
        encryptedConversion :: String
        encryptedConversion =
          if isMaybeType (haskellType field)
            then [__i|EncryptedHashed <$> (Encrypted <$> #{fieldName field}Encrypted) <*> #{fieldName field}Hash|]
            else [__i|EncryptedHashed (Encrypted #{fieldName field}Encrypted) #{fieldName field}Hash|]

        nonEncryptedConversion :: String
        nonEncryptedConversion =
          fromTTypeConversionFunction (fromTType field) (haskellType field) (getFromTTypeParams field) (relation field)

toTTypeInstance :: StorageM ()
toTTypeInstance = do
  tableDef <- ask
  onNewLine $
    tellM $
      [__i|
        instance ToTType' Beam.#{tableNameHaskell tableDef} Domain.Types.#{tableNameHaskell tableDef}.#{tableNameHaskell tableDef} where
          toTType' Domain.Types.#{tableNameHaskell tableDef}.#{tableNameHaskell tableDef} {..} = do
            Beam.#{tableNameHaskell tableDef}T
              { #{intercalate ",\n        " (concatMap toField $ filter (isNothing . relation)  (fields tableDef))}
              }
      |]
  where
    toField hfield =
      map
        ( \field ->
            if bIsEncrypted field
              then
                [__i|
              Beam.#{bFieldName field}Encrypted = Beam.#{bFieldName field} <&> unEncrypted . (.encrypted),
              Beam.#{bFieldName field}Hash = Beam.#{bFieldName field} <&> (.hash)
            |]
              else
                [__i|
              Beam.#{bFieldName field} = #{toTTypeConversionFunction (bToTType field) (haskellType hfield) (toTTypeExtractor (makeExtractorFunction $ bfieldExtractor field) (fieldName hfield))}
            |]
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
    [__i|
      #{genFunctionSignature}
      #{genBeamFunctionCall}#{genQueryParams}
          [
            #{genWhereClause}
          ]
      #{genOrderAndLimit}
    |]
  where
    genFunctionSignature = generateFunctionSignature query tableNameHaskell
    genBeamFunctionCall = generateBeamFunctionCall query.kvFunction
    genQueryParams = generateQueryParams allHaskellFields query.params
    genWhereClause = generateWhereClause allHaskellFields query.takeFullObjectAsInput 6 0 query.whereClause
    genOrderAndLimit = generateOrderAndLimit query

generateOrderAndLimit :: QueryDef -> String
generateOrderAndLimit query =
  let findAllFunctions =
        [ "findAllWithOptionsKV",
          "findAllWithOptionsKV'",
          "findAllWithOptionsKVScheduler",
          "findAllWithOptionsDb"
        ]
   in if query.kvFunction `elem` findAllFunctions
        then [i|    (Se.Desc Beam.createdAt)\n    limit\n    offset|]
        else "\n"

ignoreEncryptionFlag :: ((String, String), Bool) -> (String, String)
ignoreEncryptionFlag ((field, tp), _) = (field, tp)

generateFunctionSignature :: QueryDef -> String -> String
generateFunctionSignature query tableNameHaskell =
  [__i|
    #{queryName query} :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => #{paramTypes} m (#{genQueryReturnType})
    #{queryName query} #{paramNames} = do
  |]
  where
    genQueryReturnType = generateQueryReturnType query.kvFunction tableNameHaskell
    qparams = filter ((/= "updatedAt") . fst) $ map getIdsOut $ nub (map ignoreEncryptionFlag (params query) ++ addLimitParams query ++ getWhereClauseFieldNamesAndTypes (whereClause query))
    paramTypes = bool (foldMap ((++ " -> ") . snd) qparams) ("Domain.Types." ++ tableNameHaskell ++ "." ++ tableNameHaskell ++ " ->") query.takeFullObjectAsInput
    paramNames = bool (foldMap ((++ " ") . fst) qparams) ("Domain.Types." ++ tableNameHaskell ++ "." ++ tableNameHaskell ++ " {..}") query.takeFullObjectAsInput

addLimitParams :: QueryDef -> [(String, String)]
addLimitParams query =
  let findAllFunctions =
        [ "findAllWithOptionsKV",
          "findAllWithOptionsKV'",
          "findAllWithOptionsKVScheduler",
          "findAllWithOptionsDb"
        ]
   in if query.kvFunction `elem` findAllFunctions
        then [("limit", "Maybe Int"), ("offset", "Maybe Int")]
        else []

getIdsOut :: (String, String) -> (String, String)
getIdsOut (k, t)
  | "Kernel.Types.Id.Id " `isPrefixOf` t = ("(Kernel.Types.Id.Id " ++ k ++ ")", t)
  | "Kernel.Types.Id.ShortId " `isPrefixOf` t = ("(Kernel.Types.Id.ShortId " ++ k ++ ")", t)
  | otherwise = (k, t)

generateQueryReturnType :: String -> String -> String
generateQueryReturnType kvFunction tableNameHaskell =
  case kvFunction of
    _ | kvFunction `elem` findOneFunctions -> "Maybe (Domain.Types." ++ tableNameHaskell ++ "." ++ tableNameHaskell ++ ")"
    _ | kvFunction `elem` findAllFunctions -> "[" ++ "Domain.Types." ++ tableNameHaskell ++ "." ++ tableNameHaskell ++ "]"
    _ -> ""
  where
    findOneFunctions = ["findOneWithKV", "findOneWithKVScheduler", "findOneWithDb"]
    findAllFunctions =
      [ "findAllWithKV",
        "findAllWithKVScheduler",
        "findAllWithOptionsKV",
        "findAllWithOptionsKV'",
        "findAllWithOptionsKVScheduler",
        "findAllWithDb",
        "findAllWithOptionsDb",
        "findAllWithKVAndConditionalDB"
      ]

getWhereClauseFieldNamesAndTypes :: WhereClause -> [(String, String)]
getWhereClauseFieldNamesAndTypes EmptyWhere = []
getWhereClauseFieldNamesAndTypes (Leaf (field, _type, op)) = if op == Just In then [(field, "[" <> _type <> "]")] else [(field, _type)]
getWhereClauseFieldNamesAndTypes (Query (_, clauses)) = concatMap getWhereClauseFieldNamesAndTypes clauses

generateBeamFunctionCall :: String -> String
generateBeamFunctionCall kvFunction =
  (if "update" `isPrefixOf` kvFunction then "  " ++ "now <- getCurrentTime\n" else "") ++ "  " ++ kvFunction ++ "\n"

generateQueryParams :: [FieldDef] -> [((String, String), Bool)] -> String
generateQueryParams _ [] = ""
generateQueryParams allFields params =
  [i|    [ #{intercalate ",\n      " (filter (/= "") $ map (generateQueryParam allFields) params)}
    ]|]

generateQueryParam :: [FieldDef] -> ((String, String), Bool) -> String
generateQueryParam allFields ((field, tp), encrypted) =
  let fieldDef = fromMaybe (error "Param not found in data type") $ find (\f -> fieldName f == field) allFields
   in intercalate ",\n      " $ map (generateParamField fieldDef field tp encrypted) fieldDef.beamFields

generateParamField :: FieldDef -> String -> String -> Bool -> BeamField -> String
generateParamField fieldDef field tp encrypted bField
  | isJust fieldDef.relation = ""
  | encrypted =
    let mapOperator :: String
        mapOperator = if isMaybeType tp then " <&> " else " & "
     in [__i|
          Se.Set Beam.#{field}Encrypted $ #{field}#{mapOperator}unEncrypted . (.encrypted),
          Se.Set Beam.#{field}Hash $ #{field}#{mapOperator}(.hash)
        |]
  | otherwise =
    [__i|Se.Set Beam.#{bFieldName bField} $ #{correctSetField field tp bField}|]

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

generateWhereClause :: [FieldDef] -> Bool -> Int -> Int -> WhereClause -> String
generateWhereClause _ _ _ _ EmptyWhere = ""
generateWhereClause allFields isFullObjInp n v (Leaf (field, tp, op)) =
  let fieldDef = fromMaybe (error "Param not found in data type") $ find (\f -> fieldName f == field) allFields
   in intercalate " , " $ filter (/= "") $ map (\bfield -> generateLeafClause field tp bfield op isFullObjInp n v) fieldDef.beamFields
generateWhereClause allFields isFullObjInp n v (Query (op, clauses)) =
  (if v == 0 then " " else spaces n)
    ++ (if op `elem` comparisonOperator then "" else operator op ++ "\n" ++ spaces (n + 2) ++ "[")
    ++ intercalate ",\n" (filter (/= "") $ mapWithIndex (generateWhereClause allFields isFullObjInp (n + 4)) clauses)
    ++ (if op `elem` comparisonOperator then "" else "\n" ++ spaces (n + 2) ++ "]")

generateLeafClause :: String -> String -> BeamField -> Maybe Operator -> Bool -> Int -> Int -> String
generateLeafClause field tp bfield op isFullObjInp n v =
  [i|#{if v == 0 then " " else spaces n}
    Se.Is Beam.#{bFieldName bfield} $ #{operator (fromMaybe Eq op)} #{if isFullObjInp then correctSetField field tp bfield else correctEqField field tp bfield}|]

generateToTTypeFuncs :: StorageM ()
generateToTTypeFuncs = do
  def <- ask
  onNewLine $ tellM $ intercalate "\n" $ map generateToTTypeFunc (fields def)
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
  onNewLine $ tellM $ intercalate "\n" $ filter (not . null) $ map generateFromTTypeFunc (fields def)
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
operator op = case op of
  And -> "Se.And"
  Or -> "Se.Or"
  In -> "Se.In"
  Eq -> "Se.Eq"
  LessThan -> "Se.LessThan"
  LessThanOrEq -> "Se.LessThanOrEq"
  GreaterThan -> "Se.GreaterThan"
  GreaterThanOrEq -> "Se.GreaterThanOrEq"

spaces :: Int -> String
spaces n = replicate n ' '

defaultQueryDefs :: TableDef -> [QueryDef]
defaultQueryDefs tableDef =
  [ QueryDef "findByPrimaryKey" "findOneWithKV" [] findByPrimayKeyWhereClause False,
    QueryDef "updateByPrimaryKey" "updateWithKV" (getAllFieldNamesWithTypesExcludingPks (primaryKey tableDef) (fields tableDef)) findByPrimayKeyWhereClause True
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
