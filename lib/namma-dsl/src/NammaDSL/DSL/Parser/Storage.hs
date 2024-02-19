{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module NammaDSL.DSL.Parser.Storage (storageParser, getOldSqlFile, debugParser, runAnyParser) where

import Control.Lens.Combinators
import Control.Lens.Operators
import Data.Aeson
import Data.Aeson.Key (fromString, toString)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Lens (key, _Array, _Object, _Value)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import Data.Char (toUpper)
import qualified Data.List as L
import qualified Data.List.Extra as L
import Data.List.Split (split, splitOn, splitWhen, whenElt)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Tuple (swap)
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml
import qualified Debug.Trace as DT
import FlatParse.Basic
import Kernel.Prelude hiding (fromString, toString, toText, traceShowId, try)
import NammaDSL.DSL.Syntax.Common
import NammaDSL.DSL.Syntax.Storage
import NammaDSL.Utils hiding (typeDelimiter)
import System.Directory (doesFileExist)
import Text.Casing (quietSnake)
import Text.Regex.TDFA ((=~))

debugParser :: String -> Parser e ()
debugParser tag = do
  !_ <- DT.traceShowId . ((<>) tag) <$> lookahead (many anyChar)
  pure ()

snakeCaseToCamelCase :: String -> String
snakeCaseToCamelCase = concat . makeMeCamel . map (T.unpack . T.strip) . T.split (== '_') . T.pack
  where
    makeMeCamel (x : xs) = x : map captialise xs
    makeMeCamel [] = error "empty field name"

    captialise (x : xs) = toUpper x : xs
    captialise [] = error "got two underscores together in field name, feeling sad...."

parseAlterTablePrefix :: String -> Parser e ()
parseAlterTablePrefix dbName =
  case dbName of
    "atlas_app" -> $(string $ "ALTER TABLE atlas_app.")
    "atlas_driver_offer_bpp" -> $(string $ "ALTER TABLE atlas_driver_offer_bpp.")
    _ -> error "I need it, please add this in sqlCreateParser function as well"

sqlAlterAddPrimaryKeyParser :: String -> Parser e ([String], [String])
sqlAlterAddPrimaryKeyParser dbName = do
  parseAlterTablePrefix dbName
  _tableName <- many $ notFollowedBy anyChar $(string "ADD PRIMARY KEY (")
  $(string " ADD PRIMARY KEY ( ")
  keys <- many $ notFollowedBy anyChar $(string ";")
  $(string ");\n") <|> $(string ");")
  let snakeKeys = map (snakeCaseToCamelCase . T.unpack) (T.split (== ',') . T.pack $ keys)
  case snakeKeys of
    (x : xs) -> pure ([x], xs)
    [] -> pure ([], [])

parseWithDefault :: Parser e (Maybe String)
parseWithDefault = do
  $(string " default ")
  defaultValStart <- many $ notFollowedBy anyChar $(string ";")
  defaultValEnd <- anyChar
  return . Just $ defaultValStart <> [defaultValEnd]

parseConstraint :: Parser e (Maybe FieldConstraint)
parseConstraint = do
  constarint <-
    $( switch
         [|
           case _ of
             " NOT NULL" -> pure (Just NotNull)
             _ -> pure Nothing
           |]
     )
  return constarint

sqlAlterTableAddColumn :: String -> Bool -> Parser e FieldDef
sqlAlterTableAddColumn dbName _fromUpdatesSection = do
  parseAlterTablePrefix dbName
  _tableName <- many $ notFollowedBy anyChar ($(string "ADD COLUMN") <|> $(string "ADD PRIMARY KEY"))
  $(string " ADD COLUMN ")
  fieldNameStart <- many $ notFollowedBy anyChar $(string " ")
  fieldNameEnd <- anyChar
  let fieldName = fieldNameStart <> [fieldNameEnd]
      isEncrypted = fieldName =~ ("_hash" :: String)
  $(string " ")
  sqlType <- many $ notFollowedBy anyChar ($(string "NOT NULL") <|> $(string ";") <|> $(string "default")) -- TODO: update it when we add more constraints in generator.
  constraint <- parseConstraint <|> return Nothing
  $(string " ;") <|> pure ()
  defaultVal <- parseWithDefault <|> return Nothing
  $(string ";\n") <|> $(string "\n") <|> $(string ";")
  when isEncrypted $ do
    _ignoreNextLine <- many $ notFollowedBy anyChar $(string "\n")
    $(string ";\n")
  if _fromUpdatesSection then debugParser "" else pure ()
  let fromTType = Nothing
      beamFields = [bf]
      bf =
        BeamField
          { bFieldName = snakeCaseToCamelCase fieldName,
            hFieldType = findMatchingHaskellType sqlType, -- not required, but anyways did.
            bFieldType = sqlType, -- not required for this case
            bConstraints = [PrimaryKey | fieldName == "id"] <> maybeToList constraint, -- as hardcoded in the generator part
            bSqlType = sqlType, -- not required for this case
            bFieldUpdates = [],
            bDefaultVal = defaultVal,
            bfieldExtractor = [],
            bToTType = Nothing,
            bIsEncrypted = False
          }
  pure $
    FieldDef
      (snakeCaseToCamelCase fieldName)
      (findMatchingHaskellType sqlType)
      beamFields
      fromTType
      isEncrypted
      Nothing
      Nothing

sqlCreateParser :: String -> Parser e String
sqlCreateParser dd = do
  case dd of
    "atlas_app" -> $(string $ "CREATE TABLE atlas_app.") -- need to fix the generator as well to take tablename as a argument.
    "atlas_driver_offer_bpp" -> $(string $ "CREATE TABLE atlas_driver_offer_bpp.") -- need to fix the generator as well to take tablename as a argument.
    _ -> error "I need it, do this in parseAlterTablePrefix function as well" -- need to fix the generator as well to take tablename as a argument.
  tableName <- many $ notFollowedBy anyChar $(char '(')
  $(string " ();\n")
  pure tableName

updateStamp :: String
updateStamp = "------- SQL updates -------"

sqlUpdateStampParser :: Parser e ()
sqlUpdateStampParser = $(string "------- SQL updates -------\n")

columnUpdateActionParser :: Parser e SqlFieldUpdates
columnUpdateActionParser = do
  updateAction <-
    ($(string " DROP DEFAULT") *> pure DropDefault)
      <|> ($(string " SET DEFAULT ") *> pure (AddDefault ""))
      <|> ($(string " DROP NOT NULL") *> pure DropNotNull)
      <|> ($(string " SET NOT NULL") *> pure AddNotNull)
  finalAction <-
    case updateAction of
      AddDefault _ -> do
        defStart <- many $ notFollowedBy anyChar ($(string ";\n") <|> $(string ";"))
        defEnd <- anyChar
        pure . AddDefault $ defStart <> [defEnd]
      val -> pure val
  $(string ";\n") <|> $(string ";")
  return finalAction

sqlUpdatesParser :: String -> Parser e SqlUpdates
sqlUpdatesParser dbName = do
  parseAlterTablePrefix dbName
  _tableNameStart <- many (notFollowedBy anyChar $(string " "))
  _tableNameEndChar <- anyChar
  sqlAction :: String <-
    ($(string " ALTER COLUMN ") *> pure "ALTER COLUMN")
      <|> ($(string " DROP CONSTRAINT ") *> pure "DROP CONSTRAINT")
      <|> ($(string " DROP COLUMN ") *> pure "DROP COLUMN")
  case sqlAction of
    "DROP COLUMN" -> do
      fieldNameStart <- many $ notFollowedBy anyChar ($(string ";\n") <|> $(string ";"))
      fieldNameEnd <- anyChar
      $(string ";\n") <|> $(string ";")
      pure $
        SqlUpdates
          (Just (snakeCaseToCamelCase (fieldNameStart <> [fieldNameEnd]), DropColumn))
          []
    "ALTER COLUMN" -> do
      fieldName <- many (notFollowedBy anyChar ($(string "DROP") <|> $(string "SET") <|> $(string "TYPE")))
      update <- columnUpdateActionParser
      pure $
        SqlUpdates
          (Just (snakeCaseToCamelCase fieldName, update))
          []
    "DROP CONSTRAINT" -> do
      _ <- many $ notFollowedBy anyChar $(string "pkey;\n")
      $(string "_pkey;\n") <|> $(string "_pkey;")
      (pk, sk) <- (sqlAlterAddPrimaryKeyParser dbName) <|> return ([], [])
      pure $
        SqlUpdates
          Nothing
          (pk <> sk)
    _ -> pure $ SqlUpdates Nothing []

updateParser :: String -> Parser e [(Maybe SqlUpdates, Maybe FieldDef)]
updateParser dbName = do
  sqlUpdateStampParser
  res <- many ((asFst (sqlUpdatesParser dbName)) <|> (asSnd (sqlAlterTableAddColumn dbName True)))
  pure res
  where
    asSnd f = (Nothing,) . Just <$> f
    asFst f = (,Nothing) . Just <$> f

migrationFileParser :: String -> String -> Parser e MigrationFile
migrationFileParser dbName lastSqlFile = do
  !tableName <- sqlCreateParser dbName
  fields' <- many (sqlAlterTableAddColumn dbName False)
  keys <- (sqlAlterAddPrimaryKeyParser dbName) <|> return ([], [])
  fieldUpdates' <-
    concat
      <$> many (updateParser dbName)
  let !(columnUpdates, newFields) = second catMaybes . first catMaybes $ unzip fieldUpdates'
  let !deletedFields = L.nub . map fst . filter ((== DropColumn) . snd) $ mapMaybe fieldUpdates columnUpdates
  let !finalFieldsDeleted =
        filter
          ( \delField ->
              foldl'
                ( \res updF ->
                    case updF of
                      (Just sqlUpdate, Nothing) -> if (fst <$> fieldUpdates sqlUpdate) == Just delField then (snd <$> fieldUpdates sqlUpdate) == Just DropColumn else res
                      (Nothing, Just newField) -> if (fieldName newField) /= delField then res else False
                      _ -> res
                )
                True
                fieldUpdates'
          )
          deletedFields
  let !fields = filter (\field -> fieldName field `notElem` finalFieldsDeleted) $ fields' <> newFields
  let (pk, sk) =
        foldl'
          ( \(pkAcc, skAcc) columnUpdate -> case keysInPrimaryKey columnUpdate of
              (x : xs) -> ([x], xs)
              [] -> (pkAcc, skAcc)
          )
          keys
          columnUpdates
  let !finalFieldUpdates =
        foldr
          ( \sqlUpdate accMap -> do
              case fieldUpdates sqlUpdate of
                Nothing -> accMap
                Just (fieldName, updateAction) -> do
                  case M.lookup fieldName accMap of
                    Just val -> M.insert fieldName (groupRelevant updateAction val) accMap
                    Nothing -> M.insert fieldName (groupRelevant updateAction ([], [])) accMap
          )
          M.empty
          columnUpdates
  let !finalFields =
        map
          ( \field ->
              field
                { beamFields =
                    map
                      ( \beamField -> do
                          let (notNullRelatedAcc, defaultRelatedAcc) = fromMaybe ([], []) $ M.lookup beamField.bFieldName finalFieldUpdates
                          let ubf' =
                                -- using head and last below because we are never going to get empty array below, could have used NonEmpty array, will be in next PR along with other refactor.
                                case lastMay notNullRelatedAcc of
                                  Just AddNotNull | NotNull `notElem` beamField.bConstraints -> beamField {bConstraints = NotNull : beamField.bConstraints}
                                  Just DropNotNull -> beamField {bConstraints = filter (/= NotNull) beamField.bConstraints}
                                  _ -> beamField
                          case lastMay defaultRelatedAcc of
                            Just DropDefault -> ubf' {bDefaultVal = Nothing}
                            Just (AddDefault val) -> ubf' {bDefaultVal = Just val}
                            _ -> ubf'
                      )
                      (beamFields field)
                }
          )
          fields
  pure $ MigrationFile tableName finalFields pk sk lastSqlFile
  where
    groupRelevant sqlUpdate (notNullRelatedAcc, defaultRelatedAcc)
      | sqlUpdate `elem` [DropNotNull, AddNotNull] = (sqlUpdate : notNullRelatedAcc, defaultRelatedAcc)
      | sqlUpdate `elem` [AddDefault "", DropDefault] = (notNullRelatedAcc, sqlUpdate : defaultRelatedAcc)
      | otherwise = (notNullRelatedAcc, defaultRelatedAcc)

runAnyParser :: Show a => Parser e a -> String -> IO (Maybe a)
runAnyParser parser str = do
  pure $ go (runParser parser $ BSU.fromString str)
  where
    go (OK r _) = Just r
    go _ = Nothing

getOldSqlFile :: String -> FilePath -> IO (Maybe MigrationFile)
getOldSqlFile dbName filepath = do
  fileExist <- doesFileExist filepath
  if fileExist
    then do
      lastSqlFile <- BS.readFile filepath
      print ("loading old sql file" <> filepath :: String)
      pure $ go (runParser (migrationFileParser dbName (BSU.toString lastSqlFile)) $ cleanedFile lastSqlFile)
    else pure Nothing
  where
    cleanedFile = joinCleanWithNewLine . removeInlineComments . removeFullLineComments . clearExtraLines
    joinCleanWithNewLine ls = BS.append (BS.intercalate (BSU.fromString "\n") ls) (BSU.fromString "\n")
    removeFullLineComments = filter (\line -> BS.isInfixOf (BSU.fromString "SQL updates") line || not (BS.isPrefixOf (BSU.fromString "--") line))
    removeInlineComments =
      map
        ( \line -> do
            let splitWithColonRev = reverse $ BS.split 59 line -- 59 = ";"
            case splitWithColonRev of
              [] -> line
              [res] -> if res == BSU.fromString updateStamp then res else error "Line neither a comment nor a correct query."
              (_revComment : revQuery) -> BS.concat [BS.intercalate (BSU.fromString ";") $ reverse revQuery, BSU.fromString ";"]
        )
    clearExtraLines = filter (not . BS.null) . BS.split 10 -- 10 = "\n"
    go (OK r _) = Just r
    go _ = Nothing

storageParser :: FilePath -> IO [TableDef]
storageParser filepath = do
  contents <- BS.readFile filepath
  case Yaml.decodeEither' contents of
    Left _ -> error "Not a Valid Yaml"
    Right yml -> do
      let modelList = toModelList yml
          dList = fst <$> modelList
          tableDef = map (parseTableDef dList yml) $ filter ((/= "imports") . fst) modelList
      pure $ map (modifyRelationalTableDef tableDef) tableDef

modifyWithIdRelationalField :: FieldDef -> FieldDef
modifyWithIdRelationalField fieldDef =
  case relation fieldDef of
    Just (WithId _ _) ->
      fieldDef
        { beamFields =
            [ BeamField
                { bFieldName = (fieldName fieldDef) ++ "Id",
                  hFieldType = haskellType fieldDef,
                  bFieldType = "Kernel.Prelude.Maybe Kernel.Prelude.Text",
                  bConstraints = [],
                  bSqlType = "character varying(36)",
                  bDefaultVal = Nothing,
                  bFieldUpdates = [],
                  bfieldExtractor = ["Kernel.Types.Id.getId", "(.id) <$>"],
                  bToTType = Nothing,
                  bIsEncrypted = False
                }
            ]
        }
    Just (WithIdStrict _ _) ->
      fieldDef
        { beamFields =
            [ BeamField
                { bFieldName = (fieldName fieldDef) ++ "Id",
                  hFieldType = haskellType fieldDef,
                  bFieldType = "Kernel.Prelude.Text",
                  bConstraints = [NotNull],
                  bSqlType = "character varying(36)",
                  bDefaultVal = Nothing,
                  bFieldUpdates = [],
                  bfieldExtractor = ["Kernel.Types.Id.getId", "(.id)"],
                  bToTType = Nothing,
                  bIsEncrypted = False
                }
            ]
        }
    _ -> fieldDef

modifyRelationalTableDef :: [TableDef] -> TableDef -> TableDef
modifyRelationalTableDef allTableDefs tableDef@TableDef {..} = do
  let mbForeignTable = find (\tDef -> tableDef.tableNameHaskell `elem` tDef.relationalTableNamesHaskell) allTableDefs
  case mbForeignTable <&> (.tableNameHaskell) of
    Just foreignTableNameHaskell -> do
      let fieldName = lowercaseFirstLetter foreignTableNameHaskell ++ "Id"
          haskellType = "Kernel.Types.Id.Id ()"
          foreignField =
            FieldDef
              { fieldName = fieldName,
                haskellType = haskellType,
                beamFields =
                  [ BeamField
                      { bFieldName = lowercaseFirstLetter foreignTableNameHaskell ++ "Id",
                        hFieldType = haskellType,
                        bFieldType = "Kernel.Prelude.Text",
                        bConstraints = [NotNull],
                        bSqlType = "character varying(36)",
                        bDefaultVal = Nothing,
                        bFieldUpdates = [],
                        bfieldExtractor = [],
                        bToTType = Nothing,
                        bIsEncrypted = False
                      }
                  ],
                fromTType = Nothing,
                isEncrypted = False,
                relation = Nothing,
                relationalTableNameHaskell = Nothing
              }
          query =
            [ QueryDef
                { queryName = "findAllBy" ++ foreignTableNameHaskell ++ "Id",
                  kvFunction = "findAllWithKV",
                  params = [],
                  whereClause =
                    Leaf
                      ( fieldName,
                        haskellType,
                        Just Eq
                      ),
                  orderBy = defaultOrderBy,
                  takeFullObjectAsInput = False
                },
              QueryDef
                { queryName = "findBy" ++ foreignTableNameHaskell ++ "Id",
                  kvFunction = "findOneWithKV",
                  params = [],
                  whereClause =
                    Leaf
                      ( fieldName,
                        haskellType,
                        Just Eq
                      ),
                  orderBy = defaultOrderBy,
                  takeFullObjectAsInput = False
                }
            ]
      TableDef {fields = fields <> [foreignField], queries = queries <> query, ..}
    Nothing -> TableDef {..}

parseTableDef :: [String] -> Object -> (String, Object) -> TableDef
parseTableDef dList importObj (parseDomainName, obj) =
  let parsedTypesAndExcluded = parseExtraTypes parseDomainName dList importObj obj
      parsedTypes = view _1 <$> parsedTypesAndExcluded
      excludedList = view _2 <$> parsedTypesAndExcluded
      enumList = maybe [] (view _3) parsedTypesAndExcluded
      parsedFields = map modifyWithIdRelationalField $ parseFields (Just parseDomainName) excludedList dList enumList (fromMaybe [] parsedTypes) importObj obj
      containsEncryptedField = any isEncrypted parsedFields
      parsedImports = parseImports parsedFields (fromMaybe [] parsedTypes)
      parsedImportPackageOverrides = fromMaybe M.empty $ preview (ix "importPackageOverrides" . _Value . to mkList . to M.fromList) obj
      parsedQueries = parseQueries (Just parseDomainName) excludedList dList parsedFields importObj obj
      parsedExtraOperations = fromMaybe [] $ preview (ix "extraOperations" . _Array . to V.toList . to (map (extraOperation . valueToString))) obj
      parsedDerives = preview (ix "derives" ._String) obj
      parsedBeamTypeInstance = fromMaybe MakeTableInstances $ preview (ix "beamInstance" . _String . to mkBeamInstance) obj
      (primaryKey, secondaryKey) = extractKeys parsedFields
      relationalTableNamesHaskell = catMaybes $ map (.relationalTableNameHaskell) parsedFields
   in TableDef parseDomainName (quietSnake parseDomainName) parsedFields parsedImports parsedImportPackageOverrides parsedQueries primaryKey secondaryKey parsedTypes containsEncryptedField relationalTableNamesHaskell parsedDerives parsedBeamTypeInstance parsedExtraOperations

mkBeamInstance :: String -> BeamInstance
mkBeamInstance rw =
  case instanceName of
    "MakeTableInstances" -> MakeTableInstances
    "MakeTableInstancesGenericSchema" -> MakeTableInstancesGenericSchema
    "MakeTableInstancesWithTModifier" -> MakeTableInstancesWithTModifier extraParams
    _ -> error $ T.pack $ "Unknow Beam Instance " <> instanceName
  where
    (instanceName, extraParams) = L.break (== ' ') rw

parseImports :: [FieldDef] -> [TypeObject] -> [String]
parseImports fields typObj =
  figureOutImports (map haskellType fields <> concatMap figureOutInsideTypeImports typObj <> concatMap (figureOutBeamFieldsImports . beamFields) fields)
  where
    figureOutBeamFieldsImports :: [BeamField] -> [String]
    figureOutBeamFieldsImports bms = map bFieldType bms <> map hFieldType bms

    figureOutInsideTypeImports :: TypeObject -> [String]
    figureOutInsideTypeImports tobj@(TypeObject _ (_, (tps, _))) =
      let isEnum = isEnumType tobj
       in concatMap
            ( ( \potentialImport ->
                  if isEnum
                    then filter ('.' `elem`) $ splitWhen (`elem` ("() []," :: String)) potentialImport
                    else [potentialImport]
              )
                . snd
            )
            tps

--extraImports = concatMap (\f -> maybe [] pure (toTType f) <> maybe [] pure (fromTType f)) fields

searchForKey :: [FieldDef] -> String -> ((String, String), Bool)
searchForKey fields inputKey = do
  let errorMsg = error $ T.pack $ "Param " ++ inputKey ++ " not found in fields"
  let filedDef = fromMaybe errorMsg $ find ((== inputKey) . fieldName) fields
  ((inputKey, haskellType filedDef), isEncrypted filedDef)

parseQueries :: Maybe String -> Maybe [String] -> [String] -> [FieldDef] -> Object -> Object -> [QueryDef]
parseQueries moduleName excludedList dList fields impObj obj = do
  let mbQueries = preview (ix "queries" . _Value . to mkListObject) obj
      defaultImportModule = "Domain.Types."
      makeTypeQualified' = makeTypeQualified moduleName excludedList (Just dList) defaultImportModule impObj
      parseQuery query =
        let queryName = fst query
            queryDataObj = snd query
            params = addDefaultUpdatedAtToQueryParams queryName $ map (first (second makeTypeQualified')) $ fromMaybe [] (queryDataObj ^? ix "params" . _Array . to V.toList . to (map (searchForKey fields . valueToString)))
            kvFunction = fromMaybe (error "kvFunction is neccessary") (queryDataObj ^? ix "kvFunction" . _String)
            whereClause = fromMaybe EmptyWhere (queryDataObj ^? ix "where" . to (parseWhereClause makeTypeQualified' "eq" fields))
            orderBy = fromMaybe defaultOrderBy (queryDataObj ^? ix "orderBy" . to (parseOrderBy fields))
         in QueryDef queryName kvFunction params whereClause orderBy False

  case mbQueries of
    Just queries -> map parseQuery queries
    Nothing -> []
  where
    addDefaultUpdatedAtToQueryParams :: String -> [((String, String), Bool)] -> [((String, String), Bool)]
    addDefaultUpdatedAtToQueryParams queryName params =
      if "update" `L.isPrefixOf` queryName
        then if any (\((k, _), _) -> k == "updatedAt") params then params else params <> [(("updatedAt", "Kernel.Prelude.UTCTime"), False)]
        else params

parseOrderBy :: [FieldDef] -> Value -> (String, Order)
parseOrderBy fields (String st) = do
  let ((key_, _), _) = searchForKey fields (T.unpack st)
  (key_, Desc)
parseOrderBy fields (Object obj) = do
  let obj' = KM.toList obj
  extractFieldOrder fields obj'
parseOrderBy _ val = error $ T.pack $ "Invalid orderBy: Must be a string or an object: " <> show val

extractFieldOrder :: [FieldDef] -> [(Key, Value)] -> (String, Order)
extractFieldOrder fields input = do
  let fieldValue = fromMaybe (error "`field` param is missing for orderBy") $ lookupString "field" input
  let orderValue = fromMaybe (error "`order` param is missing for orderBy") $ lookupString "order" input
  if unique "field" input && unique "order" input
    then do
      let ((key_, _), _) = searchForKey fields fieldValue
      (key_, castOrder orderValue)
    else error "`field` and `order` should be unique"
  where
    lookupString _key = fmap valueToString . lookup (fromString _key)
    unique _key xs = length (filter ((== fromString _key) . fst) xs) == 1

    castOrder :: String -> Order
    castOrder "desc" = Desc
    castOrder "asc" = Asc
    castOrder _ = error "Order type can be either `desc` or `asc`"

parseWhereClause :: (String -> String) -> String -> [FieldDef] -> Value -> WhereClause
parseWhereClause mkQTypeFunc operatorStr fields (String st) = do
  let ((key_, value), _) = searchForKey fields (T.unpack st)
  Leaf (key_, mkQTypeFunc value, Just $ parseOperator (T.unpack (T.toLower (T.pack operatorStr))))
parseWhereClause mkQTypeFunc _ fields (Object clauseObj) = do
  let clauseObj' = KM.toList clauseObj
  case clauseObj' of
    [(operatorStr, value)] -> do
      case value of
        Array arr_ -> do
          let op_ = if parseOperator (T.unpack (T.toLower (T.pack (toString operatorStr)))) `elem` comparisonOperator then toString operatorStr else "Eq"
          let clauses = map (parseWhereClause mkQTypeFunc op_ fields) (V.toList arr_)
          Query (parseOperator (toString operatorStr), clauses)
        _ -> error "Invalid where clause, operator must be followed by an array of clauses"
    _ -> error "Invalid where clause, element of where clause array must be an single key object"
parseWhereClause _ _ _ val = error $ T.pack $ "Invalid where clause, must be a string or an object: " <> show val

parseOperator :: String -> Operator
parseOperator "and" = And
parseOperator "or" = Or
parseOperator "in" = In
parseOperator "eq" = Eq
parseOperator "gt" = GreaterThan
parseOperator "lt" = LessThan
parseOperator "gte" = GreaterThanOrEq
parseOperator "lte" = LessThanOrEq
parseOperator val = error $ "Invalid operator " <> show val

parseTypes :: Object -> Maybe [TypeObject]
parseTypes obj = case preview (ix "types" ._Object) obj of
  Just typesObj -> Just $ parseTypeObjects typesObj
  _ -> Nothing

parseTypeObjects :: Object -> [TypeObject]
parseTypeObjects obj =
  map processType $ KM.toList obj
  where
    extractFields :: KM.KeyMap Value -> [(String, String)]
    extractFields = map (first toString) . KM.toList . fmap extractString

    extractString :: Value -> String
    extractString (String t) = T.unpack t
    extractString _ = error "Non-string type found in field definition"

    splitTypeAndDerivation :: [(String, String)] -> ([(String, String)], [String])
    splitTypeAndDerivation fields = (filter (\(k, _) -> not $ k `elem` ["derive", "recordType"]) fields, extractDerive fields)
      where
        extractDerive :: [(String, String)] -> [String]
        extractDerive [] = []
        extractDerive ((k, value) : xs)
          | k == "derive" = map T.unpack (T.split (== ',') (T.pack value))
          | otherwise = extractDerive xs

    extractRecordType :: Object -> RecordType
    extractRecordType =
      (fromMaybe Data)
        . ( preview
              ( ix "recordType" . _String
                  . to
                    ( \case
                        "NewType" -> NewType
                        "Data" -> Data
                        "Type" -> Type
                        _ -> error "Not a valid"
                    )
              )
          )

    processType :: (Key, Value) -> TypeObject
    processType (typeName, Object typeDef) =
      TypeObject (extractRecordType typeDef) (toString typeName, splitTypeAndDerivation $ extractFields typeDef)
    processType _ = error "Expected an object in fields"

parseExtraTypes :: String -> [String] -> Object -> Object -> Maybe ([TypeObject], [String], [String])
parseExtraTypes moduleName dList importObj obj = do
  _types <- parseTypes obj
  let allExcludeQualified = map (\(TypeObject _ (name, _)) -> name) _types
  let allEnums = map (\(TypeObject _ (name, _)) -> name) $ filter isEnumType _types
  return (map (mkQualifiedTypeObject allExcludeQualified) _types, allExcludeQualified, map (\nm -> defaultImportModule ++ moduleName ++ "." ++ nm) allEnums ++ allEnums)
  where
    defaultImportModule = "Domain.Types."

    mkEnumTypeQualified :: [String] -> String -> String
    mkEnumTypeQualified excluded enumTp =
      let individualEnums = L.trim <$> L.splitOn "," enumTp
       in L.intercalate "," $ map (uncurry (<>) . second (makeTypeQualified (Just moduleName) (Just excluded) (Just dList) defaultImportModule importObj) . L.breakOn " ") individualEnums

    mkQualifiedTypeObject :: [String] -> TypeObject -> TypeObject
    mkQualifiedTypeObject excluded (TypeObject recType (_nm, (arrOfFields, derive))) =
      TypeObject
        recType
        ( _nm,
          ( map
              ( \(_n, _t) ->
                  ( _n,
                    if _n == "enum"
                      then mkEnumTypeQualified excluded _t
                      else makeTypeQualified (Just moduleName) (Just excluded) (Just dList) defaultImportModule importObj _t
                  )
              )
              arrOfFields,
            derive
          )
        )

parseFields :: Maybe String -> Maybe [String] -> [String] -> [String] -> [TypeObject] -> Object -> Object -> [FieldDef]
parseFields moduleName excludedList dataList enumList definedTypes impObj obj =
  let unfilteredFields = preview (ix "fields" . _Value . to mkList) obj
      excludedDefaultFields = preview (ix "excludedFields" . _Array . to V.toList . to (map valueToString)) obj
      fields = (++) <$> unfilteredFields <*> (getNotPresentDefaultFields excludedDefaultFields <$> unfilteredFields)
      getFieldDef field =
        let fieldName = fst field
            (haskellType, optionalRelation) = break (== '|') $ snd field
            fieldKey = fromString fieldName
            parseFromTType = obj ^? (ix "fromTType" . _Object) >>= preview (ix fieldKey . _String . to (makeTF impObj))
            defaultImportModule = "Domain.Types."
            getbeamFields = makeBeamFields (fromMaybe (error "Module name not found") moduleName) excludedList dataList enumList fieldName haskellType definedTypes impObj obj
            typeQualifiedHaskellType = makeTypeQualified moduleName excludedList (Just dataList) defaultImportModule impObj haskellType
            fieldRelationAndModule =
              if has (ix "beamFields" . _Object . ix fieldKey . _Object) obj
                then Nothing
                else getFieldRelationAndHaskellType $ typeQualifiedHaskellType <> optionalRelation
         in FieldDef
              { fieldName = fieldName,
                haskellType = typeQualifiedHaskellType,
                beamFields = getbeamFields,
                fromTType = maybe (if length getbeamFields > 1 then error ("Complex type (" <> T.pack fieldName <> ") should have fromTType function") else Nothing) pure parseFromTType,
                isEncrypted = "EncryptedHashedField" `T.isInfixOf` (T.pack haskellType),
                relation = fst <$> fieldRelationAndModule,
                relationalTableNameHaskell = snd <$> fieldRelationAndModule
              }
   in case map getFieldDef <$> fields of
        Just f -> f
        Nothing -> error "Error Parsing Fields"

beamFieldsWithExtractors :: String -> Maybe Object -> String -> String -> [TypeObject] -> [String] -> [(String, String, [String])]
beamFieldsWithExtractors moduleName beamFieldObj fieldName haskellType definedTypes extractorFuncs =
  case beamFieldObj >>= preview (ix (fromString fieldName) . _Object . to Object . to mkList) of
    Just arrOfFields ->
      foldl (\acc (nm, tpp) -> acc ++ [(nm, tpp, [])]) [] arrOfFields
    Nothing ->
      case findIfComplexType haskellType of
        Just (TypeObject _ (_nm, (arrOfFields, _))) ->
          foldl (\acc (nm, tpp) -> acc ++ beamFieldsWithExtractors moduleName beamFieldObj (fieldName ++ capitalise nm) tpp definedTypes (qualified nm : extractorFuncs)) [] arrOfFields
        Nothing ->
          [(fromMaybe fieldName (beamFieldObj >>= preview (ix (fromString fieldName) . _String)), haskellType, extractorFuncs)]
  where
    qualified tp = "Domain.Types." ++ moduleName ++ "." ++ tp
    capitalise :: String -> String
    capitalise [] = []
    capitalise (c : cs) = toUpper c : cs

    findIfComplexType :: String -> Maybe TypeObject
    findIfComplexType tpp = find (\(TypeObject _ (nm, (arrOfFields, _))) -> (nm == tpp || tpp == "Domain.Types." ++ moduleName ++ "." ++ nm) && all (\(k, _) -> k /= "enum") arrOfFields) definedTypes

makeTF :: Object -> String -> TransformerFunction
makeTF impObj func =
  TransformerFunction
    { tfName = qualifiedName,
      tfType = tfType'
    }
  where
    (name, details) = L.break (== '|') func
    tfType' = if 'M' `L.elem` details then MonadicT else PureT
    isImported = 'I' `L.elem` details || '.' `L.elem` name
    qualifiedName =
      if isImported
        then
          if '.' `L.elem` name
            then name
            else maybe (error $ T.pack $ "Function " <> func <> " not imported") (\nm -> nm <> "." <> name) $ impObj ^? ix "imports" . key (fromString name) . _String
        else name

makeBeamFields :: String -> Maybe [String] -> [String] -> [String] -> String -> String -> [TypeObject] -> Object -> Object -> [BeamField]
makeBeamFields moduleName excludedList dataList enumList fieldName haskellType definedTypes impObj obj =
  let constraintsObj = obj ^? (ix "constraints" . _Object)
      sqlTypeObj = obj ^? (ix "sqlType" . _Object)
      beamTypeObj = obj ^? (ix "beamType" ._Object)
      beamFieldObj = obj ^? (ix "beamFields" . _Object)
      defaultsObj = obj ^? (ix "default" . _Object)
      extractedBeamInfos = beamFieldsWithExtractors moduleName beamFieldObj fieldName haskellType definedTypes []
      getBeamFieldDef (fName, tpp, extractorFuncs) =
        let fieldKey = fromString fName
            beamType = fromMaybe (findBeamType tpp) (beamTypeObj >>= preview (ix fieldKey . _String))
            sqlType = fromMaybe (findMatchingSqlType enumList beamType) (sqlTypeObj >>= preview (ix fieldKey . _String))
            defaultImportModule = "Domain.Types."
            defaultValue = maybe (sqlDefaultsWrtName fName) pure (defaultsObj >>= preview (ix fieldKey . _String))
            parseToTType = obj ^? (ix "toTType" . _Object) >>= preview (ix fieldKey . _String . to (makeTF impObj))
            constraints = L.nub $ getDefaultFieldConstraints fName tpp ++ fromMaybe [] (constraintsObj >>= preview (ix fieldKey . _String . to (splitOn "|") . to (map getProperConstraint)))
            isEncrypted = "EncryptedHashedField" `T.isInfixOf` T.pack tpp
         in BeamField
              { bFieldName = fName,
                hFieldType = makeTypeQualified (Just moduleName) excludedList (Just dataList) defaultImportModule impObj tpp,
                bFieldType = makeTypeQualified (Just moduleName) excludedList (Just dataList) defaultImportModule impObj beamType,
                bConstraints = constraints,
                bFieldUpdates = [], -- not required while creating
                bSqlType = sqlType,
                bDefaultVal = defaultValue,
                bToTType = parseToTType,
                bfieldExtractor = extractorFuncs,
                bIsEncrypted = isEncrypted
              }
   in map getBeamFieldDef extractedBeamInfos

findBeamType :: String -> String
findBeamType str = concatMap (typeMapper . L.trimStart) (split (whenElt (`elem` typeDelimiter)) str)
  where
    typeDelimiter :: String
    typeDelimiter = "()[]"
    typeMapper :: String -> String
    typeMapper hkType
      | L.isPrefixOf "Id " hkType || L.isPrefixOf "Kernel.Types.Id.Id " hkType = "Text"
      | L.isPrefixOf "ShortId " hkType || L.isPrefixOf "Kernel.Types.Id.ShortId " hkType = "Text"
      | otherwise = hkType

getDefaultFieldConstraints :: String -> String -> [FieldConstraint]
getDefaultFieldConstraints nm tp = primaryKeyConstraint ++ notNullConstraint
  where
    trimmedTp = L.trim tp
    primaryKeyConstraint = [PrimaryKey | nm == "id"]
    notNullConstraint =
      [ NotNull | not (isMaybeType trimmedTp)
      ]

getProperConstraint :: String -> FieldConstraint
getProperConstraint txt = case L.trim txt of
  "PrimaryKey" -> PrimaryKey
  "SecondaryKey" -> SecondaryKey
  "NotNull" -> NotNull
  "AUTOINCREMENT" -> AUTOINCREMENT
  _ -> error "No a proper contraint type"

toModelList :: Object -> [(String, Object)]
toModelList obj =
  KM.toList obj >>= \(k, v) -> case v of
    Object o -> [(toString k, o)]
    _ -> []

mkListObject :: Value -> [(String, Object)]
mkListObject (Object obj) =
  KM.toList obj >>= \(k, v) -> case v of
    Object t -> [(toString k, t)]
    _ -> []
mkListObject _ = []

-- Default Fields --
defaultFields :: [(String, String)]
defaultFields =
  [ ("merchantId", "Maybe (Id Merchant)"),
    ("merchantOperatingCityId", "Maybe (Id MerchantOperatingCity)"),
    ("createdAt", "UTCTime"),
    ("updatedAt", "UTCTime")
  ]

getNotPresentDefaultFields :: Maybe [String] -> [(String, String)] -> [(String, String)]
getNotPresentDefaultFields excludedDefaultFields fields = filter (\(k, _) -> (k `notElem` map fst fields && k `notElem` (fromMaybe [] excludedDefaultFields))) defaultFields

-- SQL Types --
findMatchingSqlType :: [String] -> String -> String
findMatchingSqlType allEnums haskellType =
  if any (haskellType =~) allEnums
    then "text"
    else case filter ((haskellType =~) . fst) sqlTypeWrtType of
      [] -> "text"
      ((_, sqlType) : _) -> sqlType

sqlDefaultsWrtName :: String -> Maybe String
sqlDefaultsWrtName = \case
  "createdAt" -> Just "CURRENT_TIMESTAMP"
  "updatedAt" -> Just "CURRENT_TIMESTAMP"
  _ -> Nothing

sqlTypeWrtType :: [(String, String)]
sqlTypeWrtType =
  [ ("\\[Text\\]", "text[]"),
    ("Text", "text"),
    ("\\[Id ", "text[]"),
    ("Id ", "character varying(36)"),
    ("\\[ShortId ", "text[]"),
    ("ShortId ", "character varying(36)"),
    ("Int", "integer"),
    ("Double", "double precision"),
    ("HighPrecMoney", "double precision"),
    ("Money", "integer"),
    ("Bool", "boolean"),
    ("UTCTime", "timestamp with time zone"),
    ("TimeOfDay", "time without time zone"),
    ("Day", "date"),
    ("Seconds", "integer"),
    ("Kilometers", "integer"),
    ("Meters", "integer")
  ]

extractKeys :: [FieldDef] -> ([String], [String])
extractKeys fieldDefs = extractKeysFromBeamFields (concatMap beamFields fieldDefs)

extractKeysFromBeamFields :: [BeamField] -> ([String], [String])
extractKeysFromBeamFields fieldDefs = (primaryKeyFields, secondaryKeyFields)
  where
    primaryKeyFields = [bFieldName fd | fd <- fieldDefs, PrimaryKey `elem` bConstraints fd]
    secondaryKeyFields = [bFieldName fd | fd <- fieldDefs, SecondaryKey `elem` bConstraints fd]

-- SQL reverse parse
findMatchingHaskellType :: String -> String
findMatchingHaskellType sqlType =
  case filter ((sqlType =~) . fst) haskellTypeWrtSqlType of
    [] -> error $ "Type not found " <> T.pack sqlType
    ((_, haskellType) : _) -> haskellType

haskellTypeWrtSqlType :: [(String, String)]
haskellTypeWrtSqlType = map (first (T.unpack . T.replace "(" "\\(" . T.replace ")" "\\)" . T.replace "[" "\\[" . T.replace "]" "\\]" . T.pack) . second (T.unpack . T.replace "\\[" "[" . T.replace "\\]" "]" . T.pack) . swap) sqlTypeWrtType

isEnumType :: TypeObject -> Bool
isEnumType (TypeObject _ (_, (arrOfFields, _))) = any (\(k, _) -> k == "enum") arrOfFields
