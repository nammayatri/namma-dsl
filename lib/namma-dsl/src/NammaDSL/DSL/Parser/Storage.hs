{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module NammaDSL.DSL.Parser.Storage (storageParser, getOldSqlFile, debugParser, runAnyParser) where

import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad (when)
import Control.Monad.Trans.RWS.Lazy
import Data.Aeson
import Data.Aeson.Key (fromString, toString)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Lens (key, _Array, _Object, _Value)
import Data.Bifunctor
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import Data.Char (toUpper)
import Data.Default
import Data.Foldable (foldlM)
import Data.List (find, foldl')
import qualified Data.List as L
import qualified Data.List.Extra as L
import Data.List.Split (split, splitOn, splitWhen, whenElt)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, mapMaybe, maybeToList)
import qualified Data.Text as T
import Data.Tuple (swap)
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml
import qualified Debug.Trace as DT
import FlatParse.Basic
import NammaDSL.AccessorTH
import NammaDSL.DSL.Syntax.Common
import NammaDSL.DSL.Syntax.Storage
import NammaDSL.Utils hiding (typeDelimiter)
import Safe (lastMay)
import System.Directory (doesFileExist)
import Text.Casing (quietSnake)
import Text.Regex.TDFA ((=~))
import Prelude

parseTableDef :: StorageParserM ()
parseTableDef = do
  parseExtraTypes
  parseFields
  parseImports
  parseQueries
  parseDerives
  parseBeamTypeInstance
  parsePrimaryAndSecondaryKeys
  parseRelationalTableNamesHaskell
  parseExtraOperations

parseExtraTypes :: StorageParserM ()
parseExtraTypes = do
  dList <- gets (.extraParseInfo.dList)
  importObj <- gets (.extraParseInfo.yamlObject)
  moduleName <- gets (.extraParseInfo.domainName)
  defaultImportModule <- asks (.domainTypeModulePrefix)
  defaultTypeImportMap <- asks (.storageDefaultTypeImportMapper)
  parseTypes
  _types <- gets (types . tableDef)
  let mkEnumTypeQualified = \enumTp ->
        L.intercalate "," $ map (uncurry (<>) . second (makeTypeQualified defaultTypeImportMap (Just moduleName) (Just allExcludeQualified) (Just dList) defaultImportModule importObj) . L.breakOn " ") (L.trim <$> L.splitOn "," enumTp)
      mkQualifiedTypeObject = \(TypeObject recType (_nm, (arrOfFields, derive))) ->
        TypeObject
          recType
          ( _nm,
            ( map
                ( \(_n, _t) ->
                    ( _n,
                      if _n == "enum"
                        then mkEnumTypeQualified _t
                        else makeTypeQualified defaultTypeImportMap (Just moduleName) (Just allExcludeQualified) (Just dList) defaultImportModule importObj _t
                    )
                )
                arrOfFields,
              derive
            )
          )
      types' = fromMaybe [] _types
      allExcludeQualified = map (\(TypeObject _ (name, _)) -> name) types'
      allEnums = map (\(TypeObject _ (name, _)) -> name) $ filter isEnumType types'
      qualifiedTypeObject = (map mkQualifiedTypeObject) <$> _types
      qualifiedAllEnums = map (\nm -> defaultImportModule ++ "." ++ moduleName ++ "." ++ nm) allEnums ++ allEnums
  modify $ \s -> s {tableDef = (tableDef s) {types = qualifiedTypeObject}, extraParseInfo = (extraParseInfo s) {enumList = qualifiedAllEnums, excludedImportList = allExcludeQualified}}

parseFields :: StorageParserM ()
parseFields = do
  moduleName <- gets (domainName . extraParseInfo)
  excludedList <- Just <$> gets (excludedImportList . extraParseInfo) -- remove just later ...
  dataList <- gets (dList . extraParseInfo)
  impObj <- gets (yamlObject . extraParseInfo)
  obj <- gets (dataObject . extraParseInfo)
  defaultImportModule <- asks (.domainTypeModulePrefix)
  extraDefaultFields <- asks (.extraDefaultFields)
  defaultTypeImportMap <- asks (.storageDefaultTypeImportMapper)
  let unfilteredFields = obj ^? ix acc_fields . _Value . to mkList
      excludedDefaultFields = obj ^? ix acc_excludedFields . _Array . to V.toList . to (map valueToString)
      getNotPresentDefaultFields = \flds -> filter (\(k, _) -> (k `notElem` map fst flds && k `notElem` (fromMaybe [] excludedDefaultFields))) extraDefaultFields
      fields = (++) <$> unfilteredFields <*> (getNotPresentDefaultFields <$> unfilteredFields)
      getFieldDef field = do
        let fieldName = fst field
            (haskellType, optionalRelation) = break (== '|') $ snd field
            fieldKey = fromString fieldName
            parseFromTType = obj ^? ix acc_fromTType . _Object . ix fieldKey . _String . to (makeTF impObj)
        getbeamFields <- makeBeamFields fieldName haskellType
        let typeQualifiedHaskellType = makeTypeQualified defaultTypeImportMap (Just moduleName) excludedList (Just dataList) defaultImportModule impObj haskellType
            fieldRelationAndModule =
              if has (ix acc_beamFields . _Object . ix fieldKey . _Object) obj
                then Nothing
                else getFieldRelationAndHaskellType $ typeQualifiedHaskellType <> optionalRelation
        pure $
          FieldDef
            { fieldName = fieldName,
              haskellType = typeQualifiedHaskellType,
              beamFields = getbeamFields,
              fromTType = maybe (if length getbeamFields > 1 then error ("Complex type (" <> fieldName <> ") should have fromTType function") else Nothing) pure parseFromTType,
              isEncrypted = "EncryptedHashedField" `T.isInfixOf` (T.pack haskellType),
              relation = fst <$> fieldRelationAndModule,
              relationalTableNameHaskell = snd <$> fieldRelationAndModule
            }
  case mapM getFieldDef <$> fields of
    Just fM -> do
      f <- fM
      let containsEncryptedField = any isEncrypted f
      modify $ \s -> s {tableDef = (tableDef s) {fields = f, containsEncryptedField = containsEncryptedField}}
    Nothing -> throwError $ InternalError "Error Parsing Fields"
  modifyWithIdRelationalFields

modifyWithIdRelationalFields :: StorageParserM ()
modifyWithIdRelationalFields = do
  fields <- gets (fields . tableDef)
  let modifiedFields =
        map
          ( \fieldDef ->
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
          )
          fields
  modify $ \s -> s {tableDef = (tableDef s) {fields = modifiedFields}}

parseImports :: StorageParserM ()
parseImports = do
  fields <- gets (fields . tableDef)
  typObj <- fromMaybe [] <$> gets (types . tableDef)
  let _imports = figureOutImports (map haskellType fields <> concatMap figureOutInsideTypeImports typObj <> concatMap (figureOutBeamFieldsImports . beamFields) fields)
  modify $ \s -> s {tableDef = (tableDef s) {imports = _imports}}
  parseImportPackageOverrides
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

parseQueries :: StorageParserM ()
parseQueries = do
  moduleName <- gets (.extraParseInfo.domainName)
  excludedList <- gets (.extraParseInfo.excludedImportList)
  dList <- gets (.extraParseInfo.dList)
  fields <- gets (.tableDef.fields)
  impObj <- gets (.extraParseInfo.yamlObject)
  obj <- gets (.extraParseInfo.dataObject)
  defaultImportModule <- asks (.domainTypeModulePrefix)
  defaultTypeImportMap <- asks (.storageDefaultTypeImportMapper)
  let makeTypeQualified' = makeTypeQualified defaultTypeImportMap (Just moduleName) (Just excludedList) (Just dList) defaultImportModule impObj
      mbQueries = obj ^? ix acc_queries . _Value . to mkListObject
      parseQuery query =
        let queryName = fst query
            queryDataObj = snd query
            params = addDefaultUpdatedAtToQueryParams queryName $ map (first (second makeTypeQualified')) $ fromMaybe [] (queryDataObj ^? ix acc_params . _Array . to V.toList . to (map (searchForKey fields . valueToString)))
            kvFunction = fromMaybe (error "kvFunction is neccessary") (queryDataObj ^? ix acc_kvFunction . _String)
            whereClause = fromMaybe EmptyWhere (queryDataObj ^? ix acc_where . to (parseWhereClause makeTypeQualified' "eq" fields))
            orderBy = fromMaybe defaultOrderBy (queryDataObj ^? ix acc_orderBy . to (parseOrderBy fields))
         in QueryDef queryName kvFunction params whereClause orderBy False
  case mbQueries of
    Just queries -> modify $ \s -> s {tableDef = (tableDef s) {queries = map parseQuery queries}}
    Nothing -> pure ()
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
    parseOrderBy _ val = error $ "Invalid orderBy: Must be a string or an object: " <> show val

parseImportPackageOverrides :: StorageParserM ()
parseImportPackageOverrides = do
  obj <- gets (dataObject . extraParseInfo)
  let parsedImportPackageOverrides = fromMaybe M.empty $ obj ^? ix acc_importPackageOverrides . _Value . to mkList . to M.fromList
  modify $ \s -> s {tableDef = (tableDef s) {importPackageOverrides = parsedImportPackageOverrides}}

parseExtraOperations :: StorageParserM ()
parseExtraOperations = do
  obj <- gets (dataObject . extraParseInfo)
  let parsedExtraOperations = fromMaybe [] $ obj ^? ix acc_extraOperations . _Array . to V.toList . to (map (extraOperation . valueToString))
  modify $ \s -> s {tableDef = (tableDef s) {extraOperations = parsedExtraOperations}}

parseDerives :: StorageParserM ()
parseDerives = do
  obj <- gets (dataObject . extraParseInfo)
  let parsedDerives = obj ^? ix acc_derives ._String
  modify $ \s -> s {tableDef = (tableDef s) {derives = parsedDerives}}

parseBeamTypeInstance :: StorageParserM ()
parseBeamTypeInstance = do
  obj <- gets (dataObject . extraParseInfo)
  let parsedBeamTypeInstance = fromMaybe MakeTableInstances $ obj ^? ix acc_beamInstance . _String . to mkBeamInstance
  modify $ \s -> s {tableDef = (tableDef s) {beamTableInstance = parsedBeamTypeInstance}}

parsePrimaryAndSecondaryKeys :: StorageParserM ()
parsePrimaryAndSecondaryKeys = do
  fields <- gets (fields . tableDef)
  let (primaryKey, secondaryKey) = extractKeys fields
  modify $ \s -> s {tableDef = (tableDef s) {primaryKey = primaryKey, secondaryKey = secondaryKey}}
  where
    extractKeys :: [FieldDef] -> ([String], [String])
    extractKeys fieldDefs = extractKeysFromBeamFields (concatMap beamFields fieldDefs)

    extractKeysFromBeamFields :: [BeamField] -> ([String], [String])
    extractKeysFromBeamFields fieldDefs = (primaryKeyFields, secondaryKeyFields)
      where
        primaryKeyFields = [bFieldName fd | fd <- fieldDefs, PrimaryKey `elem` bConstraints fd]
        secondaryKeyFields = [bFieldName fd | fd <- fieldDefs, SecondaryKey `elem` bConstraints fd]

parseRelationalTableNamesHaskell :: StorageParserM ()
parseRelationalTableNamesHaskell = do
  fields <- gets (fields . tableDef)
  let relationalTableNamesHaskell = catMaybes $ map (.relationalTableNameHaskell) fields
  modify $ \s -> s {tableDef = (tableDef s) {relationalTableNamesHaskell = relationalTableNamesHaskell}}

storageParser :: StorageRead -> FilePath -> IO [TableDef]
storageParser storageRead filepath = do
  contents <- BS.readFile filepath
  case Yaml.decodeEither' contents of
    Left _ -> error "Not a Valid Yaml"
    Right yml -> do
      let modelList = toModelList yml
          dList = fst <$> modelList
          extraParseInfo = def {dList = dList, yamlObject = yml}
          storageState = StorageState {tableDef = def, extraParseInfo = extraParseInfo}
      tableDef <- mapM (\table -> tableDef <$> evalParser parseTableDef storageRead (storageState {tableDef = def {tableNameSql = quietSnake (fst table), tableNameHaskell = fst table}, extraParseInfo = extraParseInfo {domainName = fst table, dataObject = snd table}})) $ filter ((/= "imports") . fst) modelList
      pure $ map (modifyRelationalTableDef tableDef) tableDef

-------------------------------------------------------------------------------------------------------------

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
parseAlterTablePrefix dbName = do
  let alterTableStmt = "ALTER TABLE " <> dbName <> "."
  return alterTableStmt *> pure ()

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

sqlAlterTableAddColumn :: String -> [(String, String)] -> Bool -> Parser e FieldDef
sqlAlterTableAddColumn dbName sqlTypeWrtType _fromUpdatesSection = do
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
            hFieldType = findMatchingHaskellType sqlTypeWrtType sqlType, -- not required, but anyways did.
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
      (findMatchingHaskellType sqlTypeWrtType sqlType)
      beamFields
      fromTType
      isEncrypted
      Nothing
      Nothing

sqlCreateParser :: String -> Parser e String
sqlCreateParser dd = do
  let createStmt = "CREATE TABLE " <> dd <> "."
  tableName <- many $ notFollowedBy anyChar $(char '(')
  $(string " ();\n")
  createStmt <$ pure tableName

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

updateParser :: [(String, String)] -> String -> Parser e [(Maybe SqlUpdates, Maybe FieldDef)]
updateParser sqlTypeWrtType dbName = do
  sqlUpdateStampParser
  res <- many ((asFst (sqlUpdatesParser dbName)) <|> (asSnd (sqlAlterTableAddColumn dbName sqlTypeWrtType True)))
  pure res
  where
    asSnd f = (Nothing,) . Just <$> f
    asFst f = (,Nothing) . Just <$> f

migrationFileParser :: [(String, String)] -> String -> String -> Parser e MigrationFile
migrationFileParser sqlTypeWrtType dbName lastSqlFile = do
  !tableName <- sqlCreateParser dbName
  fields' <- many (sqlAlterTableAddColumn dbName sqlTypeWrtType False)
  keys <- (sqlAlterAddPrimaryKeyParser dbName) <|> return ([], [])
  fieldUpdates' <-
    concat
      <$> many (updateParser sqlTypeWrtType dbName)
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

getOldSqlFile :: [(String, String)] -> String -> FilePath -> IO (Maybe MigrationFile)
getOldSqlFile sqlTypeWrtType dbName filepath = do
  fileExist <- doesFileExist filepath
  if fileExist
    then do
      lastSqlFile <- BS.readFile filepath
      print ("loading old sql file" <> filepath :: String)
      pure $ go (runParser (migrationFileParser sqlTypeWrtType dbName (BSU.toString lastSqlFile)) $ cleanedFile lastSqlFile)
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
              [res] -> if res == BSU.fromString updateStamp then res else error "Line neither a comment nor a correct query.\nPlease remove extra query lines you might have written in any read-only sql migration file.\nExtra sql migrations should be written in migration folder."
              (_revComment : revQuery) -> BS.concat [BS.intercalate (BSU.fromString ";") $ reverse revQuery, BSU.fromString ";"]
        )
    clearExtraLines = filter (not . BS.null) . BS.split 10 -- 10 = "\n"
    go (OK r _) = Just r
    go _ = Nothing

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

mkBeamInstance :: String -> BeamInstance
mkBeamInstance rw =
  case instanceName of
    "MakeTableInstances" -> MakeTableInstances
    "MakeTableInstancesGenericSchema" -> MakeTableInstancesGenericSchema
    "MakeTableInstancesWithTModifier" -> MakeTableInstancesWithTModifier extraParams
    _ -> error $ "Unknow Beam Instance " <> instanceName
  where
    (instanceName, extraParams) = L.break (== ' ') rw

searchForKey :: [FieldDef] -> String -> ((String, String), Bool)
searchForKey fields inputKey = do
  let errorMsg = error $ "Param " ++ inputKey ++ " not found in fields"
  let filedDef = fromMaybe errorMsg $ find ((== inputKey) . fieldName) fields
  ((inputKey, haskellType filedDef), isEncrypted filedDef)

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
parseWhereClause _ _ _ val = error $ "Invalid where clause, must be a string or an object: " <> show val

parseOperator :: String -> Operator
parseOperator val = case val of
  "and" -> And
  "or" -> Or
  "in" -> In
  "eq" -> Eq
  "gt" -> GreaterThan
  "lt" -> LessThan
  "gte" -> GreaterThanOrEq
  "lte" -> LessThanOrEq
  _ -> error $ "Invalid operator " <> show val

parseTypes :: StorageParserM ()
parseTypes = do
  obj <- gets (dataObject . extraParseInfo)
  let tps = ((map processType) . KM.toList) <$> obj ^? ix acc_types . _Object
  modify $ \s -> s {tableDef = (tableDef s) {types = tps}}
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
              ( ix acc_recordType . _String
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

beamFieldsWithExtractors :: String -> String -> [String] -> StorageParserM [(String, String, [String])]
beamFieldsWithExtractors fieldName haskellType extractorFuncs = do
  moduleName <- gets (.extraParseInfo.domainName)
  domainTypeModulePrefix <- asks (.domainTypeModulePrefix)
  definedTypes <- (fromMaybe []) <$> gets (.tableDef.types)
  obj <- gets (.extraParseInfo.dataObject)
  let beamFieldObj = obj ^? (ix acc_beamFields . _Object)
      qualified tp = domainTypeModulePrefix ++ "." ++ moduleName ++ "." ++ tp
      findIfComplexType tpp = find (\(TypeObject _ (nm, (arrOfFields, _))) -> (nm == tpp || tpp == domainTypeModulePrefix ++ "." ++ moduleName ++ "." ++ nm) && all (\(k, _) -> k /= "enum") arrOfFields) definedTypes
  case beamFieldObj >>= preview (ix (fromString fieldName) . _Object . to Object . to mkList) of
    Just arrOfFields ->
      pure $ foldl (\acc (nm, tpp) -> acc ++ [(nm, tpp, [])]) [] arrOfFields
    Nothing ->
      case findIfComplexType haskellType of
        Just (TypeObject _ (_nm, (arrOfFields, _))) -> do
          foldlM
            ( \acc (nm, tpp) -> do
                bFieldWithExt <- beamFieldsWithExtractors (fieldName ++ capitalise nm) tpp (qualified nm : extractorFuncs)
                pure $ acc ++ bFieldWithExt
            )
            []
            arrOfFields
        Nothing ->
          pure $ [(fromMaybe fieldName (beamFieldObj >>= preview (ix (fromString fieldName) . _String)), haskellType, extractorFuncs)]
  where
    capitalise :: String -> String
    capitalise [] = []
    capitalise (c : cs) = toUpper c : cs

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
            else maybe (error $ "Function " <> func <> " not imported") (\nm -> nm <> "." <> name) $ impObj ^? ix acc_imports . key (fromString name) . _String
        else name

makeBeamFields :: String -> String -> StorageParserM [BeamField]
makeBeamFields fieldName haskellType = do
  sqlTypeMapper <- asks (.sqlMapper)
  defaultImportModule <- asks (.domainTypeModulePrefix)
  moduleName <- gets (.extraParseInfo.domainName)
  defaultTypeImportMap <- asks (.storageDefaultTypeImportMapper)
  excludedList <- gets (.extraParseInfo.excludedImportList)
  dataList <- gets (.extraParseInfo.dList)
  enumList <- gets (.extraParseInfo.enumList)
  impObj <- gets (.extraParseInfo.yamlObject)
  obj <- gets (.extraParseInfo.dataObject)
  let constraintsObj = obj ^? (ix acc_constraints . _Object)
      sqlTypeObj = obj ^? (ix acc_sqlType . _Object)
      beamTypeObj = obj ^? (ix acc_beamType ._Object)
      defaultsObj = obj ^? (ix acc_default . _Object)
  extractedBeamInfos <- beamFieldsWithExtractors fieldName haskellType []
  let getBeamFieldDef (fName, tpp, extractorFuncs) =
        let fieldKey = fromString fName
            beamType = fromMaybe (findBeamType tpp) (beamTypeObj >>= preview (ix fieldKey . _String))
            sqlType = fromMaybe (findMatchingSqlType sqlTypeMapper enumList tpp beamType) (sqlTypeObj >>= preview (ix fieldKey . _String))
            defaultValue = maybe (sqlDefaultsWrtName fName) pure (defaultsObj >>= preview (ix fieldKey . _String))
            parseToTType = obj ^? (ix acc_toTType . _Object) >>= preview (ix fieldKey . _String . to (makeTF impObj))
            constraints = L.nub $ getDefaultFieldConstraints fName tpp ++ fromMaybe [] (constraintsObj >>= preview (ix fieldKey . _String . to (splitOn "|") . to (map getProperConstraint)))
            isEncrypted = "EncryptedHashedField" `T.isInfixOf` T.pack tpp
         in BeamField
              { bFieldName = fName,
                hFieldType = makeTypeQualified defaultTypeImportMap (Just moduleName) (Just excludedList) (Just dataList) defaultImportModule impObj tpp,
                bFieldType = makeTypeQualified defaultTypeImportMap (Just moduleName) (Just excludedList) (Just dataList) defaultImportModule impObj beamType,
                bConstraints = constraints,
                bFieldUpdates = [], -- not required while creating
                bSqlType = sqlType,
                bDefaultVal = defaultValue,
                bToTType = parseToTType,
                bfieldExtractor = extractorFuncs,
                bIsEncrypted = isEncrypted
              }
  pure $ map getBeamFieldDef extractedBeamInfos

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

-- SQL Types --
findMatchingSqlType :: [(String, String)] -> [String] -> String -> String -> String
findMatchingSqlType sqlMapper allEnums haskellType beamType
  | haskellType `elem` allEnums = "text"
  | otherwise = case filter ((haskellType =~) . fst) sqlMapper of
    [] -> case filter ((beamType =~) . fst) sqlMapper of
      [] -> "text"
      ((_, sqlType) : _) -> sqlType
    ((_, sqlType) : _) -> sqlType

sqlDefaultsWrtName :: String -> Maybe String
sqlDefaultsWrtName = \case
  "createdAt" -> Just "CURRENT_TIMESTAMP"
  "updatedAt" -> Just "CURRENT_TIMESTAMP"
  _ -> Nothing

isEnumType :: TypeObject -> Bool
isEnumType (TypeObject _ (_, (arrOfFields, _))) = any (\(k, _) -> k == "enum") arrOfFields

-- SQL reverse parse
findMatchingHaskellType :: [(String, String)] -> String -> String
findMatchingHaskellType sqlTypeWrtType sqlType =
  case filter ((sqlType =~) . fst) (haskellTypeWrtSqlType sqlTypeWrtType) of
    [] -> error $ "Type not found " <> sqlType
    ((_, haskellType) : _) -> haskellType

haskellTypeWrtSqlType :: [(String, String)] -> [(String, String)]
haskellTypeWrtSqlType sqlTypeWrtType = map (first (T.unpack . T.replace "(" "\\(" . T.replace ")" "\\)" . T.replace "[" "\\[" . T.replace "]" "\\]" . T.pack) . second (T.unpack . T.replace "\\[" "[" . T.replace "\\]" "]" . T.pack) . swap) sqlTypeWrtType
