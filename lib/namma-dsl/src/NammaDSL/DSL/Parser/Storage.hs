{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module NammaDSL.DSL.Parser.Storage (storageParser, getOldSqlFile, debugParser, runAnyParser, sqlCleanedLineParser, SQL_MANIPULATION) where

import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad (when)
import Control.Monad.Trans.RWS.Lazy
import Data.Aeson
import Data.Aeson.Key (fromString, toString)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Lens (key, _Array, _Object, _Value)
import Data.Bifunctor
import Data.Bool (bool)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import Data.Char (toUpper)
import Data.Default
import Data.Foldable (foldlM)
import Data.List (find, foldl')
import qualified Data.List as L
import qualified Data.List.Extra as L
import Data.List.Split (split, splitOn, splitWhen, whenElt)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, mapMaybe, maybeToList)
import qualified Data.Text as T
import Data.Tuple (swap)
import qualified Data.Tuple.Extra as TE
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
import qualified Text.Parsec as PS
import qualified Text.Parsec.String as PS
import Text.Regex.TDFA ((=~))
import Prelude

parseTableDef :: StorageParserM ()
parseTableDef = do
  parseSqlTableName
  parseExtraTypes
  parseIntermediateTransformers
  parseFields
  parseInstances
  parseDerives
  parseImports
  parseDefaultQueryTypeConstraint
  parseQueries
  parsePrimaryAndSecondaryKeys
  parseRelationalTableNamesHaskell
  parseExtraOperations

parseDefaultQueryTypeConstraint :: StorageParserM ()
parseDefaultQueryTypeConstraint = do
  obj <- gets (dataObject . extraParseInfo)
  let defaultQueryTypeConstraint' = obj ^? ix acc_defaultQueryTypeConstraint . _String
  modify $ \s -> s {tableDef = (tableDef s) {defaultQueryTypeConstraint = defaultQueryTypeConstraint'}}

parseIntermediateTransformers :: StorageParserM ()
parseIntermediateTransformers = do
  obj <- gets (dataObject . extraParseInfo)
  impObj <- gets (.extraParseInfo.yamlObject)
  let mkInterTfs (outputVarName, rawTf) = ITransformer outputVarName (makeTF impObj rawTf)
      toTTypeTfs = fromMaybe [] (obj ^? (ix acc_intermediateTransformers . _Object . ix acc_toTType . _Array . to V.toList . to (concatMap mkList) . to (map mkInterTfs)))
      fromTTypeTfs = fromMaybe [] (obj ^? (ix acc_intermediateTransformers . _Object . ix acc_fromTType . _Array . to V.toList . to (concatMap mkList) . to (map mkInterTfs)))
      interTfs = IntermediateTransformers toTTypeTfs fromTTypeTfs
  modify $ \s -> s {tableDef = (tableDef s) {intermediateTransformers = interTfs}}

parseSqlTableName :: StorageParserM ()
parseSqlTableName = do
  obj <- gets (dataObject . extraParseInfo)
  defaultName <- gets (tableNameSql . tableDef)
  let sqlTableName = fromMaybe defaultName (obj ^? (ix acc_tableName . _String))
  modify $ \s -> s {tableDef = (tableDef s) {tableNameSql = sqlTableName}}

parseExtraTypes :: StorageParserM ()
parseExtraTypes = do
  dList <- gets (.extraParseInfo.dList)
  importObj <- gets (.extraParseInfo.yamlObject)
  moduleName <- gets (.extraParseInfo.domainName)
  defaultImportModule <- asks (.domainTypeModulePrefix)
  defaultTypeImportMap <- asks (.storageDefaultTypeImportMapper)
  parseTypes
  _types <- gets (types . tableDef)
  let mkEnumTypeQualified enumTp =
        FieldType $ L.intercalate "," $ map (uncurry (<>) . second (makeTypeQualified defaultTypeImportMap (Just moduleName) (Just allExcludeQualified) (Just dList) defaultImportModule importObj) . L.breakOn " ") (L.trim <$> L.splitOn "," enumTp.getFieldType)
      mkQualifiedTypeObject = \(TypeObject recType _nm arrOfFields derive overrideDerives) ->
        TypeObject
          recType
          _nm
          ( map
              ( \(_n, _t) ->
                  ( _n,
                    if _n.getFieldName == "enum"
                      then mkEnumTypeQualified _t
                      else FieldType $ makeTypeQualified defaultTypeImportMap (Just moduleName) (Just allExcludeQualified) (Just dList) defaultImportModule importObj _t.getFieldType
                  )
              )
              arrOfFields
          )
          derive
          overrideDerives
      types' = fromMaybe [] _types
      allExcludeQualified = map (\(TypeObject _ name _ _ _) -> name.getTypeName) types'
      allEnums = map (\(TypeObject _ name _ _ _) -> name.getTypeName) $ filter isEnumType types'
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
            isEncryptedHashedField = "EncryptedHashedField" `T.isInfixOf` (T.pack haskellType)
            getDefaultFromTTypeForEncryptedField fn = pure $ makeTF impObj ("EncryptedHashed (Encrypted " <> fn <> "Encrypted) " <> fn <> "Hash|E")
        pure $
          FieldDef
            { fieldName = fieldName,
              haskellType = typeQualifiedHaskellType,
              beamFields = getbeamFields,
              fromTType = maybe (if isEncryptedHashedField then getDefaultFromTTypeForEncryptedField fieldName else if length getbeamFields > 1 then error ("Complex type (" <> fieldName <> ") should have fromTType function") else Nothing) pure parseFromTType,
              isEncrypted = isEncryptedHashedField,
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
  beamInstances <- gets (beamTableInstance . tableDef)
  domainInstances <- gets (domainTableInstance . tableDef)
  typObj <- fromMaybe [] <$> gets (types . tableDef)
  deriveList <- fromMaybe [] <$> gets (derives . tableDef)
  let _imports = figureOutImports (map getInstanceToDerive deriveList <> map haskellType fields <> concatMap figureOutInsideTypeImports typObj <> concatMap (figureOutBeamFieldsImports . beamFields) fields <> figureOutInstancesImports (beamInstances <> domainInstances))
  modify $ \s -> s {tableDef = (tableDef s) {imports = _imports}}
  parseImportPackageOverrides
  where
    figureOutInstancesImports :: [Instance] -> [String]
    figureOutInstancesImports bis =
      filter (not . null) $
        map
          ( \bi -> case bi of
              Custom iname _ _ -> iname
              _ -> mempty
          )
          bis
    figureOutBeamFieldsImports :: [BeamField] -> [String]
    figureOutBeamFieldsImports bms = map bFieldType bms <> map hFieldType bms

    figureOutInsideTypeImports :: TypeObject -> [String]
    figureOutInsideTypeImports tobj@(TypeObject _ _ tps derives _) =
      let isEnum = isEnumType tobj
       in (getInstanceToDerive <$> derives)
            <> concatMap
              ( ( \(FieldType potentialImport) ->
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
      excludedQueries = fromMaybe [] $ obj ^? ix acc_excludedDefaultQueries . _Array . to V.toList . to (map valueToString)
      parseQuery query =
        let queryName = fst query
            queryDataObj = snd query
            params = addDefaultUpdatedAtToQueryParams queryName $ map (first (second makeTypeQualified')) $ fromMaybe [] (queryDataObj ^? ix acc_params . _Array . to V.toList . to (map (searchForKey fields . valueToString)))
            kvFunction = fromMaybe (error "kvFunction is neccessary") (queryDataObj ^? ix acc_kvFunction . _String)
            whereClause = fromMaybe EmptyWhere (queryDataObj ^? ix acc_where . to (parseWhereClause makeTypeQualified' "eq" fields))
            orderBy = fromMaybe defaultOrderBy (queryDataObj ^? ix acc_orderBy . to (parseOrderBy fields))
            takeFullObjectAsParam = DT.traceShowId $ fromMaybe False (queryDataObj ^? ix acc_fullObjectAsParam . _Bool)
            typeConstraint = queryDataObj ^? ix acc_typeConstraint . _String
         in QueryDef queryName kvFunction params whereClause orderBy takeFullObjectAsParam typeConstraint
  case mbQueries of
    Just queries -> modify $ \s -> s {tableDef = (tableDef s) {queries = map parseQuery queries}}
    Nothing -> pure ()
  modify $ \s -> s {tableDef = (tableDef s) {excludedDefaultQueries = excludedQueries}}
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
  let parsedDerives = map InstanceToDerive <$> (obj ^? ix acc_derives . _String . to (L.split (== ',')))
  modify $ \s -> s {tableDef = (tableDef s) {derives = parsedDerives}}

parseInstances :: StorageParserM ()
parseInstances = do
  obj <- gets (dataObject . extraParseInfo)
  let parsedBeamInstance = fromMaybe [MakeTableInstances] $ obj ^? ix acc_beamInstance . (_String . to mkInstance . to pure `failing` _Array . to V.toList . to (map (mkInstance . valueToString)))
      parseDomainInstance = fromMaybe [] $ obj ^? ix acc_domainInstance . (_String . to mkInstance . to pure `failing` _Array . to V.toList . to (map (mkInstance . valueToString)))
  modify $ \s -> s {tableDef = (tableDef s) {beamTableInstance = parsedBeamInstance, domainTableInstance = parseDomainInstance}}

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

sqlCleanedLineParser :: String -> SQL_MANIPULATION
sqlCleanedLineParser line
  | "CREATE TABLE" `L.isPrefixOf` line = SQL_CREATE
  | "ALTER TABLE" `L.isPrefixOf` line = case PS.parse parseAlterTable "" line of
    Right sqlManipulation -> sqlManipulation
    Left errk -> error $ "Error Parsing SQL line : " <> line <> " Error : " <> show errk
  | otherwise = error "Invalid SQL line. Is any of the sql query line not generated ?"
  where
    parseAlterTable :: PS.Parser SQL_MANIPULATION
    parseAlterTable =
      PS.string "ALTER" >> PS.spaces >> PS.string "TABLE" >> PS.spaces >> PS.manyTill PS.anyChar PS.space >> PS.spaces
        >> ( PS.try parseAddColumn
               <||> PS.try parseDropColumn
               <||> PS.try parseAlterColumn
               <||> PS.try parseDropConstraint
               <||> PS.try parseAddPrimaryKey
           )

    parseAddColumn :: PS.Parser SQL_MANIPULATION
    parseAddColumn = do
      colName <- PS.string "ADD" >> PS.spaces >> PS.string "COLUMN" >> PS.spaces >> (PS.manyTill PS.anyChar (PS.try PS.space))
      sqlType <- L.trim <$> (PS.manyTill PS.anyChar (PS.lookAhead $ (PS.try (PS.string "NOT NULL")) <||> (PS.try (PS.string "default")) <||> (PS.try (PS.string ";"))))
      constraint <- fromMaybe [] <$> (PS.optionMaybe $ PS.manyTill parseConstraints (PS.try (PS.string ";")))
      return $ SQL_ALTER $ ADD_COLUMN colName sqlType constraint

    parseConstraints :: PS.Parser ALTER_COLUMN_ACTION
    parseConstraints =
      PS.spaces
        >> PS.try (PS.string "NOT NULL" >> return SET_NOT_NULL)
        <||> PS.try
          ( do
              PS.string "default" >> PS.spaces
              defaultVal <- L.trim <$> PS.manyTill PS.anyChar (PS.lookAhead $ PS.try (PS.string ";") <||> PS.try (PS.string "NOT NULL"))
              return $ SET_DEFAULT defaultVal
          )

    parseDropColumn :: PS.Parser SQL_MANIPULATION
    parseDropColumn = do
      colName <- PS.string "DROP" >> PS.spaces >> PS.string "COLUMN" >> PS.spaces >> PS.manyTill PS.anyChar (PS.string ";")
      return $ SQL_ALTER $ DROP_COLUMN colName

    parseAlterColumn :: PS.Parser SQL_MANIPULATION
    parseAlterColumn = do
      colName <- PS.string "ALTER" >> PS.spaces >> PS.string "COLUMN" >> PS.spaces >> PS.manyTill PS.anyChar PS.space
      alterColumnAction <- PS.spaces >> (PS.try parseChangeType <||> PS.try parseDropDefault <||> PS.try parseSetDefault <||> PS.try parseDropNotNull <||> PS.try parseSetNotNull)
      return $ SQL_ALTER $ ALTER_COLUMN colName alterColumnAction

    parseChangeType :: PS.Parser ALTER_COLUMN_ACTION
    parseChangeType = do
      newType <- L.trim <$> PS.string "TYPE" >> PS.spaces >> PS.manyTill PS.anyChar (PS.string ";")
      return $ CHANGE_TYPE newType

    parseDropDefault :: PS.Parser ALTER_COLUMN_ACTION
    parseDropDefault = PS.string "DROP" >> PS.spaces >> PS.string "DEFAULT" >> return DROP_DEFAULT

    parseSetDefault :: PS.Parser ALTER_COLUMN_ACTION
    parseSetDefault = do
      newDefault <- L.trim <$> PS.string "SET" >> PS.spaces >> PS.string "DEFAULT" >> PS.spaces >> PS.manyTill PS.anyChar (PS.string ";")
      return $ SET_DEFAULT newDefault

    parseDropNotNull :: PS.Parser ALTER_COLUMN_ACTION
    parseDropNotNull = PS.string "DROP" >> PS.spaces >> PS.string "NOT" >> PS.spaces >> PS.string "NULL" >> return DROP_NOT_NULL

    parseSetNotNull :: PS.Parser ALTER_COLUMN_ACTION
    parseSetNotNull = PS.string "SET" >> PS.spaces >> PS.string "NOT" >> PS.spaces >> PS.string "NULL" >> return SET_NOT_NULL

    parseDropConstraint :: PS.Parser SQL_MANIPULATION
    parseDropConstraint = PS.string "DROP" >> PS.spaces >> PS.string "CONSTRAINT" >> return (SQL_ALTER DROP_CONSTRAINT_PKS)

    parseAddPrimaryKey :: PS.Parser SQL_MANIPULATION
    parseAddPrimaryKey = do
      rawPks <- PS.string "ADD" >> PS.spaces >> PS.string "PRIMARY" >> PS.spaces >> PS.string "KEY" >> PS.spaces >> PS.string "(" >> PS.manyTill PS.anyChar (PS.string ")")
      let pks = map L.trim $ L.split (== ',') rawPks
      return $ SQL_ALTER $ ADD_PRIMARY_KEYS pks

migrationFileParserLineByLine :: [(String, String)] -> String -> String -> MigrationFile
migrationFileParserLineByLine _sqlTypeWrtType _dbName lastSqlFile = MigrationFile "" [fieldDeff] allPks [] lastSqlFile
  where
    fieldDeff = def {beamFields = beamFieldsFromSQL}
    allPks = foldl (\acc beamField -> if PrimaryKey `elem` beamField.bConstraints then beamField.bFieldName : acc else acc) [] beamFieldsFromSQL
    beamFieldsFromSQL = M.elems $ foldl (\acc sqlManipulation -> sqlManipulationApply acc sqlManipulation) M.empty allSqlManipulations
    allSqlManipulations = map sqlCleanedLineParser cleanSQLines
    cleanSQLines = filter (\x -> not $ null x || "---" `L.isPrefixOf` x) $ lines lastSqlFile

sqlManipulationApply :: Map String BeamField -> SQL_MANIPULATION -> Map String BeamField
sqlManipulationApply mp = \case
  SQL_CREATE -> mp
  SQL_ALTER sqlAlter -> sqlAlterApply mp sqlAlter

sqlAlterApply :: Map String BeamField -> SQL_ALTER -> Map String BeamField
sqlAlterApply mp = \case
  ADD_COLUMN colName sqlType constraints ->
    let newMp = M.insert (snakeCaseToCamelCase $ removeQuoteWrap colName) (def {bFieldName = snakeCaseToCamelCase $ removeQuoteWrap colName, bSqlType = sqlType}) mp
     in foldl (\acc constraint -> alterColumnApply acc colName constraint) newMp constraints
  DROP_COLUMN colName -> M.delete (snakeCaseToCamelCase $ removeQuoteWrap colName) mp
  ALTER_COLUMN colName action -> alterColumnApply mp colName action
  DROP_CONSTRAINT_PKS -> M.map (\bf -> bf {bConstraints = filter (/= PrimaryKey) bf.bConstraints}) mp
  ADD_PRIMARY_KEYS pks -> foldl (\acc pkField -> M.adjust (\bf -> bf {bConstraints = PrimaryKey : bf.bConstraints}) (snakeCaseToCamelCase pkField) acc) mp pks

alterColumnApply :: Map String BeamField -> String -> ALTER_COLUMN_ACTION -> Map String BeamField
alterColumnApply mp colName = \case
  SET_NOT_NULL -> M.adjust (\bf -> bf {bConstraints = NotNull : bf.bConstraints}) (snakeCaseToCamelCase $ removeQuoteWrap colName) mp
  DROP_NOT_NULL -> M.adjust (\bf -> bf {bConstraints = filter (/= NotNull) bf.bConstraints}) (snakeCaseToCamelCase $ removeQuoteWrap colName) mp
  SET_DEFAULT val -> M.adjust (\bf -> bf {bDefaultVal = Just val}) (snakeCaseToCamelCase $ removeQuoteWrap colName) mp
  DROP_DEFAULT -> M.adjust (\bf -> bf {bDefaultVal = Nothing}) (snakeCaseToCamelCase $ removeQuoteWrap colName) mp
  CHANGE_TYPE newType -> M.adjust (\bf -> bf {bSqlType = newType}) (snakeCaseToCamelCase $ removeQuoteWrap colName) mp

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
      pure $ Just $ migrationFileParserLineByLine sqlTypeWrtType dbName (BSU.toString lastSqlFile)
    else -- go (runParser (migrationFileParser sqlTypeWrtType dbName (BSU.toString lastSqlFile)) $ cleanedFile lastSqlFile)
      pure Nothing
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
                  takeFullObjectAsInput = False,
                  typeConstraint = Nothing
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
                  takeFullObjectAsInput = False,
                  typeConstraint = Nothing
                }
            ]
      TableDef {fields = fields <> [foreignField], queries = queries <> query, ..}
    Nothing -> TableDef {..}

mkInstance :: String -> Instance
mkInstance rw =
  case instanceName of
    "MakeTableInstances" -> MakeTableInstances
    "MakeTableInstancesGenericSchema" -> MakeTableInstancesGenericSchema
    "MakeTableInstancesWithTModifier" -> MakeTableInstancesWithTModifier remArgs'
    "Custom" ->
      let (customName, remArgs) = TE.both L.trim (L.break (== ' ') remArgs')
          (potentialDataName, remExtraParams) = TE.both L.trim $ L.break (== ' ') remArgs
          (dataName, extraParams) = if "<" `L.isPrefixOf` potentialDataName && ">" `L.isSuffixOf` potentialDataName then ((pure . init . tail) potentialDataName, remExtraParams) else (Nothing, remArgs)
       in Custom customName dataName extraParams
    _ -> error $ "Unknow Beam Instance " <> instanceName
  where
    (instanceName, remArgs') = TE.both L.trim (L.break (== ' ') rw)

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

    splitTypeAndDerivation :: [(String, String)] -> ([(FieldName, FieldType)], [InstanceToDerive], OverrideDefaultDerive)
    splitTypeAndDerivation fields = (bimap FieldName FieldType <$> filter (\(k, _) -> not $ k `elem` ["derive", "recordType", "derive'"]) fields, extractDerive fields, overrideDerives)
      where
        overrideDerives = any (\(nm, _) -> nm == "derive'") fields
        extractDerive :: [(String, String)] -> [InstanceToDerive]
        extractDerive [] = []
        extractDerive ((k, value) : xs)
          | (k == "derive" || k == "derive'") = map (InstanceToDerive . T.unpack) (T.split (== ',') (T.pack value))
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
    processType (typeName, Object typeDef) = do
      let (fields, derivations, overrideDerives) = splitTypeAndDerivation $ extractFields typeDef
      TypeObject (extractRecordType typeDef) (TypeName $ toString typeName) fields derivations overrideDerives
    processType _ = error "Expected an object in fields"

beamFieldsWithExtractors :: String -> String -> [String] -> StorageParserM [(String, String, [String])]
beamFieldsWithExtractors fieldName haskellType extractorFuncs = do
  moduleName <- gets (.extraParseInfo.domainName)
  domainTypeModulePrefix <- asks (.domainTypeModulePrefix)
  definedTypes <- (fromMaybe []) <$> gets (.tableDef.types)
  obj <- gets (.extraParseInfo.dataObject)
  let beamFieldObj = obj ^? (ix acc_beamFields . _Object)
      qualified tp = domainTypeModulePrefix ++ "." ++ moduleName ++ "." ++ tp
      findIfComplexType tpp = find (\(TypeObject _ (TypeName nm) arrOfFields _ _) -> (nm == tpp || tpp == domainTypeModulePrefix ++ "." ++ moduleName ++ "." ++ nm) && all (\(FieldName k, _) -> k /= "enum") arrOfFields) definedTypes
  case beamFieldObj >>= preview (ix (fromString fieldName) . _Object . to Object . to mkList) of
    Just arrOfFields ->
      pure $ foldl (\acc (nm, tpp) -> acc ++ [(nm, tpp, [])]) [] arrOfFields
    Nothing ->
      case findIfComplexType haskellType of
        Just (TypeObject _ _nm arrOfFields _ _) -> do
          foldlM
            ( \acc (FieldName nm, FieldType tpp) -> do
                bFieldWithExt <- beamFieldsWithExtractors (fieldName ++ capitalise nm) tpp (qualified nm : extractorFuncs)
                pure $ acc ++ bFieldWithExt
            )
            []
            arrOfFields
        Nothing ->
          pure [(fromMaybe fieldName (beamFieldObj >>= preview (ix (fromString fieldName) . _String)), haskellType, extractorFuncs)]
  where
    capitalise :: String -> String
    capitalise [] = []
    capitalise (c : cs) = toUpper c : cs

makeTF :: Object -> String -> TransformerFunction
makeTF impObj func =
  TransformerFunction
    { tfName = qualifiedName,
      tfType = tfType',
      tfIsEmbeddedArgs = isEmbeddedArgs
    }
  where
    (name, details) = L.break (== '|') func
    tfType' = if 'M' `L.elem` details then MonadicT else PureT
    isImported = 'I' `L.elem` details || '.' `L.elem` name || 'E' `L.elem` details
    isEmbeddedArgs = 'E' `L.elem` details
    qualifiedName =
      if isImported
        then
          if '.' `L.elem` name || isEmbeddedArgs
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
  let isEncryptedHashedField = "EncryptedHashedField" `T.isInfixOf` (T.pack haskellType)
  let constraintsObj = obj ^? (ix acc_constraints . _Object)
      sqlTypeObj = obj ^? (ix acc_sqlType . _Object)
      beamTypeObj = obj ^? (ix acc_beamType ._Object)
      defaultsObj = obj ^? (ix acc_default . _Object)
  if isEncryptedHashedField && not (has (ix acc_beamFields . _Object . ix (fromString fieldName) . _Object) obj)
    then do
      -- its a excrypted field and needs default beam fields --
      let isFieldMaybeType = isMaybeType haskellType
          bEncryptedFieldName = fieldName ++ "Encrypted"
          bHashFieldName = fieldName ++ "Hash"
          bEncryptedFieldType = bool "Text" "Maybe Text" isFieldMaybeType
          bHashFieldType = bool "Kernel.External.Encryption.DbHash" "Maybe Kernel.External.Encryption.DbHash" isFieldMaybeType
          qType = makeTypeQualified defaultTypeImportMap (Just moduleName) (Just excludedList) (Just dataList) defaultImportModule impObj
          getToTType fn = obj ^? (ix acc_toTType . _Object) >>= preview (ix (fromString fn) . _String . to (makeTF impObj))
          bEncryptedFieldDefaultToTType = TransformerFunction ("(" <> fieldName <> bool " & " " <&> " (isMaybeType haskellType) <> "unEncrypted . encrypted)") PureT True
          bHashFieldDefaultToTType = TransformerFunction ("(" <> fieldName <> bool " & " " <&> " (isMaybeType haskellType) <> "hash)") PureT True
      pure $
        [ BeamField
            { bFieldName = bEncryptedFieldName,
              hFieldType = qType haskellType,
              bFieldType = qType bEncryptedFieldType,
              bConstraints = getDefaultFieldConstraints bEncryptedFieldName bEncryptedFieldType ++ fromMaybe [] (constraintsObj >>= preview (ix (fromString bEncryptedFieldName) . _String . to (splitOn "|") . to (map getProperConstraint))),
              bFieldUpdates = [], -- not required while creating
              bSqlType = "character varying(255)",
              bDefaultVal = obj ^? (ix acc_default . _Object . ix (fromString bEncryptedFieldName) . _String),
              bToTType = maybe (pure bEncryptedFieldDefaultToTType) pure (getToTType bEncryptedFieldName),
              bfieldExtractor = [],
              bIsEncrypted = True
            },
          BeamField
            { bFieldName = bHashFieldName,
              hFieldType = qType haskellType,
              bFieldType = qType bHashFieldType,
              bConstraints = getDefaultFieldConstraints bHashFieldName bHashFieldType ++ fromMaybe [] (constraintsObj >>= preview (ix (fromString bHashFieldName) . _String . to (splitOn "|") . to (map getProperConstraint))),
              bFieldUpdates = [], -- not required while creating
              bSqlType = "bytea",
              bDefaultVal = obj ^? (ix acc_default . _Object . ix (fromString bHashFieldName) . _String),
              bToTType = maybe (pure bHashFieldDefaultToTType) pure (getToTType bHashFieldName),
              bfieldExtractor = [],
              bIsEncrypted = True
            }
        ]
    else do
      extractedBeamInfos <- beamFieldsWithExtractors fieldName haskellType []
      let getBeamFieldDef (fName, tpp, extractorFuncs) =
            let fieldKey = fromString fName
                beamType = fromMaybe (findBeamType tpp) (beamTypeObj >>= preview (ix fieldKey . _String))
                sqlType = fromMaybe (findMatchingSqlType sqlTypeMapper enumList tpp beamType) (sqlTypeObj >>= preview (ix fieldKey . _String))
                defaultValue = maybe (sqlDefaultsWrtName fName) pure (defaultsObj >>= preview (ix fieldKey . _String))
                parseToTType = obj ^? (ix acc_toTType . _Object) >>= preview (ix fieldKey . _String . to (makeTF impObj))
                constraints = L.nub $ getDefaultFieldConstraints fName beamType ++ fromMaybe [] (constraintsObj >>= preview (ix fieldKey . _String . to (splitOn "|") . to (map getProperConstraint)))
                isEncrypted = "EncryptedHashedField" `T.isInfixOf` T.pack tpp
             in BeamField
                  { bFieldName = fName,
                    hFieldType = makeTypeQualified defaultTypeImportMap (Just moduleName) (Just excludedList) (Just dataList) defaultImportModule impObj haskellType,
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
isEnumType (TypeObject _ _ arrOfFields _ _) = any (\(FieldName k, _) -> k == "enum") arrOfFields

-- SQL reverse parse
findMatchingHaskellType :: [(String, String)] -> String -> String
findMatchingHaskellType sqlTypeWrtType sqlType =
  case filter ((sqlType =~) . fst) (haskellTypeWrtSqlType sqlTypeWrtType) of
    [] -> error $ "Type not found " <> sqlType
    ((_, haskellType) : _) -> haskellType

haskellTypeWrtSqlType :: [(String, String)] -> [(String, String)]
haskellTypeWrtSqlType sqlTypeWrtType = map (first (T.unpack . T.replace "(" "\\(" . T.replace ")" "\\)" . T.replace "[" "\\[" . T.replace "]" "\\]" . T.pack) . second (T.unpack . T.replace "\\[" "[" . T.replace "\\]" "]" . T.pack) . swap) sqlTypeWrtType
