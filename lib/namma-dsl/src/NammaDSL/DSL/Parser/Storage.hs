{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module NammaDSL.DSL.Parser.Storage (storageParser, getOldSqlFile, debugParser, runAnyParser, sqlCleanedLineParser, SQL_MANIPULATION) where

import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad (unless, when)
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
import Data.List (find, foldl', nub)
import qualified Data.List as L
import qualified Data.List.Extra as L
import Data.List.Split (split, splitOn, splitWhen, whenElt)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
import Data.Set (Set)
import qualified Data.Set as S
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
import Safe (headMay)
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
  parseCachedQueries
  parsePrimaryAndSecondaryKeys
  parseRelationalTableNamesHaskell
  parseExtraOperations
  parseExtraIndexes

parseExtraIndexes :: StorageParserM ()
parseExtraIndexes = do
  obj <- gets (dataObject . extraParseInfo)
  tableName <- gets (tableNameSql . tableDef)
  isGenerationAllowed <- gets ((GENERATE_INDEXES `elem`) . extraOperations . tableDef)
  isDefaultIndexingStopped <- gets ((NO_DEFAULT_INDEXES `elem`) . extraOperations . tableDef)
  defaultColForIndexing <- gets (map bFieldName . filter (\bf -> SecondaryKey `elem` bConstraints bf) . concatMap beamFields . fields . tableDef)
  let defaultIndexes = map (\col -> mkIndexDef tableName [col] False Nothing) defaultColForIndexing
  let rawExtraIndexes = fromMaybe [] $ obj ^? ix acc_extraIndexes . _Array . to V.toList
  parsedExtraIndexes <- mapM parseExtraIndex rawExtraIndexes
  let finalIndex =
        nub $
          if isGenerationAllowed && not isDefaultIndexingStopped
            then defaultIndexes <> parsedExtraIndexes
            else
              if isGenerationAllowed && isDefaultIndexingStopped
                then parsedExtraIndexes
                else mempty
  modify $ \s -> s {tableDef = (tableDef s) {indexes = finalIndex}}

mkIndexDef :: String -> [String] -> Bool -> Maybe String -> IndexDef
mkIndexDef tableName cols isUnique name' = IndexDef name (S.fromList cols) isUnique
  where
    name = fromMaybe (L.intercalate "_" ([tableName] <> ["unique" | isUnique] <> ("idx" : map quietSnake (cols & L.sort)))) name'

parseExtraIndex :: Value -> StorageParserM IndexDef
parseExtraIndex obj = do
  fields <- gets (fields . tableDef)
  tableName <- gets (tableNameSql . tableDef)
  let bFields = concatMap beamFields fields
      bFieldNames = map bFieldName bFields
      indexDef = case obj of
        String _ ->
          let cols = [valueToString obj]
           in mkIndexDef tableName cols False Nothing
        Object _ ->
          let cols = fromMaybe [] $ obj ^? ix acc_columns . _Array . to V.toList . to (map valueToString)
              isUnique = fromMaybe False $ obj ^? ix acc_unique . _Bool
              name' = obj ^? ix acc_name . _String
           in mkIndexDef tableName cols isUnique name'
        Array _ ->
          let cols = fromMaybe [] $ obj ^? _Array . to V.toList . to (map valueToString)
           in mkIndexDef tableName cols False Nothing
        _ -> error "Invalid Index Definition"
  unless (all (`elem` bFieldNames) (indexColumns indexDef)) $ throwError (InternalError $ "Invalid Column Name in Index. Please ensure beam side name is used " <> show (indexColumns indexDef))
  pure indexDef

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
        FieldType $ L.intercalate "," $ map (uncurry (<>) . second (makeTypeQualified defaultTypeImportMap (Just moduleName) (Just allExcludeQualified) (Just dList) (Just defaultImportModule) importObj) . L.breakOn " ") (L.trim <$> L.splitOn "," enumTp.getFieldType)
      mkQualifiedTypeObject = \(TypeObject recType _nm arrOfFields derive overrideDerives) ->
        TypeObject
          recType
          _nm
          ( map
              ( \(_n, _t) ->
                  ( _n,
                    if _n.getFieldName == "enum"
                      then mkEnumTypeQualified _t
                      else FieldType $ makeTypeQualified defaultTypeImportMap (Just moduleName) (Just allExcludeQualified) (Just dList) (Just defaultImportModule) importObj _t.getFieldType
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
        let typeQualifiedHaskellType = makeTypeQualified defaultTypeImportMap (Just moduleName) excludedList (Just dataList) (Just defaultImportModule) impObj haskellType
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

parseCachedQueries :: StorageParserM ()
parseCachedQueries = do
  moduleName <- gets (.extraParseInfo.domainName)
  excludedList <- gets (.extraParseInfo.excludedImportList)
  dList <- gets (.extraParseInfo.dList)
  fields <- gets (.tableDef.fields)
  impObj <- gets (.extraParseInfo.yamlObject)
  obj <- gets (.extraParseInfo.dataObject)
  defaultImportModule <- asks (.domainTypeModulePrefix)
  defaultTypeImportMap <- asks (.storageDefaultTypeImportMapper)
  let makeTypeQualified' = makeTypeQualified defaultTypeImportMap (Just moduleName) (Just excludedList) (Just dList) (Just defaultImportModule) impObj
      rawCachedQueries = obj ^? ix acc_cachedQueries . _Value . to mkListObject
      parseKeyParam paramValue = case paramValue of
        pObj@(Object _) ->
          let (paramKey, paramType) = head $ mkList pObj
           in Variable paramKey (makeTypeQualified' paramType)
        pStr@(String _) ->
          let paramKey = valueToString pStr
           in if "|C" `L.isInfixOf` paramKey
                then parseConstantParam paramKey
                else Variable paramKey (haskellType $ fromMaybe (error "Param Key not found in fields") $ find (\f -> (fieldName f) == paramKey) fields)
        _ -> error "Invalid Param Type"
      parseCachedQuery cquery =
        let cQueryName = fst cquery
            cqueryObj = snd cquery
            withCrossAppRedis = fromMaybe False (cqueryObj ^? ix acc_withCrossAppRedis ._Bool)
            ctypeConstraint = cqueryObj ^? ix acc_typeConstraint . _String
            cacheDataType = fromMaybe (COne) $ cqueryObj ^? (ix acc_cacheDataType . _String `failing` ix acc_returnType . _String) . to parseReturnType
            keyMaker = cqueryObj ^? ix acc_keyMaker . _String
            keyParams = fromMaybe (error "Key Params for cached query is missing") (cqueryObj ^? ix acc_keyParams . _Array . to V.toList . to (map parseKeyParam))
            dbQuery = fromMaybe (fst cquery) (cqueryObj ^? ix acc_dbQuery . _String)
            dbQueryParams = fromMaybe [] (cqueryObj ^? ix acc_dbQueryParams . _Array . to V.toList . to (map parseKeyParam))
            paramsOrder = cqueryObj ^? ix acc_paramsOrder . _Array . to V.toList . to (map valueToString)
            cQueryType = fromMaybe (figureCQueryType cQueryName) $ cqueryObj ^? ix acc_queryType . _String . to parseCQueryType
         in CachedQueryDef {..}
  case rawCachedQueries of
    Just cqueries -> modify $ \s -> s {tableDef = (tableDef s) {cachedQueries = map parseCachedQuery cqueries}}
    Nothing -> pure ()
  where
    figureCQueryType :: String -> CQueryType
    figureCQueryType cQueryName
      | "cache" `L.isPrefixOf` cQueryName = CacheOnly
      | "delete" `L.isPrefixOf` cQueryName = DeleteCache
      | "clear" `L.isPrefixOf` cQueryName = DeleteCache
      | otherwise = FindAndCache

    parseCQueryType :: String -> CQueryType
    parseCQueryType = \case
      "FindAndCache" -> FindAndCache
      "FindOnly" -> FindOnly
      "CacheOnly" -> CacheOnly
      "DeleteCache" -> DeleteCache
      _ -> error "Invalid CQueryType"

    parseReturnType :: String -> CQReturnType
    parseReturnType = \case
      "Array" -> CArray
      "One" -> COne
      _ -> error "Invalid ReturnType"

makeQueryParam :: [FieldDef] -> [BeamField] -> Value -> QueryParam
makeQueryParam fds bfs val = case val of
  st@(String _) ->
    let (fd, extInf) = getValAndExt (valueToString st)
     in if extInf == "B" -- Beam Case
          then
            let bf = fromMaybe (error "Beam Field not found") $ find (\b -> bFieldName b == fd) bfs
             in QueryParam {qpName = fd, qpType = bFieldType bf, qpExtParam = Just (Variable (fd ++ "Beam") (bFieldType bf)), qpIsBeam = True}
          else -- Normal Case

            let ((_, fldType), _) = searchForKey fds fd
             in QueryParam {qpName = fd, qpType = fldType, qpExtParam = Nothing, qpIsBeam = False}
  rawC@(Object _) ->
    let (fd, sval) = fromMaybe (error "Invalid Query Param") $ headMay (mkList rawC)
     in if ("|C" `L.isInfixOf` sval)
          then QueryParam {qpName = fd, qpType = mempty, qpExtParam = Just (parseConstantParam sval), qpIsBeam = True}
          else
            let (fdR, extInf) = getValAndExt sval
             in if extInf == "B" -- Beam Case
                  then
                    let bf = fromMaybe (error "Beam Field not found") $ find (\b -> bFieldName b == fd) bfs
                     in QueryParam {qpName = fd, qpType = bFieldType bf, qpExtParam = Just (Variable fdR (bFieldType bf)), qpIsBeam = True}
                  else -- Normal Case

                    let ((_, fldType), _) = searchForKey fds fd
                     in QueryParam {qpName = fd, qpType = fldType, qpExtParam = Just (Variable fdR fldType), qpIsBeam = False}
  _ -> error "Not a proper QueryParam Definition"
  where
    getValAndExt :: String -> (String, String)
    getValAndExt prm =
      case L.splitOn "|" prm of
        [val_] -> (val_, "")
        [val_, ext] -> (val_, ext)
        _ -> error ("Invalid Extension Type Expression " <> show prm)

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
  let bFields = concatMap beamFields fields
      _makeTypeQualified' = makeTypeQualified defaultTypeImportMap (Just moduleName) (Just excludedList) (Just dList) (Just defaultImportModule) impObj
      mbQueries = obj ^? ix acc_queries . _Value . to mkListObject
      excludedQueries = fromMaybe [] $ obj ^? ix acc_excludedDefaultQueries . _Array . to V.toList . to (map valueToString)
      parseQuery query =
        let queryName = fst query
            queryDataObj = snd query
            kvFunction = fromMaybe (error "kvFunction is neccessary") (queryDataObj ^? ix acc_kvFunction . _String)
            params = addDefaultUpdatedAtToQueryParams kvFunction fields $ fromMaybe [] (queryDataObj ^? ix acc_params . _Array . to V.toList . to (map (makeQueryParam fields bFields)))
            whereClause = fromMaybe EmptyWhere (queryDataObj ^? ix acc_where . to (parseWhereClause (makeQueryParam fields bFields) "eq"))
            orderBy = queryDataObj ^? ix acc_orderBy . to (parseOrderBy fields)
            takeFullObjectAsParam = fromMaybe False (queryDataObj ^? ix acc_fullObjectAsParam . _Bool)
            typeConstraint = queryDataObj ^? ix acc_typeConstraint . _String
         in QueryDef queryName kvFunction params whereClause orderBy takeFullObjectAsParam typeConstraint
  case mbQueries of
    Just queries -> modify $ \s -> s {tableDef = (tableDef s) {queries = map parseQuery queries}}
    Nothing -> pure ()
  modify $ \s -> s {tableDef = (tableDef s) {excludedDefaultQueries = excludedQueries}}
  where
    addDefaultUpdatedAtToQueryParams :: String -> [FieldDef] -> [QueryParam] -> [QueryParam]
    addDefaultUpdatedAtToQueryParams kvFunctionName fields params =
      if "update" `L.isPrefixOf` kvFunctionName
        then
          let searchUpdatedAtType = haskellType <$> find (("updatedAt" ==) . fieldName) fields
           in maybe
                params
                ( \hType ->
                    if any (\(QueryParam k _ _ _) -> k == "updatedAt") params
                      then params
                      else params <> [QueryParam {qpName = "updatedAt", qpType = hType, qpExtParam = Nothing, qpIsBeam = False}]
                )
                searchUpdatedAtType
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
  srcStatus <- asks srcFileStatus
  fields <- gets (fields . tableDef)
  queries <- gets (queries . tableDef)
  allFieldsUsedInQueries <- getAllFieldsUsedInQueries queries
  let (primaryKey, secondaryKey) = extractKeys allFieldsUsedInQueries fields
  when ((not $ null $ L.intersect secondaryKey notAllowedSecondaryKeys) && srcStatus `elem` [NEW, CHANGED]) $ throwError $ InternalError "Secondary Key should not contain merchantId, merchantOperatingCityId, status"
  modify $ \s -> s {tableDef = (tableDef s) {primaryKey = primaryKey, secondaryKey = secondaryKey}}
  where
    extractKeys :: [String] -> [FieldDef] -> ([String], [String])
    extractKeys possKeys fieldDefs = extractKeysFromBeamFields possKeys (concatMap beamFields fieldDefs)

    extractKeysFromBeamFields :: [String] -> [BeamField] -> ([String], [String])
    extractKeysFromBeamFields possKeys fieldDefs = (primaryKeyFields, secondaryKeyFields)
      where
        primaryKeyFields = [bFieldName fd | fd <- fieldDefs, PrimaryKey `elem` bConstraints fd]
        secondaryKeyFields = map bFieldName filterFieldForSecondaryKey
        filterFieldForSecondaryKey = filter secondaryKeyCondition fieldDefs
        secondaryKeyCondition bf
          | (Forced SecondaryKey) `elem` bConstraints bf = True
          | (SecondaryKey `elem` bConstraints bf) && (bFieldName bf `elem` possKeys) = True
          | (SecondaryKey `elem` bConstraints bf) && (not $ bFieldName bf `elem` possKeys) = error ("SecondaryKey constaint for beam field " ++ bFieldName bf ++ " cannot be applied as it's not used in src-read-only query section.\nIf you want to use this field or are using it in a query file outside of the src-read-only folder, you can force it with !SecondaryKey.\nAlert: If you are adding this constraint, please ensure you are aware of its cardinality and use cases.")
          | otherwise = False

notAllowedSecondaryKeys :: [String]
notAllowedSecondaryKeys = ["merchantId", "merchantOperatingCityId", "status"]

parseRelationalTableNamesHaskell :: StorageParserM ()
parseRelationalTableNamesHaskell = do
  fields <- gets (fields . tableDef)
  let relationalTableNamesHaskell = catMaybes $ map (.relationalTableNameHaskell) fields
  modify $ \s -> s {tableDef = (tableDef s) {relationalTableNamesHaskell = relationalTableNamesHaskell}}

storageParser :: StorageRead -> FilePath -> IO [TableDef]
storageParser storageRead filepath = do
  contents <- BS.readFile filepath
  case Yaml.decodeEither' contents of
    Left _ -> error $ "Not a Valid Yaml: " <> filepath
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
  let snakeKeys = map (snakeCaseToCamelCase . removeQuoteWrap . T.unpack . T.strip) (T.split (== ',') . T.pack $ DT.traceShowId $ keys)
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
  | "CREATE INDEX" `L.isPrefixOf` line = case PS.parse parseCreateIndex "" line of
    Right sqlManipulation -> sqlManipulation
    Left errk -> error $ "Error Parsing SQL line : " <> line <> " Error : " <> show errk
  | "ALTER TABLE" `L.isPrefixOf` line = case PS.parse parseAlterTable "" line of
    Right sqlManipulation -> sqlManipulation
    Left errk -> error $ "Error Parsing SQL line : " <> line <> " Error : " <> show errk
  | "DROP INDEX" `L.isPrefixOf` line = case PS.parse parseDropIndex "" line of
    Right sqlManipulation -> sqlManipulation
    Left errk -> error $ "Error Parsing SQL line : " <> line <> " Error : " <> show errk
  | otherwise = error "Invalid SQL line. Is any of the sql query line not generated ?"
  where
    parseDropIndex :: PS.Parser SQL_MANIPULATION
    parseDropIndex = do
      PS.string "DROP INDEX" >> PS.spaces
      indexName <- PS.manyTill PS.anyChar (PS.string ";")
      return $ SQL_ALTER (DROP_CONSTRAINT indexName)
    parseCreateIndex :: PS.Parser SQL_MANIPULATION
    parseCreateIndex = do
      PS.string "CREATE INDEX" >> PS.spaces
      indexName <- PS.manyTill PS.anyChar PS.space
      PS.spaces >> PS.string "ON" >> PS.spaces
      _tableName <- PS.manyTill PS.anyChar PS.space
      PS.spaces >> PS.string "USING" >> PS.spaces >> PS.string "btree" >> PS.spaces
      PS.string "(" >> PS.spaces
      rawColNames <- PS.manyTill PS.anyChar (PS.string ");")
      let colNames = map (snakeCaseToCamelCase . removeQuoteWrap . L.trim) $ L.split (== ',') rawColNames
      return $ SQL_ALTER (ADD_CONSTRAINT indexName colNames False)

    parseAlterTable :: PS.Parser SQL_MANIPULATION
    parseAlterTable =
      PS.string "ALTER" >> PS.spaces >> PS.string "TABLE" >> PS.spaces >> PS.manyTill PS.anyChar PS.space >> PS.spaces
        >> ( PS.try parseAddColumn
               <||> PS.try parseDropColumn
               <||> PS.try parseAlterColumn
               <||> PS.try parseDropConstraint
               <||> PS.try parseAddPrimaryKey
               <||> PS.try parseAddUniqueIndex
           )

    -- ALTER TABLE atlas_app.app_dynamic_logic_element ADD CONSTRAINT unique_idx_domain_version UNIQUE (domain, version);
    parseAddUniqueIndex :: PS.Parser SQL_MANIPULATION
    parseAddUniqueIndex = do
      uniqueIdxName <- PS.string "ADD" >> PS.spaces >> PS.string "CONSTRAINT" >> PS.spaces >> PS.manyTill PS.anyChar PS.space
      rawColNames <- PS.spaces >> PS.string "UNIQUE" >> PS.spaces >> PS.string "(" >> PS.manyTill PS.anyChar (PS.string ");")
      let colNames = map (snakeCaseToCamelCase . removeQuoteWrap . L.trim) $ L.split (== ',') rawColNames
      return $ SQL_ALTER (ADD_CONSTRAINT uniqueIdxName colNames True)

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
    parseDropConstraint = do
      constaintName <- PS.string "DROP" >> PS.spaces >> PS.string "CONSTRAINT" >> PS.spaces >> PS.manyTill PS.anyChar (PS.string ";")
      if "pkey" `L.isSuffixOf` constaintName
        then return (SQL_ALTER DROP_CONSTRAINT_PKS)
        else return (SQL_ALTER $ DROP_CONSTRAINT (L.trim constaintName))

    parseAddPrimaryKey :: PS.Parser SQL_MANIPULATION
    parseAddPrimaryKey = do
      rawPks <- PS.string "ADD" >> PS.spaces >> PS.string "PRIMARY" >> PS.spaces >> PS.string "KEY" >> PS.spaces >> PS.string "(" >> PS.manyTill PS.anyChar (PS.string ")")
      let pks = map (removeQuoteWrap . L.trim) $ L.split (== ',') rawPks
      return $ SQL_ALTER $ ADD_PRIMARY_KEYS pks

migrationFileParserLineByLine :: [(String, String)] -> String -> String -> MigrationFile
migrationFileParserLineByLine _sqlTypeWrtType _dbName lastSqlFile = MigrationFile "" [fieldDeff] allPks allSecKeys pastIndexes lastSqlFile
  where
    fieldDeff = def {beamFields = beamFieldsFromSQL}
    allPks = foldl (\acc beamField -> if PrimaryKey `elem` beamField.bConstraints then beamField.bFieldName : acc else acc) [] beamFieldsFromSQL
    beamFieldsFromSQL = M.elems $ foldl (\acc sqlManipulation -> sqlManipulationApply acc sqlManipulation) M.empty allSqlManipulations
    (pastIndexes, allSecKeys) = checkIndexStatus allSqlManipulations
    allSqlManipulations = map sqlCleanedLineParser cleanSQLines
    cleanSQLines = filter (\x -> not $ null x || "---" `L.isPrefixOf` x) $ lines lastSqlFile

checkIndexStatus :: [SQL_MANIPULATION] -> (Set IndexDef, [String])
checkIndexStatus manipulations = (S.fromList allIndexes, allSecKeys)
  where
    allSecKeys = map (head . S.toList . indexColumns) $ filter (\indexDef -> (not $ indexUnique indexDef) && (S.size $ indexColumns indexDef) == 1) allIndexes
    allIndexes =
      M.elems $
        if null manipulations
          then M.empty
          else
            foldl'
              ( \accIndex sqlManipulation -> case sqlManipulation of
                  SQL_ALTER sqlAlter -> case sqlAlter of
                    ADD_CONSTRAINT indexName colNames isUnique ->
                      M.insert indexName (IndexDef indexName (S.fromList colNames) isUnique) accIndex
                    DROP_CONSTRAINT keyName -> M.delete keyName accIndex
                    _ -> accIndex
                  _ -> accIndex
              )
              M.empty
              manipulations

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
  _ -> mp

alterColumnApply :: Map String BeamField -> String -> ALTER_COLUMN_ACTION -> Map String BeamField
alterColumnApply mp colName = \case
  SET_NOT_NULL -> M.adjust (\bf -> bf {bConstraints = NotNull : bf.bConstraints}) (snakeCaseToCamelCase $ removeQuoteWrap colName) mp
  DROP_NOT_NULL -> M.adjust (\bf -> bf {bConstraints = filter (/= NotNull) bf.bConstraints}) (snakeCaseToCamelCase $ removeQuoteWrap colName) mp
  SET_DEFAULT val -> M.adjust (\bf -> bf {bDefaultVal = Just val}) (snakeCaseToCamelCase $ removeQuoteWrap colName) mp
  DROP_DEFAULT -> M.adjust (\bf -> bf {bDefaultVal = Nothing}) (snakeCaseToCamelCase $ removeQuoteWrap colName) mp
  CHANGE_TYPE newType -> M.adjust (\bf -> bf {bSqlType = newType}) (snakeCaseToCamelCase $ removeQuoteWrap colName) mp

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
                      ( QueryParam {qpName = fieldName, qpType = haskellType, qpExtParam = Nothing, qpIsBeam = False},
                        Just Eq
                      ),
                  orderBy = Just defaultOrderBy,
                  takeFullObjectAsInput = False,
                  typeConstraint = Nothing
                },
              QueryDef
                { queryName = "findBy" ++ foreignTableNameHaskell ++ "Id",
                  kvFunction = "findOneWithKV",
                  params = [],
                  whereClause =
                    Leaf
                      ( QueryParam {qpName = fieldName, qpType = haskellType, qpExtParam = Nothing, qpIsBeam = False},
                        Just Eq
                      ),
                  orderBy = Just defaultOrderBy,
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

-- todo: makeTypeArrayForInOperator
parseWhereClause :: (Value -> QueryParam) -> String -> Value -> WhereClause
parseWhereClause mkQueryParam operatorStr val@(String _) = do
  let op_ = parseOperator (T.unpack (T.toLower (T.pack operatorStr)))
  Leaf ((mkQueryParam val), Just op_)
parseWhereClause mkQueryParam lastOp val@(Object clauseObj) = do
  let clauseObj' = KM.toList clauseObj
  case clauseObj' of
    [(operatorStr, value)] -> do
      let op_ = if parseOperator (T.unpack (T.toLower (T.pack (toString operatorStr)))) `elem` comparisonOperator then toString operatorStr else lastOp
      case value of
        Array arr_ -> do
          let clauses = map (parseWhereClause mkQueryParam op_) (V.toList arr_)
          Query (parseOperator (toString operatorStr), clauses)
        String _ -> do
          Leaf ((mkQueryParam val), Just (parseOperator lastOp))
        _ -> error "Invalid where clause, operator must be followed by an array of clauses"
    _ -> error "Invalid where clause, element of where clause array must be an single key object"
parseWhereClause _ _ val = error $ "Invalid where clause, must be a string or an object: " <> show val

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
  (L.stripPrefix "not_" -> Just opr) ->
    let popr = parseOperator opr
     in if popr `elem` comparisonOperator then Not popr else error $ "Invalid operator " <> show val
  _ -> error $ "Invalid operator " <> show val

parseTypes :: StorageParserM ()
parseTypes = do
  obj <- gets (dataObject . extraParseInfo)
  let tps = (map processType) <$> (obj ^? ix acc_types . _Value . to mkList)
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

    extractRecordType :: [(String, String)] -> RecordType
    extractRecordType flds =
      find ((== "recordType") . fst) flds
        & fmap snd
        & fromMaybe "Data"
        & \case
          "NewType" -> NewType
          "Data" -> Data
          "Type" -> Type
          _ -> error "Not a valid record type"

    processType :: (Key, Value) -> TypeObject
    processType (typeName, val) = do
      let extractedFields = case val of
            Object typeDef -> extractFields typeDef
            Array arr -> concatMap mkList $ V.toList arr
            _ -> error "Expected an object or array in type definition"
      let (fields, derivations, overrideDerives) = splitTypeAndDerivation extractedFields
      TypeObject (extractRecordType extractedFields) (TypeName $ toString typeName) fields derivations overrideDerives

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
          qType = makeTypeQualified defaultTypeImportMap (Just moduleName) (Just excludedList) (Just dataList) (Just defaultImportModule) impObj
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
                    hFieldType = makeTypeQualified defaultTypeImportMap (Just moduleName) (Just excludedList) (Just dataList) (Just defaultImportModule) impObj haskellType,
                    bFieldType = makeTypeQualified defaultTypeImportMap (Just moduleName) (Just excludedList) (Just dataList) (Just defaultImportModule) impObj beamType,
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
    typeDelimiter = "()[]'"
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
  "!SecondaryKey" -> Forced SecondaryKey
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

getAllFieldsUsedInQueries :: [QueryDef] -> StorageParserM [String]
getAllFieldsUsedInQueries queries = do
  fields <- gets (.tableDef.fields)
  pure $ L.nub $ concatMap (getAllFieldsInQuery fields) queries
  where
    getAllFieldsInQuery :: [FieldDef] -> QueryDef -> [String]
    getAllFieldsInQuery fdefs (QueryDef {..}) = L.nub ((maybe [] fst orderBy) : (concatMap (getAllBeamFieldsUsedInQueryParam fdefs) (params <> allWhereClauseQueryParams whereClause)))

    allWhereClauseQueryParams :: WhereClause -> [QueryParam]
    allWhereClauseQueryParams = \case
      EmptyWhere -> []
      Leaf (qp, _) -> [qp]
      Query (_, clauses) -> concatMap allWhereClauseQueryParams clauses

    getAllBeamFieldsUsedInQueryParam :: [FieldDef] -> QueryParam -> [String]
    getAllBeamFieldsUsedInQueryParam fdefs (QueryParam {..}) =
      case qpIsBeam of
        True -> [qpName]
        False -> case find (\fdef -> fieldName fdef == qpName) fdefs of
          Just (FieldDef {..}) -> map bFieldName beamFields
          Nothing -> []
