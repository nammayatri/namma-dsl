module NammaDSL.DSL.Syntax.Storage where

import Data.Aeson (Object)
import Data.Default
import Data.Map (Map)
import GHC.Generics (Generic)
import NammaDSL.DSL.Syntax.Common
import NammaDSL.GeneratorCore
import Prelude

data SqlUpdates = SqlUpdates
  { fieldUpdates :: Maybe (String, SqlFieldUpdates),
    keysInPrimaryKey :: [String]
  }
  deriving (Show)

data MigrationFile = MigrationFile
  { sqlTableName :: String,
    fields_ :: [FieldDef],
    primaryKeys :: [String],
    secondaryKeys :: [String],
    rawLastSqlFile :: String
  }
  deriving (Show)

data ExtraOperations = EXTRA_QUERY_FILE | EXTRA_DOMAIN_TYPE_FILE | EXTRA_CACHED_QUERY_FILE deriving (Show, Eq)

type Database = String

data ITransformer = ITransformer
  { outputVariableName :: String,
    transformer :: TransformerFunction
  }
  deriving (Show)

data IntermediateTransformers = IntermediateTransformers
  { getToTTypes :: [ITransformer],
    getFromTTypes :: [ITransformer]
  }
  deriving (Show, Generic)

instance Default IntermediateTransformers where
  def = IntermediateTransformers [] []

data TableDef = TableDef
  { tableNameHaskell :: String,
    tableNameSql :: String,
    fields :: [FieldDef],
    imports :: [String],
    importPackageOverrides :: Map String String,
    queries :: [QueryDef],
    defaultQueryTypeConstraint :: Maybe String,
    cachedQueries :: [CachedQueryDef],
    excludedDefaultQueries :: [String],
    primaryKey :: [String],
    secondaryKey :: [String],
    types :: Maybe [TypeObject],
    containsEncryptedField :: Bool,
    relationalTableNamesHaskell :: [String],
    derives :: Maybe [InstanceToDerive],
    beamTableInstance :: [Instance],
    domainTableInstance :: [Instance],
    extraOperations :: [ExtraOperations],
    intermediateTransformers :: IntermediateTransformers
  }
  deriving (Show, Generic)

instance Default TableDef where
  def = TableDef mempty mempty [] [] mempty [] Nothing [] [] [] [] Nothing False [] Nothing [MakeTableInstances] [] [] def

data CachedQueryDef = CachedQueryDef
  { cQueryName :: String,
    withCrossAppRedis :: Bool,
    ctypeConstraint :: Maybe String,
    cacheDataType :: CQReturnType,
    keyMaker :: Maybe String, -- will be used in future if required
    keyParams :: [Param],
    dbQuery :: String,
    dbQueryParams :: [Param],
    paramsOrder :: Maybe [String],
    cQueryType :: CQueryType
  }
  deriving (Show, Generic)

data Param = Constant String ParamConstantType | Variable String String deriving (Show, Generic, Eq) -- Variable Name Type -- Constant Value Type

--might require later for embedded constants in beam queries ?
data ParamConstantType = PString | PInt | PBool | PDouble | PImportedData deriving (Show, Generic, Eq)

data CQReturnType = CArray | COne deriving (Show, Eq)

data CQueryType = FindAndCache | FindOnly | CacheOnly | DeleteCache deriving (Show, Eq)

data Instance
  = MakeTableInstances
  | MakeTableInstancesGenericSchema
  | MakeTableInstancesWithTModifier String
  | Custom String (Maybe String) String -- Custom <Instance Name> <Data Name> <Params>
  deriving (Eq, Show)

newtype TypeName = TypeName {getTypeName :: String}
  deriving (Show, Eq)

newtype FieldName = FieldName {getFieldName :: String}
  deriving (Show, Eq)

newtype FieldType = FieldType {getFieldType :: String}
  deriving (Show)

newtype InstanceToDerive = InstanceToDerive {getInstanceToDerive :: String}
  deriving (Show, Eq)

type OverrideDefaultDerive = Bool

data TypeObject = TypeObject RecordType TypeName [(FieldName, FieldType)] [InstanceToDerive] OverrideDefaultDerive
  deriving (Show)

data QueryDef = QueryDef
  { queryName :: String,
    kvFunction :: String,
    params :: [QueryParam],
    whereClause :: WhereClause,
    orderBy :: (String, Order),
    takeFullObjectAsInput :: Bool,
    typeConstraint :: Maybe String
  }
  deriving (Show)

data QueryParam = QueryParam
  { qpName :: String,
    qpType :: String,
    qpExtParam :: Maybe Param,
    qpIsBeam :: Bool
  }
  deriving (Show, Eq)

data WhereClause = EmptyWhere | Leaf (QueryParam, Maybe Operator) | Query (Operator, [WhereClause]) deriving (Show)

data Operator = And | Or | In | Eq | GreaterThan | LessThan | GreaterThanOrEq | LessThanOrEq | Not Operator deriving (Show, Eq)

data Order = Asc | Desc deriving (Show, Eq)

comparisonOperator :: [Operator]
comparisonOperator = simpleComparison <> negatedComparison
  where
    negatedComparison = Not <$> simpleComparison
    simpleComparison = [In, Eq, GreaterThan, LessThan, GreaterThanOrEq, LessThanOrEq]

data FieldDef = FieldDef
  { fieldName :: String,
    haskellType :: String,
    beamFields :: [BeamField],
    fromTType :: Maybe TransformerFunction,
    isEncrypted :: Bool,
    relation :: Maybe FieldRelation,
    relationalTableNameHaskell :: Maybe String
  }
  deriving (Show)

instance Default FieldDef where
  def = FieldDef mempty mempty [] Nothing False Nothing Nothing

data FieldRelation = OneToOne | MaybeOneToOne | OneToMany | WithId Create FromCached | WithIdStrict Create FromCached deriving (Show, Eq)

type Create = Bool

type FromCached = Bool

data FieldConstraint = PrimaryKey | SecondaryKey | NotNull | Forced FieldConstraint deriving (Show, Eq, Ord)

data SqlFieldUpdates = DropNotNull | DropDefault | AddNotNull | AddDefault String | DropColumn | TypeChange deriving (Show)

instance Eq SqlFieldUpdates where
  (==) DropNotNull DropNotNull = True
  (==) DropColumn DropColumn = True
  (==) DropDefault DropDefault = True
  (==) AddNotNull AddNotNull = True
  (==) (AddDefault _) (AddDefault _) = True
  (==) TypeChange TypeChange = True
  (==) _ _ = False

data TFType = PureT | MonadicT deriving (Show, Eq)

data TransformerFunction = TransformerFunction
  { tfName :: String,
    tfType :: TFType,
    tfIsEmbeddedArgs :: Bool
  }
  deriving (Show)

data BeamField = BeamField
  { bFieldName :: String,
    hFieldType :: String,
    bFieldType :: String,
    bConstraints :: [FieldConstraint],
    bFieldUpdates :: [SqlFieldUpdates],
    bSqlType :: String,
    bDefaultVal :: Maybe String,
    bfieldExtractor :: [String],
    bToTType :: Maybe TransformerFunction,
    bIsEncrypted :: Bool
  }
  deriving (Show)

data ExtraParseInfo = ExtraParseInfo
  { dList :: [String],
    enumList :: [String],
    excludedImportList :: [String],
    yamlObject :: Object,
    dataObject :: Object,
    domainName :: String
  }
  deriving (Show)

instance Default BeamField where
  def = BeamField mempty mempty mempty [] [] mempty Nothing [] Nothing False

instance Default ExtraParseInfo where
  def = ExtraParseInfo [] [] [] mempty mempty mempty

data StorageState = StorageState
  { tableDef :: TableDef,
    extraParseInfo :: ExtraParseInfo
  }
  deriving (Show)

data StorageRead = StorageRead
  { domainTypeModulePrefix :: String,
    beamTypeModulePrefix :: String,
    queryModulePrefix :: String,
    cachedQueryModulePrefix :: String,
    sqlMapper :: [(String, String)],
    extraDefaultFields :: [(String, String)],
    storageDefaultTypeImportMapper :: [(String, String)],
    defaultCachedQueryKeyPfx :: String,
    srcFileStatus :: FileState
  }
  deriving (Show)

instance Default StorageRead where
  def = StorageRead mempty mempty mempty mempty [] [] [] mempty NEW

type StorageParserM = ParserM StorageRead StorageState

type StorageM = BuilderM TableDef

type Spaces = Int

data SQL_MANIPULATION = SQL_CREATE | SQL_ALTER SQL_ALTER deriving (Show)

data SQL_ALTER = ADD_COLUMN String String [ALTER_COLUMN_ACTION] | DROP_COLUMN String | ALTER_COLUMN String ALTER_COLUMN_ACTION | DROP_CONSTRAINT_PKS | ADD_PRIMARY_KEYS [String] deriving (Show)

data ALTER_COLUMN_ACTION = CHANGE_TYPE String | DROP_DEFAULT | SET_DEFAULT String | DROP_NOT_NULL | SET_NOT_NULL deriving (Show)

type SQL_ERROR = String
