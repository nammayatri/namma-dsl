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

data ExtraOperations = EXTRA_QUERY_FILE deriving (Show, Eq)

type Database = String

data TableDef = TableDef
  { tableNameHaskell :: String,
    tableNameSql :: String,
    fields :: [FieldDef],
    imports :: [String],
    importPackageOverrides :: Map String String,
    queries :: [QueryDef],
    excludedDefaultQueries :: [String],
    primaryKey :: [String],
    secondaryKey :: [String],
    types :: Maybe [TypeObject],
    containsEncryptedField :: Bool,
    relationalTableNamesHaskell :: [String],
    derives :: Maybe String,
    beamTableInstance :: [Instance],
    domainTableInstance :: [Instance],
    extraOperations :: [ExtraOperations]
  }
  deriving (Show, Generic)

instance Default TableDef where
  def = TableDef "" "" [] [] mempty [] [] [] [] Nothing False [] Nothing [MakeTableInstances] [] []

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

data TypeObject = TypeObject RecordType TypeName [(FieldName, FieldType)] [InstanceToDerive]
  deriving (Show)

data QueryDef = QueryDef
  { queryName :: String,
    kvFunction :: String,
    params :: [((String, String), Bool)],
    whereClause :: WhereClause,
    orderBy :: (String, Order),
    takeFullObjectAsInput :: Bool
  }
  deriving (Show)

data WhereClause = EmptyWhere | Leaf (String, String, Maybe Operator) | Query (Operator, [WhereClause]) deriving (Show)

data Operator = And | Or | In | Eq | GreaterThan | LessThan | GreaterThanOrEq | LessThanOrEq deriving (Show, Eq)

data Order = Asc | Desc deriving (Show, Eq)

comparisonOperator :: [Operator]
comparisonOperator = [In, Eq, GreaterThan, LessThan, GreaterThanOrEq, LessThanOrEq]

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
  def = FieldDef "" "" [] Nothing False Nothing Nothing

data FieldRelation = OneToOne | MaybeOneToOne | OneToMany | WithId Create FromCached | WithIdStrict Create FromCached deriving (Show, Eq)

type Create = Bool

type FromCached = Bool

data FieldConstraint = PrimaryKey | SecondaryKey | NotNull | AUTOINCREMENT deriving (Show, Eq, Ord)

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
  def = BeamField "" "" "" [] [] "" Nothing [] Nothing False

instance Default ExtraParseInfo where
  def = ExtraParseInfo [] [] [] mempty mempty ""

data StorageState = StorageState
  { tableDef :: TableDef,
    extraParseInfo :: ExtraParseInfo
  }
  deriving (Show)

data StorageRead = StorageRead
  { domainTypeModulePrefix :: String,
    beamTypeModulePrefix :: String,
    queryModulePrefix :: String,
    sqlMapper :: [(String, String)],
    extraDefaultFields :: [(String, String)],
    storageDefaultTypeImportMapper :: [(String, String)]
  }
  deriving (Show)

instance Default StorageRead where
  def = StorageRead "" "" "" [] [] []

type StorageParserM = ParserM StorageRead StorageState

type StorageM = BuilderM TableDef

type Spaces = Int

data SQL_MANIPULATION = SQL_CREATE | SQL_ALTER SQL_ALTER deriving (Show)

data SQL_ALTER = ADD_COLUMN String String [ALTER_COLUMN_ACTION] | DROP_COLUMN String | ALTER_COLUMN String ALTER_COLUMN_ACTION | DROP_CONSTRAINT_PKS | ADD_PRIMARY_KEYS [String] deriving (Show)

data ALTER_COLUMN_ACTION = CHANGE_TYPE String | DROP_DEFAULT | SET_DEFAULT String | DROP_NOT_NULL | SET_NOT_NULL deriving (Show)
