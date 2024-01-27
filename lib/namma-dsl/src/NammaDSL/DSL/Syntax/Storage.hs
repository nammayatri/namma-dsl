module NammaDSL.DSL.Syntax.Storage where

import Data.Map (Map)
import Kernel.Prelude
import NammaDSL.GeneratorCore

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

data TableDef = TableDef
  { tableNameHaskell :: String,
    tableNameSql :: String,
    fields :: [FieldDef],
    imports :: [String],
    importPackageOverrides :: Map String String,
    queries :: [QueryDef],
    primaryKey :: [String],
    secondaryKey :: [String],
    types :: Maybe [TypeObject],
    containsEncryptedField :: Bool,
    relationalTableNamesHaskell :: [String],
    extaOperations :: [ExtraOperations]
  }
  deriving (Show)

newtype TypeObject = TypeObject (String, ([(String, String)], [String])) --  (TypeName, ([(Field, HaskellType)], [InstanceToDerive]))
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
    fromTType :: Maybe String,
    isEncrypted :: Bool,
    relation :: Maybe FieldRelation,
    relationalTableNameHaskell :: Maybe String
  }
  deriving (Show)

data FieldRelation = OneToOne | MaybeOneToOne | OneToMany deriving (Show, Eq)

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

data BeamField = BeamField
  { bFieldName :: String,
    hFieldType :: String,
    bFieldType :: String,
    bConstraints :: [FieldConstraint],
    bFieldUpdates :: [SqlFieldUpdates],
    bSqlType :: String,
    bDefaultVal :: Maybe String,
    bfieldExtractor :: [String],
    bToTType :: Maybe String,
    bIsEncrypted :: Bool
  }
  deriving (Show)

type StorageM = BuilderM TableDef
