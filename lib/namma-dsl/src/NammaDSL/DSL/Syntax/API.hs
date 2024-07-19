{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module NammaDSL.DSL.Syntax.API where

import Control.Lens hiding (noneOf)
import Data.Aeson (Object)
import Data.Default
import Data.Map (Map)
import Data.Text (Text)
import NammaDSL.DSL.Syntax.Common
import NammaDSL.GeneratorCore
import Prettyprinter
import Prelude

-- import qualified Data.Text as T
-- import Data.List (map)

data UrlParts
  = UnitPath Text
  | Capture Text Text
  | QueryParam Text Text Bool
  deriving (Show)

data ApiType = GET | POST | PUT | DELETE deriving (Show)

data AuthType = AdminTokenAuth | TokenAuth TokenAuthType | NoAuth | SafetyWebhookAuth DashboardAuthType | DashboardAuth DashboardAuthType deriving (Show)

data TokenAuthType = RIDER_TYPE | PROVIDER_TYPE deriving (Show)

data DashboardAuthType = DASHBOARD_USER | DASHBOARD_ADMIN | FLEET_OWNER | DASHBOARD_RELEASE_ADMIN | MERCHANT_ADMIN | MERCHANT_MAKER | MERCHANT_CHECKER | MERCHANT_SERVER | MERCHANT_USER
  deriving (Show)

data HeaderType = Header Text Text deriving (Show)

data ApiReq = ApiReq Text Text deriving (Show)

data ApiRes = ApiRes Text Text deriving (Show)

data ApiParts = ApiTU ApiType [UrlParts] | HeaderT HeaderType | Auth (Maybe AuthType) | Req Text Text | Res Text Text | ModuleName Text deriving (Show)

data ApiTT = ApiTT
  { _urlParts :: [UrlParts],
    _apiType :: ApiType,
    _authType :: Maybe AuthType,
    _header :: [HeaderType],
    _apiReqType :: Maybe ApiReq,
    _apiResType :: ApiRes
  }
  deriving (Show)

$(makeLenses ''ApiTT)

data Constructor = Constructor
  { constructorName :: Text,
    constructorArgs :: [Text]
  }
  deriving (Show, Eq)

data Field = Field
  { fieldName :: Text,
    fieldType :: Text
  }
  deriving (Show, Eq)

data TypeObject = TypeObject RecordType (Text, ([(Text, Text)], [Text])) deriving (Show)

type RecordName = String

data RecordObject
  = SumType RecordType RecordName Constructor
  | RecordTypeDeclaration RecordType RecordName String [Field]
  deriving (Show, Eq)

data Junk = Junk Text deriving (Show)

data Declaration = Declaration RecordObject | Junk' Text deriving (Show)

data TypesInfo = TypesInfo
  { _typeImports :: [Text],
    _types :: [TypeObject]
  }
  deriving (Show)

$(makeLenses ''TypesInfo)

data Apis = Apis
  { _moduleName :: Text,
    _apis :: [ApiTT],
    _imports :: [Text],
    _importPackageOverrides :: Map String String,
    _apiTypes :: TypesInfo,
    _hsImports :: Object,
    _extImports :: Object
  }
  deriving (Show)

data PursModule = PursModule
  { _decl :: [Declaration]
  }
  deriving (Show)

data PursModule' = PursModule'
  { _decl' :: [RecordObject],
    _junk' :: Text
  }
  deriving (Show)

$(makeLenses ''Apis)

type ApisM = BuilderM Apis

data ApiRead = ApiRead
  { apiTypesImportPrefix :: String,
    apiServantImportPrefix :: String,
    apiDomainHandlerImportPrefix :: String,
    apiDefaultTypeImportMapper :: [(String, String)]
  }

data ExtraParseInfo = ExtraParseInfo
  { _yamlObj :: Object,
    _parsedTypesDataNames :: [String]
  }

$(makeLenses ''ExtraParseInfo)

data ApiState = ApiState
  { _apisRes :: Apis,
    _extraParseInfo :: ExtraParseInfo
  }

data PursState = PursState
  { _datatypes :: [RecordObject],
    _junk :: Text
  }

$(makeLenses ''ApiState)

instance Default ExtraParseInfo where
  def = ExtraParseInfo mempty []

instance Default ApiState where
  def = ApiState def def

instance Default TypesInfo where
  def = TypesInfo [] []

instance Default ApiRead where
  def = ApiRead "" "" "" []

instance Default Apis where
  def = Apis "" [] [] mempty def mempty mempty

type ApiParserM = ParserM ApiRead ApiState

type PursParserM = ParserM ApiRead PursState

getRecordName :: RecordObject -> RecordName
getRecordName rO = case rO of
  SumType _ recName _ -> recName
  RecordTypeDeclaration _ recName _ _ -> recName

hasFields :: RecordObject -> Bool
hasFields rO = case rO of
  RecordTypeDeclaration _ _ _ _ -> True
  _ -> False

getFields :: RecordObject -> [Field]
getFields rO = case rO of
  RecordTypeDeclaration _ _ _ x -> x
  _ -> []

replaceFields :: RecordObject -> [Field] -> RecordObject
replaceFields (RecordTypeDeclaration recType recName consName _) fields =
  RecordTypeDeclaration recType recName consName fields
replaceFields a _ = a

replaceElement :: RecordObject -> RecordObject -> [RecordObject] -> [RecordObject]
replaceElement _ _ [] = []
replaceElement old new (x : xs)
  | x == old = new : xs
  | otherwise = x : replaceElement old new xs

instance Pretty RecordType where
  pretty = \case
    NewType -> "newtype"
    Data -> "data"
    Type -> "type"

instance Pretty Constructor where
  pretty (Constructor name args) = pretty name <+> (cat $ (punctuate (" ") (pretty <$> args)))

instance Pretty Field where
  pretty (Field name types') = Prettyprinter.line <> pretty name <+> "::" <+> pretty types'

instance Pretty RecordObject where
  pretty (SumType recordType recordName constructor) =
    pretty recordType <+> pretty recordName <+> "=" <+> pretty constructor
  pretty (RecordTypeDeclaration recordType recordName consName fields) =
    pretty recordType <+> pretty recordName <+> "=" <+> pretty consName <+> (align . bracesList . fmap pretty $ fields)

instance Pretty PursModule' where
  pretty (PursModule' decl junk) = vsep (pretty <$> decl) <> Prettyprinter.line <> pretty junk

bracesList :: [Doc ann] -> Doc ann
bracesList xs = "{" <+> (vsep (punctuate (", ") xs)) <+> " }"

-- group $ flatAlt "{ " "{" <> align (vsep (punctuate (", ") xs)) <> flatAlt " }" "}"
