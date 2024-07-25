module NammaDSL.DSL.Syntax.TechDesign where

import Data.Aeson
import Data.Default
import Data.Text
import Data.Void (Void)
import Language.PureScript.CST.Types
import NammaDSL.DSL.Syntax.Common
import Prelude

data TechDesign = TechDesign
  { changes :: [Ann Change],
    moduleMapper :: [(Text, Text)]
  }
  deriving (Show, Eq, Ord)

type LC = [Comment LineFeed]

type EC = [Comment Void]

type DeclSig = Text

data PRecordType = PTYPE | PNEWTYPE deriving (Show, Eq, Ord)

type RecordName = Text

data PImportType = Simple | Qualified deriving (Show, Eq, Ord)

data PImport = PImport Text PImportType deriving (Show, Eq, Ord)

data Change = AddRecord PRecordType RecordName | AddField DeclSig Text Text | AddImport PImport | AddComment DeclSig Text deriving (Show, Eq, Ord)

data TechDRead = TechDRead
  { tdPathPrefixes :: [FilePath],
    defaultModuleMapper :: [(Text, Text)],
    yamlObject :: Object
  }
  deriving (Show, Eq, Ord)

instance Default TechDRead where
  def = TechDRead [] [] mempty

type TechDM = ParserM TechDRead TechDesign

data Ann a = Ann
  { change :: a,
    mdl :: Text,
    path :: FilePath
  }
  deriving (Show, Eq, Ord)

instance Default TechDesign where
  def = TechDesign [] mempty
