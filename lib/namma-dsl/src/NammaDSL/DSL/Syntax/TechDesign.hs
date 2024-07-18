module NammaDSL.DSL.Syntax.TechDesign where

import Data.Maybe
import Data.Text
import Data.Void (Void)
import Language.PureScript.CST.Types
import Prelude

data TechDesign = TechDesign
  { changes :: [Ann Change]
  }
  deriving (Show, Eq, Ord)

type LC = [Comment LineFeed]

type EC = [Comment Void]

type DeclSig = Text

data PImportType = Simple | Qualified deriving (Show, Eq, Ord)

data PImport = PImport Text PImportType deriving (Show, Eq, Ord)

data Change = AddField Text Text | AddImport PImport | AddComment DeclSig Text deriving (Show, Eq, Ord)

data Ann a = Ann
  { change :: a,
    mdl :: Text,
    path :: Maybe Text
  }
  deriving (Show, Eq, Ord)
