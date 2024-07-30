module NammaDSL.DSL.Syntax.Common where

import Control.Monad.RWS
import Control.Monad.Trans.Except
import Data.Default
import System.Exit
import Prelude

data FileState = NEW | CHANGED | UNCHANGED | NOT_EXIST deriving (Eq, Show)

data RecordType
  = NewType
  | Data
  | Type
  deriving (Show, Eq)

data ParseError = InternalError String | YamlError String

instance Show ParseError where
  show (InternalError s) = "Internal Error: " ++ s
  show (YamlError s) = "Yaml Error: " ++ s

type ParserM r s = RWST r () s (ExceptT ParseError IO)

evalParser :: ParserM r s a -> r -> s -> IO s
evalParser p r s = do
  res <- runExceptT $ fst <$> execRWST p r s
  either (die . show) pure res

throwError :: ParseError -> ParserM r s ()
throwError err = lift $ throwE err

-- to store pin point error location --
data Marked a = Marked
  { val :: a,
    pos :: Location
  }
  deriving (Show)

data Location = Location
  { line :: Int,
    column :: Int
  }
  deriving (Show, Eq, Ord)

instance Functor Marked where
  fmap f (Marked a p) = Marked (f a) p

instance Applicative Marked where
  pure a = Marked a def
  (Marked f p) <*> (Marked a _) = Marked (f a) p

instance Monad Marked where
  return = pure
  (>>=) (Marked v p) f = Marked (val (f v)) p

instance Ord a => Ord (Marked a) where
  compare (Marked a _) (Marked b _) = compare a b

instance Eq a => Eq (Marked a) where
  (Marked a _) == (Marked b _) = a == b

instance Default a => Default (Marked a) where
  def = Marked def def

instance Default Location where
  def = Location 1 0

-- Better import definition, will work on it later
data HImport = HImport
  { hImportModule :: String,
    hIsImportQualified :: Bool,
    hImportPkg :: Maybe String,
    hImportAs :: Maybe String,
    hImportSpecs :: Maybe HImportSpecList
  }

data HImportSpecList = HImportSpecList
  { hIsHidden :: Bool,
    hSpecList :: [String]
  }
