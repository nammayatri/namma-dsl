module NammaDSL.DSL.Syntax.Common where

import Control.Monad.RWS
import Control.Monad.Trans.Except
import System.Exit
import Prelude

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
