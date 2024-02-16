module NammaDSL.DSL.Syntax.Common where

import Kernel.Prelude

data RecordType
  = NewType
  | Data
  | Type
  deriving (Show, Eq)
