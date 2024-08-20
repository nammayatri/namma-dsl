{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module NammaDSL.Generator.Purs.Common where

import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import qualified Data.Text as T
import NammaDSL.DSL.Syntax.Frontend.Purs
import NammaDSL.DSL.Syntax.TechDesign
import NammaDSL.Generator.Purs.CST
import Prelude

class ToChange a where
  toChange :: a -> [Change]

instance ToChange PursType where
  toChange (PursType {..}) =
    [AddRecord _ptypeOf (pack _ptypeName)]
      ++ map (\(PursField n t) -> AddField (pack _ptypeName) (pack n) (pack t)) _ptypeFields

instance ToChange PursDefaultObj where
  toChange (PursDefaultObj {..}) =
    [AddDefaultRecord (pack _pdefaultObjName) (pack _pdefaultTypeSig)] -- TODO: Need to add default addition in cst
      ++ map (\(PursField n t) -> AddField (pack _pdefaultObjName) (pack n) (pack t)) _pdefaultFieldsValue
