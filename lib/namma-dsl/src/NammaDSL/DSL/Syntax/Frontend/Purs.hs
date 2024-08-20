{-# LANGUAGE TemplateHaskell #-}

module NammaDSL.DSL.Syntax.Frontend.Purs where

import Control.Lens
import Data.Aeson
import Data.Default
import NammaDSL.DSL.Syntax.Common
import NammaDSL.DSL.Syntax.TechDesign
import Prelude

data Screen = Screen
  {
  } -- TODO

$(makeLenses ''Screen)

data PursField = PursField
  { _pfieldName :: String,
    _pfieldType :: String
  }

$(makeLenses ''PursField)

data PursDefaultObj = PursDefaultObj
  { _pdefaultObjName :: String,
    _pdefaultObjType :: PRecordType,
    _pdefaultTypeSig :: String,
    _pdefaultFieldsValue :: [PursField]
  }

$(makeLenses ''PursDefaultObj)

data PursType = PursType
  { _ptypeName :: String,
    _ptypeOf :: PRecordType,
    _ptypeFields :: [PursField]
  }

$(makeLenses ''PursType)

data Component = Component
  { _componentName :: String,
    _componentTypes :: [PursType],
    _componentDefauts :: [PursDefaultObj],
    _componentImports :: [String],
    _viewConfigType :: Maybe String
  }

$(makeLenses ''Component)

instance Default Component where
  def = Component mempty [] [] [] Nothing

data PursFrontend = PursFrontend
  { _extraFileChanges :: [Ann Change],
    _components :: [Component],
    _screens :: [Screen]
  }

$(makeLenses ''PursFrontend)

data FrontendPursRead = FrontendPursRead
  { _fPursDefaultImportMapper :: [(String, String)],
    _fPursDefaultImports :: [String],
    _fPursYamlObject :: Object,
    _fPursComponentModulePrefix :: String,
    _fPursScreenModulePrefix :: String
  }

$(makeLenses ''FrontendPursRead)

instance Default FrontendPursRead where
  def = FrontendPursRead [] [] mempty mempty mempty

type FrontendPursM = ParserM FrontendPursRead PursFrontend

instance Default PursFrontend where
  def = PursFrontend [] [] []
