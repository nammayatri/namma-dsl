{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module NammaDSL.DSL.Parser.Frontend.Purs where

import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad.Trans.RWS.Lazy
import Data.Aeson
import Data.Aeson.Lens (key, _Array, _Object, _String, _Value)
import Data.Bifunctor
import qualified Data.ByteString as BS
import Data.Default
import Data.List (find)
import qualified Data.List.Extra as L
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import NammaDSL.AccessorTH
import NammaDSL.DSL.Parser.TechDesign (checkIfCorrectImp, etImp, getRequiredDeclType)
import NammaDSL.DSL.Syntax.Common
import NammaDSL.DSL.Syntax.Frontend.Purs
import NammaDSL.DSL.Syntax.TechDesign
import NammaDSL.Utils (makeTypeQualified, mkList, toModelList)
import Prelude

frontendPursParser :: FrontendPursRead -> FilePath -> IO PursFrontend
frontendPursParser frontendPursRead filepath = do
  contents <- BS.readFile filepath
  case Yaml.decodeEither' contents of
    Left _ -> error "Not a Valid Yaml"
    Right yml -> do
      evalParser parsePursFrontend (frontendPursRead {_fPursYamlObject = yml}) def

parsePursFrontend :: FrontendPursM ()
parsePursFrontend = do
  parseModelList
  makeComponentsQualified

parseModelList :: FrontendPursM ()
parseModelList = do
  yaml <- asks _fPursYamlObject
  let modelList = filter ((/= "imports") . fst) $ toModelList yaml
  mapM_
    ( \(modelName, model) -> do
        let modelType = fromMaybe (error $ "Model type required for purs model " <> modelName) (model ^? ix acc_modelType . _String)
        case modelType of
          "Component" -> parseComponent modelName model
          "Screen" -> parseScreen modelName model
          _ -> error $ "Invalid model type for purs model " <> modelName
    )
    modelList

parseComponent :: String -> Object -> FrontendPursM ()
parseComponent _compName _obj = do
  let typeList = fromMaybe [] $ _obj ^? ix acc_types . _Value . to mkList
      mkTypeList =
        map
          ( \(typeName, typeObj) ->
              let fields = mkList typeObj
                  declType = maybe PTYPE (getRequiredDeclType . T.pack . snd) $ find (("declType" ==) . fst) fields
                  actualFields = filter (("declType" /=) . fst) fields
               in PursType typeName declType (map (\(fieldName, fieldType) -> PursField fieldName fieldType) actualFields)
          )
          typeList
      defaultList = fromMaybe [] $ _obj ^? ix acc_defaults . _Value . to mkList
      mkDefaultList =
        map
          ( \(defaultName, defaultObj) ->
              let fields = mkList defaultObj
                  defaultType = maybe PTYPE (getRequiredDeclType . T.pack . snd) $ find (("defaultType" ==) . fst) fields
                  defaultTypeSig = maybe (error "defaultTypeSig required") snd $ find (("defaultTypeSig" ==) . fst) fields
                  actualFields = filter ((\x -> x `L.notElem` ["defaultType", "defaultTypeSig"]) . fst) fields
               in PursDefaultObj defaultName defaultType defaultTypeSig (map (\(fieldName, fieldType) -> PursField fieldName fieldType) actualFields)
          )
          defaultList
      viewConfigType' = _obj ^? ix acc_viewConfigType . _String . to T.unpack
      pCom = Component _compName mkTypeList mkDefaultList [] viewConfigType'
  modify $ \s -> s {_components = _components s ++ [pCom]}

makeComponentsQualified :: FrontendPursM ()
makeComponentsQualified = do
  defaultTypeImportMapper <- asks _fPursDefaultImportMapper
  yaml <- asks _fPursYamlObject
  pf <- get
  let dNames = (pf ^. components) & map _componentName
      qType = makeTypeQualified defaultTypeImportMapper Nothing Nothing (Just dNames) mempty yaml
      qualifiedComponents =
        map
          ( \comp ->
              let types = comp ^. componentTypes
                  qualifiedTypes =
                    map
                      ( \tp ->
                          let fields = tp ^. ptypeFields
                              qualifiedFields =
                                map
                                  ( \field ->
                                      let fieldType = field ^. pfieldType
                                       in field & pfieldType .~ qType fieldType
                                  )
                                  fields
                           in tp & ptypeFields .~ qualifiedFields
                      )
                      types
               in comp & componentTypes .~ qualifiedTypes
          )
          (pf ^. components)
  modify $ \s -> s {_components = figureComponentImports <$> qualifiedComponents}

figureComponentImports :: Component -> Component
figureComponentImports comp = do
  let potentialImportStrings = (map _pfieldType $ (comp ^. componentTypes & concatMap _ptypeFields) <> (comp ^. componentDefauts & concatMap _pdefaultFieldsValue)) <> (comp ^. componentDefauts & map _pdefaultTypeSig)
      figuredImports = filter (checkIfCorrectImp . T.pack) $ etImp potentialImportStrings
  comp & componentImports .~ (L.nub figuredImports)

parseScreen :: String -> Object -> FrontendPursM ()
parseScreen _screenName _obj = pure () -- TODO
