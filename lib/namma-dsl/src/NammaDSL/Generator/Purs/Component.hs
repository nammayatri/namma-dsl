{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module NammaDSL.Generator.Purs.Component where

import Control.Lens ((^.))
import Control.Lens.Combinators
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import qualified Data.Text as T
import NammaDSL.Config
import NammaDSL.DSL.Parser.TechDesign (extraChanges)
import NammaDSL.DSL.Syntax.Frontend.Purs
import NammaDSL.DSL.Syntax.TechDesign
import NammaDSL.Generator.Purs.CST
import NammaDSL.Generator.Purs.Common
import NammaDSL.GeneratorCore
import NammaDSL.Lib hiding (Q, Writer)
import qualified NammaDSL.Lib.TH as TH
import qualified NammaDSL.Lib.Types as TH
import System.Directory
import System.FilePath
import Prelude

data ComponentGenCode = ComponentGenCode
  { componentGenName :: String,
    componentGenPath :: FilePath,
    reExportModule :: Code,
    controllerBaseCode :: Code,
    controllerOverlapChanges :: [Ann Change],
    viewCode :: Code
  }
  deriving (Show, Eq)

generateAllComponents :: FrontendPursRead -> FrontendConfig -> [Component] -> [ComponentGenCode]
generateAllComponents fpRead fpc comps = map (generateComponent fpRead fpc) comps

generateComponent :: FrontendPursRead -> FrontendConfig -> Component -> ComponentGenCode
generateComponent fRead fconfig component =
  ComponentGenCode
    { componentGenName = component ^. componentName,
      componentGenPath = componentGenerationPath,
      reExportModule = reExportModuleCode,
      controllerBaseCode = controllerBaseCode,
      controllerOverlapChanges = controllerOverlapChanges,
      viewCode = viewCode'
    }
  where
    controllerFilePath = componentGenerationPath </> "Controller.purs"
    (viewTypeSig, viewTypeQImps) =
      pTypeWithArrow
        "forall w."
        [ "(" <> T.pack controllerModuleName <> ".Action -> Effect Unit)",
          maybe
            (T.pack $ controllerModuleName <> ".Config")
            ( \nm ->
                if '.' `elem` nm
                  then T.pack nm
                  else T.pack $ controllerModuleName <> "." <> nm
            )
            (component ^. viewConfigType),
          "PrestoDOM (Effect Unit) w"
        ]
    reExportModuleName = (fRead ^. fPursComponentModulePrefix) <> "." <> (component ^. componentName)
    viewModuleName = (fRead ^. fPursComponentModulePrefix) <> "." <> (component ^. componentName) <> ".View"
    controllerModuleName = (fRead ^. fPursComponentModulePrefix) <> "." <> (component ^. componentName) <> ".Controller"
    reExportModuleCode =
      moduleToCode $
        (pModule (T.pack reExportModuleName) ["module Reexport"])
          & putImports
            [ pImport (Just "Reexport") (T.pack viewModuleName),
              pImport (Just "Reexport") (T.pack controllerModuleName)
            ]
    viewCode' =
      moduleToCode $
        (pModule (T.pack viewModuleName) [])
          & putImports
            ( viewTypeQImps
                ++ [ pImport Nothing "Effect (Effect)",
                     pImport Nothing "Prelude (Unit)",
                     pImport Nothing "PrestoDOM (PrestoDOM, linearLayout)"
                   ]
            )
          & pToBeImplementedFunction "view" viewTypeSig "linearLayout [] []"
    controllerBaseCode =
      moduleToCode $
        (pModule (T.pack controllerModuleName) [])
          & putImports
            ( map ((\imp -> pImport Nothing imp) . T.pack) (fconfig ^. fDefaultImports)
            )
    controllerTypeChanges =
      component ^. componentTypes
        & map toChange
        & concat
    controllerDefaultObjChanges =
      component ^. componentDefauts
        & map toChange
        & concat
    componentOverlapImps =
      component ^. componentImports
        & map T.pack
        & map (\imp -> AddImport (PImport imp Qualified))
    controllerOverlapChanges =
      map
        ( \change ->
            Ann
              { change = change,
                mdl = T.pack controllerModuleName,
                path = controllerFilePath
              }
        )
        $ concatMap extraChanges (controllerTypeChanges ++ controllerDefaultObjChanges) ++ controllerTypeChanges ++ controllerDefaultObjChanges ++ componentOverlapImps
    componentGenerationPath = (fconfig ^. fGenRootPath) </> changeModuleToPath (fRead ^. fPursComponentModulePrefix) </> (component ^. componentName)

changeModuleToPath :: String -> FilePath
changeModuleToPath = T.unpack . (T.replace "." "/") . T.pack
