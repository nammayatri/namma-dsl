{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module NammaDSL.Generator.Purs.TechDesign where

import Data.Maybe
import qualified Data.Text.IO as T
import Language.PureScript.CST as P
import NammaDSL.DSL.Syntax.TechDesign
import qualified NammaDSL.Generator.Purs.CST as PCST
import Prelude

applyTechDesignChanges :: TechDesign -> IO ()
applyTechDesignChanges td =
  mapM_ applyChange (changes td)

applyChange :: Ann Change -> IO ()
applyChange (Ann chg _ fp) = do
  contents <- T.readFile fp
  case snd (P.parse contents) of
    Left err -> print err
    Right md -> do
      newMd <- case chg of
        AddField declSig fieldName fieldTypeOrValue -> do
          let newDecls = PCST.findAndAddFieldToDecl declSig fieldName fieldTypeOrValue (modDecls md)
          pure $ md {modDecls = newDecls}
        AddImport imp -> do
          pure $ PCST.addImports (pure imp) md
        AddComment declSig cmt -> do
          let newDecls = PCST.addCmtUpDeclSig declSig (Comment cmt) (modDecls md)
          pure $ md {modDecls = newDecls}
        AddRecord recType recName enumDef -> do
          if PCST.findDeclWithName recName (modDecls md) /= Nothing
            then pure md
            else do
              let newDecl = PCST.addNewDecl recType recName enumDef
              pure $ md {modDecls = modDecls md ++ [newDecl]}

      T.writeFile fp (P.printModule newMd)
