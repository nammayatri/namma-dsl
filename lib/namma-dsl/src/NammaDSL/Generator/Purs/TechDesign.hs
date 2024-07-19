module NammaDSL.Generator.Purs.TechDesign where

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
          let newDecls = PCST.addCmtUpDeclSig declSig [Comment cmt, Line LF] (modDecls md)
          pure $ md {modDecls = newDecls}
      T.writeFile fp (P.printModule newMd)
