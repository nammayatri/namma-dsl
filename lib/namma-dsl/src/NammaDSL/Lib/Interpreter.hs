module NammaDSL.Lib.Interpreter where

import Control.Monad.Writer hiding (Writer)
import Data.Functor ((<&>))
-- import qualified Data.List as L
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Ppr as Ppr
import qualified Language.Haskell.TH.PprLib as Ppr
import NammaDSL.Lib.Types
import qualified Text.PrettyPrint as Pretty
import Prelude

interpreter :: r -> Writer r CodeUnit -> Maybe String
interpreter env unitW = do
  let codeUnits = runQ env . execWriterT $ unitW
  let codeStringDecs =
        concat $
          codeUnits <&> \case
            CodeDec codeDecs -> interpretDecs codeDecs
            CodeSplice _ -> mempty
            CodeComment comment -> interpretComment comment
  -- splices should be in the end of module
  let codeStringSplices =
        concat $
          codeUnits <&> \case
            CodeDec _ -> mempty
            CodeSplice splice -> interpretSplice splice
            CodeComment _ -> mempty
  let codeString = codeStringDecs <> codeStringSplices
  if codeString == mempty
    then Nothing
    else Just codeString

-- mkCodeTree :: CodeUnit -> CodeTree
-- mkCodeTree (CodeDec codeDecs) = mempty{codeDecs = codeDecs}
-- -- mkCodeTree (CodeImport codeImport) = mempty{codeImports = [codeImport]}
-- -- mkCodeTree (CodeExtension codeExtension) = mempty{codeExtensions = [codeExtension]}
-- mkCodeTree (CodeSplice codeSplice') = mempty{codeSplices = [codeSplice']}

-- interpret :: CodeUnit -> String
-- interpret CodeTree {..} = do
--   -- interpret :: String -> CodeTree -> String
--   -- interpret moduleName CodeTree {..} = do
--   -- L.intercalate "\n" (interpretExtension <$> codeExtensions)
--   --   <> "\n\n"
--   --   <> "module "
--   --   <> moduleName
--   --   <> " where"
--   -- <> "\n\n"
--   -- <> L.intercalate "\n" (interpretImport <$> codeImports)
--   -- <> "\n\n"
--   interpretDecs codeDecs
--     <> "\n\n"
--     <> L.intercalate "\n\n" (interpretSplice <$> codeSplices)
--     <> "\n\n"

interpretDecs :: [TH.Dec] -> String
interpretDecs = (<> "\n\n") . pprint'

-- interpretImport :: Import -> String
-- interpretImport (Import m) = "import " <> m
-- interpretImport (ImportQualified m (Just syn)) = "import qualified " <> m <> " as " <> syn
-- interpretImport (ImportQualified m Nothing) = "import qualified " <> m

-- interpretExtension :: Extension -> String
-- interpretExtension (Extension e) = "{-# LANGUAGE " <> e <> " #-}"

interpretSplice :: Splice -> String
interpretSplice (Splice e) = "$(" <> pprint' e <> ")" <> "\n\n"

interpretComment :: Comment -> String
interpretComment (Comment str) = "--" <> str <> "\n"
interpretComment AddNewLine = "\n"

myStyle :: Pretty.Style
myStyle = Pretty.style {Pretty.lineLength = 300}

pprint' :: Ppr.Ppr a => a -> String
pprint' x = Pretty.renderStyle myStyle $ Ppr.to_HPJ_Doc $ Ppr.ppr x
