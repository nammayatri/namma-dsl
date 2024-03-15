module NammaDSL.Lib.Interpreter where

import Control.Monad.Writer hiding (Writer)
import qualified Data.List as L
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Ppr as Ppr
import qualified Language.Haskell.TH.PprLib as Ppr
import NammaDSL.Lib.Types
import qualified Text.PrettyPrint as Pretty
import Prelude

interpreter :: r -> Writer r CodeUnit -> Maybe String
interpreter env unitW = do
  let codeUnits = runQ env . execWriterT $ unitW
  let codeTree = foldMap mkCodeTree codeUnits
  if codeTree == mempty
    then Nothing
    else Just $ interpret codeTree

mkCodeTree :: CodeUnit -> CodeTree
mkCodeTree (CodeDec codeDecs) = mempty{codeDecs = codeDecs}
-- mkCodeTree (CodeImport codeImport) = mempty{codeImports = [codeImport]}
-- mkCodeTree (CodeExtension codeExtension) = mempty{codeExtensions = [codeExtension]}
mkCodeTree (CodeSplice codeSplice') = mempty{codeSplices = [codeSplice']}

interpret :: CodeTree -> String
interpret CodeTree {..} = do
  -- interpret :: String -> CodeTree -> String
  -- interpret moduleName CodeTree {..} = do
  -- L.intercalate "\n" (interpretExtension <$> codeExtensions)
  --   <> "\n\n"
  --   <> "module "
  --   <> moduleName
  --   <> " where"
  -- <> "\n\n"
  -- <> L.intercalate "\n" (interpretImport <$> codeImports)
  -- <> "\n\n"
  interpretDecs codeDecs
    <> "\n\n"
    <> L.intercalate "\n\n" (interpretSplice <$> codeSplices)
    <> "\n\n"

interpretDecs :: [TH.Dec] -> String
interpretDecs = pprint'

-- interpretImport :: Import -> String
-- interpretImport (Import m) = "import " <> m
-- interpretImport (ImportQualified m (Just syn)) = "import qualified " <> m <> " as " <> syn
-- interpretImport (ImportQualified m Nothing) = "import qualified " <> m

-- interpretExtension :: Extension -> String
-- interpretExtension (Extension e) = "{-# LANGUAGE " <> e <> " #-}"

interpretSplice :: Splice -> String
interpretSplice (Splice e) = "$(" <> pprint' e <> ")"

myStyle :: Pretty.Style
myStyle = Pretty.style {Pretty.lineLength = 300}

pprint' :: Ppr.Ppr a => a -> String
pprint' x = Pretty.renderStyle myStyle $ Ppr.to_HPJ_Doc $ Ppr.ppr x
