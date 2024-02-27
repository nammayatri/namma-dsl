module NammaDSL.Lib.Interpreter where

import Control.Monad.Writer hiding (Writer)
import qualified Data.List as L
import GHC.IO (unsafePerformIO)
import Kernel.Prelude
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Ppr as Ppr
import qualified Language.Haskell.TH.PprLib as Ppr
import qualified NammaDSL.Lib.MapName as MapName
import NammaDSL.Lib.Types
import qualified Text.PrettyPrint as Pretty

-- interpreter :: String -> Writer CodeUnit -> String
-- interpreter moduleName unitW = do
interpreter :: [String] -> Writer CodeUnit -> String
interpreter imports unitW = do
  let codeUnits = unsafePerformIO . TH.runQ . execWriterT $ unitW -- FIXME remove unsafe
  let codeTree = foldMap mkCodeTree codeUnits
  interpret imports codeTree

mkCodeTree :: CodeUnit -> CodeTree
mkCodeTree (CodeDec codeDecs) = mempty{codeDecs = codeDecs}
-- mkCodeTree (CodeImport codeImport) = mempty{codeImports = [codeImport]}
-- mkCodeTree (CodeExtension codeExtension) = mempty{codeExtensions = [codeExtension]}
mkCodeTree (CodeSplice codeSplice') = mempty{codeSplices = [codeSplice']}

interpret :: [String] -> CodeTree -> String
interpret imports CodeTree {..} = do
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
  interpretDecs imports codeDecs
    <> "\n\n"
    <> L.intercalate "\n\n" (interpretSplice imports <$> codeSplices)
    <> "\n\n"

interpretDecs :: [String] -> [TH.Dec] -> String
interpretDecs imports = pprint' . MapName.mapName @[TH.Dec] (nameModifier imports)

-- interpretImport :: Import -> String
-- interpretImport (Import m) = "import " <> m
-- interpretImport (ImportQualified m (Just syn)) = "import qualified " <> m <> " as " <> syn
-- interpretImport (ImportQualified m Nothing) = "import qualified " <> m

-- interpretExtension :: Extension -> String
-- interpretExtension (Extension e) = "{-# LANGUAGE " <> e <> " #-}"

interpretSplice :: [String] -> Splice -> String
interpretSplice imports (Splice e) = "$(" <> pprint' (MapName.mapName (nameModifier imports) e) <> ")"

myStyle :: Pretty.Style
myStyle = Pretty.style {Pretty.lineLength = 300}

pprint' :: Ppr.Ppr a => a -> String
pprint' x = Pretty.renderStyle myStyle $ Ppr.to_HPJ_Doc $ Ppr.ppr x

nameModifier :: [String] -> TH.Name -> TH.Name
nameModifier imports = MapName.mkNameModifier $ mapImportModules imports . mapReexportModules

mapReexportModules :: String -> String
mapReexportModules = \case
  "Kernel.Beam.Lib.UtilsTH" -> "Tools.Beam.UtilsTH"
  m -> m

mapImportModules :: [String] -> String -> Maybe String
mapImportModules imports modName | modName `notElem` imports = Nothing
mapImportModules _ modName = Just modName
