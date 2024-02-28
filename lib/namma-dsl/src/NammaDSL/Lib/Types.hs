module NammaDSL.Lib.Types where

import Control.Monad.Writer hiding (Writer)
import Data.Semigroup
import qualified Language.Haskell.TH as TH

-- data CodeUnit = CodeDec [TH.Dec] | CodeImport Import | CodeExtension Extension | CodeSplice Splice

data CodeUnit = CodeDec [TH.Dec] | CodeSplice Splice

-- data Import = Import String | ImportQualified String (Maybe String)

-- newtype Extension = Extension String

newtype Splice = Splice TH.Exp

type Writer w = WriterT [w] TH.Q ()

-- we can use CodeTree at once instead of [CodeUnit]
-- FIXME use existing GeneratorInput
data CodeTree = CodeTree
  { -- codeImports :: [Import],
    codeDecs :: [TH.Dec],
    codeSplices :: [Splice]
    --codeExtensions :: [Extension]
  }

instance Monoid CodeTree where
  mempty =
    CodeTree
      { --codeImports = mempty,
        codeDecs = mempty,
        codeSplices = mempty
        --codeExtensions = mempty
      }

instance Semigroup CodeTree where
  a <> b =
    CodeTree
      { --codeImports = codeImports a <> codeImports b,
        codeDecs = codeDecs a <> codeDecs b,
        codeSplices = codeSplices a <> codeSplices b
        --codeExtensions = codeExtensions a <> codeExtensions b
      }
