module NammaDSL.Lib.Types where

import Control.Monad.Reader
import Control.Monad.Writer hiding (Writer)
import Data.Function (flip)
import Data.Semigroup
import qualified Language.Haskell.TH as TH
import Prelude (Eq, Monoid (mempty))

-- In future this monad can include TH.Q or error handling, now these effects are not used
type Q r = Reader r

-- type Q = ReaderT TableDef TH.Q

runQ :: r -> Q r a -> a
runQ = flip runReader

-- data CodeUnit = CodeDec [TH.Dec] | CodeImport Import | CodeExtension Extension | CodeSplice Splice

-- CodeRawDec: a declaration as literal text, bypassing Ppr.
-- Needed for 3+-link chains of a custom infix type op (e.g. :<|>, :>) --
-- template-haskell's Ppr always re-parenthesizes those, see NammaDSL.Lib.TH.
data CodeUnit = CodeDec [TH.Dec] | CodeSplice Splice | CodeRawDec String

-- data Import = Import String | ImportQualified String (Maybe String)

-- newtype Extension = Extension String

newtype Splice = Splice TH.Exp deriving (Eq)

type Writer r w = WriterT [w] (Q r) ()

-- we can use CodeTree at once instead of [CodeUnit]
-- FIXME use existing GeneratorInput
data CodeTree = CodeTree
  { -- codeImports :: [Import],
    codeDecs :: [TH.Dec],
    codeSplices :: [Splice],
    codeRawDecs :: [String]
    --codeExtensions :: [Extension]
  }
  deriving (Eq)

instance Monoid CodeTree where
  mempty =
    CodeTree
      { --codeImports = mempty,
        codeDecs = mempty,
        codeSplices = mempty,
        codeRawDecs = mempty
        --codeExtensions = mempty
      }

instance Semigroup CodeTree where
  a <> b =
    CodeTree
      { --codeImports = codeImports a <> codeImports b,
        codeDecs = codeDecs a <> codeDecs b,
        codeSplices = codeSplices a <> codeSplices b,
        codeRawDecs = codeRawDecs a <> codeRawDecs b
        --codeExtensions = codeExtensions a <> codeExtensions b
      }
