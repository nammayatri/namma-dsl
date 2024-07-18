module NammaDSL.Lib.Types where

import Control.Monad.Reader
import Control.Monad.Writer hiding (Writer)
import Data.Function (flip)
-- import Data.Semigroup
import qualified Language.Haskell.TH as TH
import Prelude (Eq, String)

-- In future this monad can include TH.Q or error handling, now these effects are not used
type Q r = Reader r

-- type Q = ReaderT TableDef TH.Q

runQ :: r -> Q r a -> a
runQ = flip runReader

-- data CodeUnit = CodeDec [TH.Dec] | CodeImport Import | CodeExtension Extension | CodeSplice Splice

data CodeUnit = CodeDec [TH.Dec] | CodeSplice Splice | CodeComment Comment

-- data Import = Import String | ImportQualified String (Maybe String)

-- newtype Extension = Extension String

newtype Splice = Splice TH.Exp deriving (Eq)

newtype Comment = Comment String

type Writer r w = WriterT [w] (Q r) ()

-- we can use CodeTree at once instead of [CodeUnit]
-- FIXME use existing GeneratorInput
-- data CodeTree = CodeTree
--   { -- codeImports :: [Import],
--     codeDecs :: [TH.Dec],
--     codeSplices :: [Splice]
--     --codeExtensions :: [Extension]
--   }
--   deriving (Eq)

-- instance Monoid CodeTree where
--   mempty =
--     CodeTree
--       { --codeImports = mempty,
--         codeDecs = mempty,
--         codeSplices = mempty
--         --codeExtensions = mempty
--       }

-- instance Semigroup CodeTree where
--   a <> b =
--     CodeTree
--       { --codeImports = codeImports a <> codeImports b,
--         codeDecs = codeDecs a <> codeDecs b,
--         codeSplices = codeSplices a <> codeSplices b
--         --codeExtensions = codeExtensions a <> codeExtensions b
--       }
