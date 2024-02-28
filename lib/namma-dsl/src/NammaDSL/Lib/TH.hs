{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module NammaDSL.Lib.TH where

import Control.Monad.Writer hiding (Writer)
import Data.Semigroup
import Data.String
import qualified Language.Haskell.TH as TH
import NammaDSL.Lib.Types
import Prelude

instance IsString TH.Name where
  fromString = TH.mkName

-- should not be used with qualified names
instance Semigroup TH.Name where
  a <> b = TH.mkName $ TH.nameBase a <> TH.nameBase b

-- based on Language.Haskell.TH.Lib, only Writer added instead of lists
tySynDW :: TH.Name -> [TH.TyVarBndr ()] -> TH.Q TH.Type -> Writer CodeUnit
tySynDW name vars tQ = do
  t <- lift tQ
  tell [CodeDec [TH.TySynD name vars t]]

instanceDW :: TH.Q TH.Cxt -> TH.Q TH.Type -> Writer TH.Dec -> Writer CodeUnit
instanceDW cxtQ tQ dW = do
  ds <- lift $ execWriterT dW
  cxt <- lift cxtQ
  t <- lift tQ
  tell [CodeDec [TH.InstanceD Nothing cxt t ds]]

funDW :: TH.Name -> Writer TH.Clause -> Writer TH.Dec
funDW n cW = do
  cs <- lift $ execWriterT cW
  tell [TH.FunD n cs]

clauseW :: [TH.Q TH.Pat] -> TH.Q TH.Body -> Writer TH.Clause
clauseW patsQ bodyQ = do
  pats <- lift $ sequenceA patsQ
  body <- lift bodyQ
  tell [TH.Clause pats body []]

doEW :: Writer TH.Stmt -> TH.Q TH.Exp
doEW stmtW = do
  stmts <- execWriterT stmtW
  pure $ TH.DoE Nothing stmts

(<--) :: TH.Q TH.Pat -> TH.Q TH.Exp -> Writer TH.Stmt
(<--) pQ eQ = do
  p <- lift pQ
  e <- lift eQ
  tell [TH.BindS p e]

infix 5 <--

pureW :: TH.Q TH.Exp -> Writer TH.Stmt
pureW eQ = do
  e <- lift [e|pure $eQ|]
  tell [TH.NoBindS e]

tySynInstDW :: TH.Q TH.TySynEqn -> Writer TH.Dec
tySynInstDW tQ = do
  t <- lift tQ
  tell [TH.TySynInstD t]

recConEW :: TH.Name -> Writer (TH.Name, TH.Exp) -> TH.Q TH.Exp
recConEW name tupleW = do
  tuples <- execWriterT tupleW
  pure $ TH.RecConE name tuples

fieldExpW :: TH.Name -> TH.Q TH.Exp -> Writer (TH.Name, TH.Exp)
fieldExpW name eQ = do
  e <- lift eQ
  tell [(name, e)]

decW :: TH.Q TH.Dec -> Writer CodeUnit
decW decsQ = do
  dec <- lift decsQ
  tell [CodeDec [dec]]

decsW :: TH.Q [TH.Dec] -> Writer CodeUnit
decsW decsQ = do
  decs <- lift decsQ
  tell [CodeDec decs]

spliceW :: TH.Q TH.Exp -> Writer CodeUnit
spliceW action = do
  expr <- lift action
  tell . pure . CodeSplice . Splice $ expr
