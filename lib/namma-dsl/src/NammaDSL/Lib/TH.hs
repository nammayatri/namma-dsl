{-# LANGUAGE TemplateHaskell #-}

module NammaDSL.Lib.TH where

import Control.Monad.Writer hiding (Writer)
import Kernel.Prelude
import qualified Language.Haskell.TH as TH
import NammaDSL.Lib.Types

(<--) :: TH.Q TH.Pat -> TH.Q TH.Exp -> Writer TH.Stmt
(<--) pQ eQ = do
  p <- lift pQ
  e <- lift eQ
  tell [TH.BindS p e]

infix 5 <--

varT :: String -> TH.Q TH.Type
varT = pure . TH.VarT . TH.mkName

conT :: String -> TH.Q TH.Type
conT = pure . TH.ConT . TH.mkName

varP :: String -> TH.Q TH.Pat
varP = pure . TH.VarP . TH.mkName

varE :: String -> TH.Q TH.Exp
varE = pure . TH.VarE . TH.mkName

conE :: String -> TH.Q TH.Exp
conE = pure . TH.ConE . TH.mkName

type_ :: String -> TH.Q TH.Type -> Writer CodeUnit
type_ name tQ = do
  t <- lift tQ
  tell [CodeDec [TH.TySynD (TH.mkName name) [] t]]

do_ :: Writer TH.Stmt -> TH.Q TH.Exp
do_ stmtW = do
  stmts <- execWriterT stmtW
  pure $ TH.DoE Nothing stmts

return_ :: TH.Q TH.Exp -> Writer TH.Stmt
return_ eQ = do
  e <- lift [e|return $eQ|]
  tell [TH.NoBindS e]

pure_ :: TH.Q TH.Exp -> Writer TH.Stmt
pure_ eQ = do
  e <- lift [e|pure $eQ|]
  tell [TH.NoBindS e]

let_ :: TH.Q [TH.Dec] -> Writer TH.Stmt
let_ dQ = do
  d <- lift dQ
  tell [TH.LetS d]

recUpd_ :: TH.Q TH.Exp -> Writer (TH.Name, TH.Exp) -> TH.Q TH.Exp
recUpd_ eQ tupleW = do
  tuples <- execWriterT tupleW
  e <- eQ
  pure $ TH.RecUpdE e tuples

fieldExp_ :: String -> TH.Q TH.Exp -> Writer (TH.Name, TH.Exp)
fieldExp_ name eQ = do
  e <- lift eQ
  tell [(TH.mkName name, e)]

recCon_ :: String -> Writer (TH.Name, TH.Exp) -> TH.Q TH.Exp
recCon_ name tupleW = do
  tuples <- execWriterT tupleW
  pure $ TH.RecConE (TH.mkName name) tuples

dec_ :: TH.Q TH.Dec -> Writer CodeUnit
dec_ decsQ = do
  dec <- lift decsQ
  tell [CodeDec [dec]]

decs_ :: TH.Q [TH.Dec] -> Writer CodeUnit
decs_ decsQ = do
  decs <- lift decsQ
  tell [CodeDec decs]

splice_ :: TH.Q TH.Exp -> Writer CodeUnit
splice_ action = do
  expr <- lift action
  tell . pure . CodeSplice . Splice $ expr
