{-# OPTIONS_GHC -Wno-orphans #-}

module NammaDSL.Lib.TH (module NammaDSL.Lib.TH, module Reexport) where

import Control.Monad.Writer hiding (Writer)
import Data.Data (Typeable, typeRep)
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.String
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH as Reexport hiding (Q, uInfixT, uInfixE, normalB, forallT, runQ, Code, listT, listE, tupE, tySynEqn, tupleT, tupP)
import NammaDSL.Lib.Types
import Text.Read (readEither)
import Prelude
import Control.Applicative
import qualified Data.Text as T

-- TODO to be removed
instance IsString TH.Name where
  fromString = TH.mkName

-- TODO to be removed
-- should not be used with qualified names
instance Semigroup TH.Name where
  a <> b = TH.mkName $ TH.nameBase a <> TH.nameBase b

vE :: String -> Q r TH.Exp
vE = pure . TH.VarE . TH.mkName

cE :: String -> Q r TH.Exp
cE = pure . TH.ConE . TH.mkName

vP :: String -> Q r TH.Pat
vP = pure . TH.VarP . TH.mkName

vT :: String -> Q r TH.Type
vT = pure . TH.VarT . TH.mkName

cT :: String -> Q r TH.Type
cT = pure . TH.ConT . TH.mkName

strE :: String -> Q r TH.Exp
strE = pure . TH.LitE . TH.StringL


(-->) :: Q r TH.Type -> Q r TH.Type -> Q r TH.Type
a --> b = uInfixT a (TH.mkName "->") b

infixr 1 -->

(~) :: Q r TH.Exp -> Q r TH.Exp -> Q r TH.Exp
(~) = liftA2 TH.AppE

(~~) :: Q r TH.Type -> Q r TH.Type -> Q r TH.Type
(~~) = liftA2 TH.AppT

infixl 2 ~~

--priority should be higher then for other operators
infixl 9 ~

uInfixT :: Q r TH.Type -> TH.Name -> Q r TH.Type -> Q r TH.Type
uInfixT a b c = TH.UInfixT <$> a <*> pure b <*> c

uInfixE :: Q r TH.Exp -> Q r TH.Exp -> Q r TH.Exp -> Q r TH.Exp
uInfixE = liftA3 TH.UInfixE

(~$) :: Q r TH.Exp -> Q r TH.Exp -> Q r TH.Exp
(~$) e1 = uInfixE e1 (vE "$")

infixr 0 ~$

(~<$>) :: Q r TH.Exp -> Q r TH.Exp -> Q r TH.Exp
(~<$>) e1 = uInfixE e1 (vE "<$>")

infixl 4 ~<$>

(~<*>) :: Q r TH.Exp -> Q r TH.Exp -> Q r TH.Exp
(~<*>) e1 = uInfixE e1 (vE "<*>")

infixl 4 ~<*>

(~<&>) :: Q r TH.Exp -> Q r TH.Exp -> Q r TH.Exp
(~<&>) e1 = uInfixE e1 (vE "<&>")

infixl 1 ~<&>

(~&) :: Q r TH.Exp -> Q r TH.Exp -> Q r TH.Exp
(~&) e1 = uInfixE e1 (vE "&")

infixl 1 ~&

(~.) :: Q r TH.Exp -> Q r TH.Exp -> Q r TH.Exp
(~.) e1 = uInfixE e1 (vE ".")

infixr 9 ~.

(~>>=) :: Q r TH.Exp -> Q r TH.Exp -> Q r TH.Exp
(~>>=) e1 = uInfixE e1 (vE ">>=")

infixl 1 ~>>=

-- based on Language.Haskell.TH.Lib, only Writer added instead of lists
tySynDW :: TH.Name -> [TH.TyVarBndr ()] -> Q r TH.Type -> Writer r CodeUnit
tySynDW name vars tQ = do
  t <- lift tQ
  tell [CodeDec [TH.TySynD name vars t]]

instanceDW :: Q r TH.Cxt -> Q r TH.Type -> Writer r TH.Dec -> Writer r CodeUnit
instanceDW cxtQ tQ dW = do
  ds <- lift $ execWriterT dW
  cxt_ <- lift cxtQ
  t <- lift tQ
  tell [CodeDec [TH.InstanceD Nothing cxt_ t ds]]

sigDW :: TH.Name -> Q r TH.Type -> Writer r TH.Dec
sigDW n tQ = do
  ts <- lift tQ
  tell [TH.SigD n ts]

funDW :: TH.Name -> Writer r TH.Clause -> Writer r TH.Dec
funDW n cW = do
  cs <- lift $ execWriterT cW
  tell [TH.FunD n cs]

itemW :: Q r a -> Writer r a
itemW aQ = do
  a <- lift aQ
  tell [a]

clauseW :: [Q r TH.Pat] -> Q r TH.Body -> Writer r TH.Clause
clauseW patsQ bodyQ = do
  pats <- lift $ sequenceA patsQ
  body <- lift bodyQ
  tell [TH.Clause pats body []]

doEW :: Writer r TH.Stmt -> Q r TH.Exp
doEW stmtW = do
  stmts <- execWriterT stmtW
  pure $ TH.DoE Nothing stmts

listEW :: Writer r TH.Exp -> Q r TH.Exp
listEW es = do
  e <- execWriterT es
  pure $ TH.ListE e

noBindSW :: Q r TH.Exp -> Writer r TH.Stmt
noBindSW eQ = do
  e <- lift eQ
  tell [TH.NoBindS e]

(<--) :: Q r TH.Pat -> Q r TH.Exp -> Writer r TH.Stmt
(<--) pQ eQ = do
  p <- lift pQ
  e <- lift eQ
  tell [TH.BindS p e]

infix 1 <--

tySynInstDW :: Q r TH.TySynEqn -> Writer r TH.Dec
tySynInstDW tQ = do
  t <- lift tQ
  tell [TH.TySynInstD t]

recConEW :: TH.Name -> Writer r (TH.Name, TH.Exp) -> Q r TH.Exp
recConEW name tupleW = do
  tuples <- execWriterT tupleW
  pure $ TH.RecConE name tuples

fieldExpW :: TH.Name -> Q r TH.Exp -> Writer r (TH.Name, TH.Exp)
fieldExpW name eQ = do
  e <- lift eQ
  tell [(name, e)]

type FieldDec = TH.VarBangType -- (TH.Name, TH.Bang, TH.Type)

defaultBang :: TH.Bang
defaultBang = TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness

fieldDecW :: TH.Name -> Q r TH.Type -> Writer r FieldDec
fieldDecW name tQ = do
  t <- lift tQ
  tell [(name, defaultBang, t)]

decW :: Q r TH.Dec -> Writer r CodeUnit
decW decQ = do
  dec <- lift decQ
  tell [CodeDec [dec]]

decsW :: Writer r TH.Dec -> Writer r CodeUnit
decsW dW = do
  ds <- lift $ execWriterT dW
  tell [CodeDec ds]

spliceW :: Q r TH.Exp -> Writer r CodeUnit
spliceW action = do
  expr <- lift action
  tell [CodeSplice $ Splice expr]

dataDW :: TH.Name -> [TH.TyVarBndr ()] -> Writer r TH.Con -> Writer r TH.DerivClause -> Writer r CodeUnit
dataDW name vars conW deriveW = do
  cons <- lift $ execWriterT conW
  derives <- lift $ execWriterT deriveW
  tell [CodeDec [TH.DataD [] name vars Nothing cons derives]]

derivClauseW :: Maybe TH.DerivStrategy -> [TH.Pred] -> Writer r TH.DerivClause
derivClauseW str preds = tell [TH.DerivClause str preds]

recCW :: TH.Name -> Writer r FieldDec -> Writer r TH.Con
recCW name fW = do
  fs <- lift $ execWriterT fW
  tell [TH.RecC name fs]

normalCW :: TH.Name -> [Q r TH.Type] -> Writer r TH.Con
normalCW name tsQ = do
  ts <- lift $ sequenceA tsQ
  tell [TH.NormalC name $ (defaultBang,) <$> ts]

dataInstDW :: Q r TH.Type -> Writer r TH.Con -> Writer r TH.DerivClause -> Writer r TH.Dec
dataInstDW tQ conW deriveW = do
  t <- lift tQ
  cons <- lift $ execWriterT conW
  derives <- lift $ execWriterT deriveW
  tell [TH.DataInstD [] Nothing t Nothing cons derives]

-- sometimes we can use Haskell code in spec, so we need to convert it to TH
--  pprint . readExp . read === id
class Typeable a => ReadExp a where
  readExp :: a -> TH.Exp

-- overlapping list instance
instance {-# OVERLAPPING #-} ReadExp String where
  readExp = TH.LitE . TH.StringL

instance (ReadExp a, ReadExp b) => ReadExp (a, b) where
  readExp (a, b) = TH.TupE [Just $ readExp a, Just $ readExp b]

instance ReadExp a => ReadExp [a] where
  readExp as = TH.ListE (readExp <$> as)

readEitherExp :: forall a. (Read a, ReadExp a) => Proxy a -> String -> Either String TH.Exp
readEitherExp _ = (readExp @a <$>) . readEither @a

-- FIXME use fail instead of error everywhere
readExpUnsafe :: forall a. (Read a, ReadExp a) => Proxy a -> String -> TH.Exp
readExpUnsafe proxy str = case readEitherExp @a proxy str of
  Right ex -> ex
  Left err -> error $ "Could not read expression: " <> show str <> " of type: " <> show (typeRep proxy) <> "; error :" <> show err

appendE :: NonEmpty (Q r TH.Exp) -> Q r TH.Exp
appendE = foldl1 (~)

appendInfixE :: Q r TH.Exp -> NonEmpty (Q r TH.Exp) -> Q r TH.Exp
appendInfixE operator = foldl1 (\acc e -> uInfixE acc operator e)

appendInfixT :: TH.Name -> NonEmpty (Q r TH.Type) -> Q r TH.Type
appendInfixT operator = foldr1 (\acc e -> uInfixT acc operator e)

appendArrow :: NonEmpty (Q r TH.Type) -> Q r TH.Type
appendArrow = appendInfixT "->"

appendT :: NonEmpty (Q r TH.Type) -> Q r TH.Type
appendT = foldl1 (~~)

normalB :: Q r TH.Exp -> Q r TH.Body
normalB = fmap TH.NormalB

forallT :: [TH.TyVarBndr TH.Specificity] -> [Q r TH.Type] -> Q r TH.Type -> Q r TH.Type
forallT a b = liftA2 (TH.ForallT a) (sequenceA b)

listT :: Q r TH.Type
listT = pure TH.ListT

listE :: [Q r TH.Exp] -> Q r TH.Exp
listE a = fmap TH.ListE (sequenceA a)

tupE :: [Maybe (Q r TH.Exp)] -> Q r TH.Exp
tupE a = fmap TH.TupE (traverse sequenceA a)

tupP :: [Q r TH.Pat] -> Q r TH.Pat
tupP a = fmap TH.TupP (sequenceA a)

tySynEqn :: Maybe [TH.TyVarBndr ()] -> Q r TH.Type -> Q r TH.Type -> Q r TH.TySynEqn
tySynEqn a = liftA2 (TH.TySynEqn a)

tupleT :: Int -> Q r TH.Type
tupleT a = pure (TH.TupleT a)

-- hack for record dots
(#.) :: String -> String -> String
(#.) a b = a <> "." <> b

mkNameT :: T.Text -> Name
mkNameT = TH.mkName . T.unpack

strT :: String -> Q r TH.Type
strT = pure . TH.LitT . TH.StrTyLit

promotedList1T :: String -> Q r TH.Type
promotedList1T str = cT $ "'[" <> str <> "]"
