module NammaDSL.Lib.MapName where

import qualified GHC.Generics as G
import Kernel.Prelude
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

mkNameModifier :: (String -> Maybe String) -> TH.Name -> TH.Name
mkNameModifier modifier (TH.Name occName (TH.NameQ (TH.ModName modName))) = case modifier modName of
  Just modifiedName -> TH.Name occName (TH.NameQ (TH.ModName modifiedName))
  Nothing -> TH.Name occName TH.NameS
mkNameModifier modifier (TH.Name occName (TH.NameG s g (TH.ModName modName))) = case modifier modName of
  Just modifiedName -> TH.Name occName (TH.NameG s g (TH.ModName modifiedName))
  Nothing -> TH.Name occName TH.NameS
mkNameModifier _ n = n

removeQualifiedModules :: TH.Name -> TH.Name
removeQualifiedModules = mkNameModifier (const Nothing)

-- modify TH.Name everywhere in TH data types tree
class MapName a where
  mapName :: (TH.Name -> TH.Name) -> a -> a
  default mapName :: (Generic a, MapName (G.Rep a ())) => (TH.Name -> TH.Name) -> a -> a
  mapName = mapNameGeneric

mapNameGeneric :: forall a. (Generic a, MapName (G.Rep a ())) => (TH.Name -> TH.Name) -> a -> a
mapNameGeneric f = G.to . mapName @(G.Rep a ()) f . G.from

-- modify TH.Name itself
instance MapName TH.Name where
  mapName = identity

-- modify TH.Name inside of tree
instance MapName a => MapName (G.K1 r a ()) where
  mapName func (G.K1 a) = G.K1 (mapName func a)

instance MapName (f ()) => MapName (G.M1 i c f ()) where
  mapName func (G.M1 a) = G.M1 (mapName func a)

instance (MapName (f ()), MapName (g ())) => MapName ((f G.:+: g) ()) where
  mapName func (G.L1 f) = G.L1 $ mapName func f
  mapName func (G.R1 g) = G.R1 $ mapName func g

instance (MapName (f ()), MapName (g ())) => MapName ((f G.:*: g) ()) where
  mapName func (f G.:*: g) = mapName func f G.:*: mapName func g

-- nothing to modify
instance MapName (G.U1 ()) where
  mapName = const identity

instance MapName Int where
  mapName = const identity

instance MapName () where
  mapName = const identity

instance MapName Char where
  mapName = const identity

instance MapName a => MapName [a] where
  mapName func = (mapName func <$>)

instance MapName a => MapName (Maybe a) where
  mapName func = (mapName func <$>)

instance MapName a => MapName (NonEmpty a) where
  mapName func = (mapName func <$>)

instance (MapName a, MapName b) => MapName (a, b)

instance (MapName a, MapName b, MapName c) => MapName (a, b, c)

-- hack for removing qualified name in binding position:
-- instance Foo Bar where
--   type RemovedImport.FooType Bar = ...
--   RemovedImport.foo = ...
instance MapName TH.Dec where
  mapName f (TH.InstanceD a b c decs) = TH.InstanceD (mapName f a) (mapName f b) (mapName f c) (removeBinding <$> decs)
    where
      removeBinding (TH.FunD name clauses) = TH.FunD (removeQualifiedModules name) (mapName f clauses)
      removeBinding (TH.ValD (TH.VarP name) d e) = TH.ValD (TH.VarP (removeQualifiedModules name)) (mapName f d) (mapName f e)
      removeBinding (TH.TySynInstD (TH.TySynEqn d (TH.AppT (TH.ConT name) e) g)) = do
        let (d', e', g') = (mapName f d, mapName f e, mapName f g)
        TH.TySynInstD (TH.TySynEqn d' (TH.AppT (TH.ConT (removeQualifiedModules name)) e') g')
      removeBinding d = error $ "failed to remove binding: " <> show d -- mapNameGeneric @TH.Dec f d
  mapName f d = mapNameGeneric @TH.Dec f d

instance MapName TH.Exp

instance MapName TH.Clause

instance MapName TH.Pat

instance MapName TH.Lit where
  mapName = const identity

instance MapName TH.Body

instance MapName TH.Guard

instance MapName TH.Stmt

instance MapName TH.Type

instance MapName TH.Con

instance MapName TH.Bang where
  mapName = const identity

instance MapName TH.Foreign

instance MapName TH.Fixity where
  mapName = const identity

instance MapName TH.Pragma

instance MapName TH.TySynEqn

instance MapName TH.RuleMatch where
  mapName = const identity

instance MapName TH.Phases where
  mapName = const identity

instance MapName TH.Inline where
  mapName = const identity

instance MapName TH.DerivClause

instance MapName TH.FunDep

instance MapName TH.DerivStrategy

instance MapName (TH.TyVarBndr ())

instance MapName (TH.TyVarBndr TH.Specificity)

instance MapName TH.Overlap where
  mapName = const identity

instance MapName TH.TypeFamilyHead

instance MapName TH.Role where
  mapName = const identity

instance MapName TH.PatSynArgs

instance MapName TH.PatSynDir

instance MapName TH.TyLit where
  mapName = const identity

instance MapName TH.Callconv where
  mapName = const identity

instance MapName TH.Safety where
  mapName = const identity

instance MapName TH.RuleBndr

instance MapName TH.AnnTarget

instance MapName TH.FamilyResultSig

instance MapName TH.Specificity where
  mapName = const identity

instance MapName TH.InjectivityAnn

instance MapName TH.Match

instance MapName TH.ModName where
  mapName = const identity

instance MapName TH.Range
