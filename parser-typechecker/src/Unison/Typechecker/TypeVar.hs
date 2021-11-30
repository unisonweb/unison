{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE PatternSynonyms #-}

module Unison.Typechecker.TypeVar where

import qualified Data.Set as Set
import qualified Unison.ABT as ABT
import qualified Unison.Term as Term
import           Unison.Term (Term, Term')
import           Unison.Type (Type)
import           Unison.Var (Var)
import qualified Unison.Var as Var

data TypeVar b v = Universal v | Existential b v deriving (Functor)

instance Eq v => Eq (TypeVar b v) where
  Universal v == Universal v2 = v == v2
  Existential _ v == Existential _ v2 = v == v2
  _ == _ = False

instance Ord v => Ord (TypeVar b v) where
  Universal v `compare` Universal v2 = compare v v2
  Existential _ v `compare` Existential _ v2 = compare v v2
  Universal _ `compare` Existential _ _ = LT
  _ `compare` _ = GT

underlying :: TypeVar b v -> v
underlying (Universal v) = v
underlying (Existential _ v) = v

instance Show v => Show (TypeVar b v) where
  show (Universal v) = show v
  show (Existential _ v) = "'" ++ show v

instance ABT.Var v => ABT.Var (TypeVar b v) where
  freshIn s v = ABT.freshIn (Set.map underlying s) <$> v

instance Var v => Var (TypeVar b v) where
  typed t = Universal (Var.typed t)
  typeOf v = Var.typeOf (underlying v)
  freshId v = Var.freshId (underlying v)
  freshenId id v = Var.freshenId id <$> v

liftType :: Ord v => Type v a -> Type (TypeVar b v) a
liftType = ABT.vmap Universal

lowerType :: Ord v => Type (TypeVar b v) a -> Type v a
lowerType = ABT.vmap underlying

liftTerm :: Ord v => Term v a -> Term' (TypeVar b v) v a
liftTerm = Term.vtmap Universal

lowerTerm :: Ord v => Term' (TypeVar b v) v a -> Term v a
lowerTerm = Term.vtmap underlying
