{-# Language DeriveFunctor #-}

module Unison.TypeVar where

import Unison.Var (Var)
import qualified Data.Set as Set
import qualified Unison.Var as Var

data TypeVar v = Universal v | Existential v deriving (Eq,Ord,Functor)

underlying :: TypeVar v -> v
underlying (Universal v) = v
underlying (Existential v) = v

instance Show v => Show (TypeVar v) where
  show (Universal v) = show v
  show (Existential v) = "'" ++ show v

instance Var v => Var (TypeVar v) where
  rename n (Universal v) = Universal (Var.rename n v)
  rename n (Existential v) = Existential (Var.rename n v)
  named txt = Universal (Var.named txt)
  name v = Var.name (underlying v)
  qualifiedName v = Var.qualifiedName (underlying v)
  freshIn s v = Var.freshIn (Set.map underlying s) <$> v
  freshenId id v = Var.freshenId id <$> v
  clear v = Var.clear <$> v
