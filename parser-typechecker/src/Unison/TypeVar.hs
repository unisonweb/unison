{-# Language DeriveFunctor #-}

module Unison.TypeVar where

import qualified Data.Set as Set
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

instance Var v => Var (TypeVar b v) where
  rename n (Universal v) = Universal (Var.rename n v)
  rename n (Existential b v) = Existential b (Var.rename n v)
  named txt = Universal (Var.named txt)
  name v = Var.name (underlying v)
  qualifiedName v = Var.qualifiedName (underlying v)
  freshIn s v = Var.freshIn (Set.map underlying s) <$> v
  freshenId id v = Var.freshenId id <$> v
  clear v = Var.clear <$> v
