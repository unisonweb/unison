{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}

module Unison.Syntax.Type where

import Control.Applicative
import Data.Maybe
import Data.Traversable
import Unison.Syntax.Var as V
import Unison.Syntax.DeBruijn as D

data T = Mono | Poly
type Monotype = Type Mono
type Polytype = Type Poly
-- | Types with constraints `c`, free variables in `v` and kind annotations in `k`

data Type (t :: T) c k v where
  Unit :: Type t c k v
  Arrow :: Type t c k v -> Type t c k v -> Type t c k v
  Universal :: v -> Type t c k v
  Existential :: v -> Type t c k v
  Ann :: Type t c k v -> k -> Type t c k v
  Constrain :: Type t c k v -> c -> Type t c k v
  Forall :: v -> Type Poly c k v -> Type Poly c k v -- | ^ `DeBruijn 1` is bounded by nearest enclosing `Forall`, `DeBruijn 2` by next enclosing `Forall`, etc

  -- deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable)
deriving instance (Eq c, Eq k, Eq v) => Eq (Type t c k v)
deriving instance (Ord c, Ord k, Ord v) => Ord (Type t c k v)
deriving instance (Show c, Show k, Show v) => Show (Type t c k v)

trav :: Applicative f => (v -> f v2) -> Type Poly c k v -> f (Type Poly c k v2)
trav f Unit = pure Unit
trav f (Arrow i o) = Arrow <$> trav f i <*> trav f o
trav f (Universal v) = Universal <$> f v
trav f (Existential v) = Existential <$> f v
trav f (Ann t k) = Ann <$> trav f t <*> pure k
trav f (Constrain t c) = Constrain <$> trav f t <*> pure c
trav f (Forall v fn) = Forall <$> f v <*> trav f fn

monotype :: Type Poly c k v -> Maybe (Monotype c k v)
monotype Unit = pure Unit
monotype (Arrow i o) = Arrow <$> monotype i <*> monotype o
monotype (Universal v) = pure (Universal v)
monotype (Existential v) = pure (Existential v)
monotype (Ann t k) = Ann <$> monotype t <*> pure k
monotype (Constrain t c) = Constrain <$> monotype t <*> pure c
monotype _ = Nothing

-- need to call this inside out
abstract1 :: Eq v => v -> Type Poly c k v -> Maybe (Type Poly c k (Var v2))
abstract1 v = trav go where
  go v2 | v2 == v = Just V.bound1
  go _ = Nothing

abstract :: Eq v => Var v
         -> Type Poly c k (Var v)
         -> ([Var v], Type Poly c k (Var v))
abstract v = trav go where
  go v2 | v2 == v    = ([], V.bound1)
  go v2 | otherwise  = ([v2], v2)

-- | Type variable which is bound by the nearest enclosing `Forall`
bound1 :: Type t c k (Var v)
bound1 = Universal V.bound1

-- forall1 $ \x -> Arrow x x
-- forall2
forall1 :: (forall v . Type t c k v -> Type Poly c k v) -> Type Poly c k (Var v2)
forall1 f = Forall V.bound1 . fromJust . abstract1 () . f $ Universal ()

subst1 :: Eq v => Type t c k (Var v) -> Type t c k (Var v) -> Type t c k (Var v)
subst1 fn arg = subst fn V.bound1 arg

-- | mnemonic `subst fn var=arg`
subst :: Eq v
      => Type t c k (Var v)
      -> Var v
      -> Type t c k (Var v)
      -> Type t c k (Var v)
subst fn var arg = case fn of
  Unit -> Unit
  Arrow i o -> Arrow (subst i var arg) (subst o var arg)
  Universal v | v == var -> arg
              | otherwise -> fn
  Existential v | v == var -> arg
                | otherwise -> fn
  Ann fn' t -> Ann (subst fn' var arg) t
  Constrain fn' t -> Constrain (subst fn' var arg) t
  Forall v fn' -> Forall v (subst fn' (V.succ var) arg)
