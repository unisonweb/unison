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
import Unison.Syntax.Var as V
import Unison.Syntax.DeBruijn as D

data T = Mono | Poly
type Monotype = Type Mono
type Polytype = Type Poly
-- | Types with constraints `c`, free variables in `v` and kind annotations in `k`

data Type (t :: T) c k v where
  Unit :: Type t c k v
  Arrow :: Type t c k v -> Type t c k v -> Type t c k v
  Var :: Var v -> Type t c k v
  Exists :: Var v -> Type t c k v
  Ann :: Type t c k v -> k -> Type t c k v
  Constrain :: Type t c k v -> c -> Type t c k v
  Forall :: Type Poly c k v -> Type Poly c k v

  -- deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable)
deriving instance (Eq c, Eq k, Eq v) => Eq (Type t c k v)
deriving instance (Ord c, Ord k, Ord v) => Ord (Type t c k v)
deriving instance (Show c, Show k, Show v) => Show (Type t c k v)
-- deriving instance (Read c, Read k, Read v) => Read (Type t c k v)

abstract1 :: Eq v => v -> Type t c k v -> Maybe (Type t c k v2)
abstract1 v = collect go where
  go (Right v2) | v2 == v = Just (Var V.bound1)
  go _ = Nothing

abstract :: Eq v => v -> Type t c k v -> ([v], Type t c k v)
abstract v = collect go where
  go (Right v2) | v2 == v = ([], (Var V.bound1))
  go (Right v2) = ([v2], Var (Free v2))
  go (Left v2) = ([v2], Exists (Free v2))

-- | Type variable which is bound by the nearest enclosing `Forall`
bound1 :: Type t c k v
bound1 = Var V.bound1

closed :: Type t c k v -> Maybe (Type t c k v2)
closed = collect (const Nothing)

collect :: Applicative f
       => (Either v v -> f (Type t c k v2)) -- `Left` is existential, `Right` is variable
       -> Type t c k v
       -> f (Type t c k v2)
collect f = go where
  go e = case e of
    Unit  -> pure Unit
    Arrow i o -> Arrow <$> go i <*> go o
    Var (Free v) -> f (Right v)
    Var (Bound ind) -> pure (Var (Bound ind))
    Exists (Free v) -> f (Left v)
    Exists (Bound ind) -> pure (Exists (Bound ind))
    Ann e' t -> Ann <$> go e' <*> pure t
    Constrain e' t -> Constrain <$> go e' <*> pure t
    Forall body -> Forall <$> go body

forall1 :: (forall v . Type t c k v -> Type Poly c k v) -> Type Poly c k v2
forall1 f = Forall . fromJust . abstract1 () . f $ Var (Free ())

subst1 :: Type t c k v -> Type t c k v -> Type t c k v
subst1 = go D.bound1 where
  go ind body e = case body of
    Unit -> Unit
    Arrow i o -> Arrow (go ind i e) (go ind o e)
    Var (Bound i) | i == ind -> e
    Var _ -> body
    Exists (Bound i) | i == ind -> e
    Exists _ -> body
    Ann body' t -> Ann (go ind body' e) t
    Constrain body' t -> Constrain (go ind body' e) t
    Forall body' -> Forall (go (D.succ ind) body' e)

vars :: Type t c k v -> [v]
vars = getConst . collect (\e -> Const [either id id e])
