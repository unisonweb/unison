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
import Unison.Syntax.DeBruijn as DeBruijn

data T = Mono | Poly

-- | Types with constraints `c`, free variables in `v` and kind annotations in `k`

data Type (t :: T) c k v where
  Unit :: Type t c k v
  Arrow :: Type t c k v -> Type t c k v -> Type t c k v
  Var :: Either DeBruijn v -> Type t c k v
  Exists :: Either DeBruijn v -> Type t c k v
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
  go (Right v2) | v2 == v = Just bind0
  go _ = Nothing

abstract :: Eq v => v -> Type t c k v -> ([v], Type t c k v)
abstract v = collect go where
  go (Right v2) | v2 == v = ([], bind0)
  go (Right v2) = ([v2], Var (Right v2))
  go (Left v2) = ([v2], Exists (Right v2))

bind0 :: Type t c k v
bind0 = Var (Left (DeBruijn 0))

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
    Var (Right v) -> f (Right v)
    Var (Left ind) -> pure (Var (Left ind))
    Exists (Right v) -> f (Left v)
    Exists (Left ind) -> pure (Exists (Left ind))
    Ann e' t -> Ann <$> go e' <*> pure t
    Constrain e' t -> Constrain <$> go e' <*> pure t
    Forall body -> Forall <$> go body

forall1 :: (forall v . Type t c k v -> Type Poly c k v) -> Type Poly c k v2
forall1 f = Forall . fromJust . abstract1 (0 :: Int) . f $ Var (Right 0)

subst1 :: Type t c k v -> Type t c k v -> Type t c k v
subst1 = go (DeBruijn 0) where
  go ind body e = case body of
    Unit -> Unit
    Arrow i o -> Arrow (go ind i e) (go ind o e)
    Var (Left i) | i == ind -> e
    Var _ -> body
    Exists (Left i) | i == ind -> e
    Exists _ -> body
    Ann body' t -> Ann (go ind body' e) t
    Constrain body' t -> Constrain (go ind body' e) t
    Forall body' -> Forall (go (DeBruijn.succ ind) body' e)

vars :: Type t c k v -> [v]
vars = getConst . collect (\e -> Const [either id id e])
