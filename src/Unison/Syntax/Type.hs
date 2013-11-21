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
import Data.Set as S
import Unison.Syntax.Var as V

-- constructor is private not exported
data Monotype c k v = Monotype { getPolytype :: Type c k v }
deriving instance (Eq c, Eq k, Eq v) => Eq (Monotype c k v)

-- | Types with constraints `c`, free variables in `v` and kind annotations in `k`
data Type c k v where
  Unit :: Type c k v
  Arrow :: Type c k v -> Type c k v -> Type c k v
  Universal :: v -> Type c k v
  Existential :: v -> Type c k v
  Ann :: Type c k v -> k -> Type c k v
  Constrain :: Type c k v -> c -> Type c k v
  Forall :: v -> Type c k v -> Type c k v -- | ^ `DeBruijn 1` is bounded by nearest enclosing `Forall`, `DeBruijn 2` by next enclosing `Forall`, etc

  -- deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable)
deriving instance (Eq c, Eq k, Eq v) => Eq (Type c k v)
deriving instance (Ord c, Ord k, Ord v) => Ord (Type c k v)
deriving instance (Show c, Show k, Show v) => Show (Type c k v)

trav :: Applicative f => (v -> f v2) -> Type c k v -> f (Type c k v2)
trav _ Unit = pure Unit
trav f (Arrow i o) = Arrow <$> trav f i <*> trav f o
trav f (Universal v) = Universal <$> f v
trav f (Existential v) = Existential <$> f v
trav f (Ann t k) = Ann <$> trav f t <*> pure k
trav f (Constrain t c) = Constrain <$> trav f t <*> pure c
trav f (Forall v fn) = Forall <$> f v <*> trav f fn

monotype :: Type c k v -> Maybe (Monotype c k v)
monotype t = Monotype <$> go t where
  go Unit = pure Unit
  go (Arrow i o) = Arrow <$> go i <*> go o
  go (Universal v) = pure (Universal v)
  go (Existential v) = pure (Existential v)
  go (Ann t' k) = Ann <$> go t' <*> pure k
  go (Constrain t' c) = Constrain <$> go t' <*> pure c
  go _ = Nothing

-- need to call this inside out
abstract1 :: Eq v => v -> Type c k v -> Maybe (Type c k (Var v2))
abstract1 v = trav go where
  go v2 | v2 == v = Just V.bound1
  go _ = Nothing

abstract :: Eq v => Var v
         -> Type c k (Var v)
         -> ([Var v], Type c k (Var v))
abstract v = trav go where
  go v2 | v2 == v    = ([], V.bound1)
  go v2 | otherwise  = ([v2], v2)

-- | Type variable which is bound by the nearest enclosing `Forall`
bound1 :: Type c k (Var v)
bound1 = Universal V.bound1

-- forall1 $ \x -> Arrow x x
-- forall2
forall1 :: (forall v . Type c k v -> Type c k v) -> Type c k (Var v2)
forall1 f = Forall V.bound1 . fromJust . abstract1 () . f $ Universal ()

subst1 :: Eq v => Type c k (Var v) -> Type c k (Var v) -> Type c k (Var v)
subst1 fn arg = subst fn V.bound1 arg

-- | mnemonic `subst fn var=arg`
subst :: Eq v
      => Type c k (Var v)
      -> Var v
      -> Type c k (Var v)
      -> Type c k (Var v)
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

-- | The set of unbound variables in this type
freeVars :: Ord v => Type c k v -> Set v
freeVars t = case t of
  Unit -> S.empty
  Arrow i o -> S.union (freeVars i) (freeVars o)
  Universal v -> S.singleton v
  Existential v -> S.singleton v
  Ann fn _ -> freeVars fn
  Constrain fn _ -> freeVars fn
  Forall v fn -> S.delete v (freeVars fn)
