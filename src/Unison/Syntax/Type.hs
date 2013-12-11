{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}

module Unison.Syntax.Type where

import Control.Applicative
import Data.Maybe
import Data.Set as S
import Unison.Syntax.Var as V
import qualified Unison.Syntax.Kind as K

-- constructor is private not exported
data Monotype l c = Monotype { getPolytype :: Type l c }
deriving instance (Eq l, Eq c) => Eq (Monotype l c)

-- | Types with constraints `c`, free variables in `v` and kind annotations in `k`
data Type l c
  = Unit l
  | Arrow (Type l c) (Type l c)
  | Universal V.Var
  | Existential V.Var
  | Ann (Type l c) K.Kind
  | Constrain (Type l c) c
  | Forall V.Var (Type l c) -- | ^ `DeBruijn 1` is bounded by nearest enclosing `Forall`, `DeBruijn 2` by next enclosing `Forall`, etc

  -- deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable)
deriving instance (Eq l, Eq c) => Eq (Type l c)
deriving instance (Ord l, Ord c) => Ord (Type l c)
deriving instance (Show l, Show c) => Show (Type l c)

trav :: Applicative f => (V.Var -> f V.Var) -> Type l c -> f (Type l c)
trav _ (Unit l) = pure (Unit l)
trav f (Arrow i o) = Arrow <$> trav f i <*> trav f o
trav f (Universal v) = Universal <$> f v
trav f (Existential v) = Existential <$> f v
trav f (Ann t k) = Ann <$> trav f t <*> pure k
trav f (Constrain t c) = Constrain <$> trav f t <*> pure c
trav f (Forall v fn) = Forall <$> f v <*> trav f fn

monotype :: Type l c -> Maybe (Monotype l c)
monotype t = Monotype <$> go t where
  go (Unit l) = pure (Unit l)
  go (Arrow i o) = Arrow <$> go i <*> go o
  go (Universal v) = pure (Universal v)
  go (Existential v) = pure (Existential v)
  go (Ann t' k) = Ann <$> go t' <*> pure k
  go (Constrain t' c) = Constrain <$> go t' <*> pure c
  go _ = Nothing

-- need to call this inside out
abstract1 :: V.Var -> Type l c -> Maybe (Type l c)
abstract1 v = trav go where
  go v2 | v2 == v = Just V.bound1
  go _ = Nothing

abstract :: V.Var
         -> Type l c
         -> ([V.Var], Type l c)
abstract v = trav go where
  go v2 | v2 == v    = ([], V.bound1)
  go v2 | otherwise  = ([v2], v2)

-- | Type variable which is bound by the nearest enclosing `Forall`
bound1 :: Type l c
bound1 = Universal V.bound1

-- forall1 $ \x -> Arrow x x
-- forall2
-- just use negative values here
forall1 :: (Type l c -> Type l c) -> Type l c
forall1 f =
  let unused = V.decr V.bound1
  in Forall V.bound1 . fromJust . abstract1 unused . f $ Universal unused

subst1 :: Type l c -> Type l c -> Type l c
subst1 fn arg = subst fn V.bound1 arg

-- | mnemonic `subst fn var=arg`
subst :: Type l c -> V.Var -> Type l c -> Type l c
subst fn var arg = case fn of
  Unit l -> Unit l
  Arrow i o -> Arrow (subst i var arg) (subst o var arg)
  Universal v | v == var -> arg
              | otherwise -> fn
  Existential v | v == var -> arg
                | otherwise -> fn
  Ann fn' t -> Ann (subst fn' var arg) t
  Constrain fn' t -> Constrain (subst fn' var arg) t
  Forall v fn' -> Forall v (subst fn' (V.succ var) arg)

-- | The set of unbound variables in this type
freeVars :: Type l c -> Set V.Var
freeVars t = case t of
  Unit _ -> S.empty
  Arrow i o -> S.union (freeVars i) (freeVars o)
  Universal v -> S.singleton v
  Existential v -> S.singleton v
  Ann fn _ -> freeVars fn
  Constrain fn _ -> freeVars fn
  Forall v fn -> S.delete v (freeVars fn)
