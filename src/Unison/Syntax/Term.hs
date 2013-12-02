{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE Rank2Types #-}

module Unison.Syntax.Term where

import Control.Applicative
import Control.Lens
import Data.Foldable
import Data.Maybe
import Unison.Syntax.Var as V
import Unison.Syntax.DeBruijn as D

type ClosedTerm l t = forall v. Term l t (Var v)

-- | Terms with free variables in `v`, type annotations in `t`,
-- and literals in `k`.
data Term l t v
  = Var v
  | Lit l
  | App (Term l t v) (Term l t v)
  | Ann (Term l t v) t
  | Lam (Term l t v)
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

abstract1 :: Eq v => v -> Term l t (Var v) -> Maybe (Term l t (Var v2))
abstract1 v = collect go where
  go (V.Free v2) | v2 == v = Just (Var V.bound1)
  go _ = Nothing

abstract :: Eq v => v -> Term l t (Var v) -> ([v], Term l t (Var v))
abstract v = collect go where
  go (V.Free v2) | v2 == v = ([], Var V.bound1)
  go (V.Free v2) = ([v2], Var (V.Free v2))
  go x = ([], Var x)

ap1 :: Term l t (Var v) -> Term l t (Var v) -> Maybe (Term l t (Var v))
ap1 (Lam body) t = Just (subst1 body t)
ap1 _ _ = Nothing

bound1 :: Term l t (Var v)
bound1 = Var V.bound1

closed :: Term l t v -> Maybe (Term l t v2)
closed = traverse (const Nothing)

collect :: Applicative f
       => (v -> f (Term l t v2))
       -> Term l t v
       -> f (Term l t v2)
collect f = go where
  go e = case e of
    Var v -> f v
    Lit l -> pure (Lit l) -- not clear why can't just recyle LHS
    App fn arg -> App <$> go fn <*> go arg
    Ann e' t -> Ann <$> go e' <*> pure t
    Lam body -> Lam <$> go body

lam1 :: (forall v . Term l t v -> Term l t v) -> Term l t (Var v2)
lam1 f = Lam . fromJust . abstract1 () . f $ Var (Free ())

-- subst1 f x
subst1 :: Term l t (Var v) -> Term l t (Var v) -> Term l t (Var v)
subst1 = go D.bound1 where
  go ind body e = case body of
    Var (Bound i) | i == ind -> e
    Var _ -> body
    Lit _ -> body
    App f arg -> App (go ind f e) (go ind arg e)
    Ann body' t -> Ann (go ind body' e) t
    Lam body' -> Lam (go (D.succ ind) body' e)

vars :: Term l t v -> [v]
vars = toList
