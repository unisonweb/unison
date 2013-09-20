{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Unison.Syntax.Term where

import Control.Applicative
import Control.Lens
import Data.Foldable
import Unison.Syntax.Literal
import Unison.Syntax.DeBruijn as DeBruijn

-- | Terms with free variables in `v` and type annotations in `t`
data Term t v
  = Var (Either DeBruijn v) -- a variable is either free, or bound
  | Lit Literal
  | App (Term t v) (Term t v)
  | Ann (Term t v) t
  | Lam (Term t v)
  deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable)

subst1 :: Term t v -> Term t v -> Term t v
subst1 = go (DeBruijn 0) where
  go ind body e = case body of
    Var (Left i) | i == ind -> e
    Var _ -> body
    Lit _ -> body
    App f arg -> App (go ind f e) (go ind arg e)
    Ann body' t -> Ann (go ind body' e) t
    Lam body' -> Lam (go (DeBruijn.succ ind) body' e)

bind0 :: Term t v
bind0 = Var (Left (DeBruijn 0))

vars :: Term t v -> [v]
vars = toList

collect :: Applicative f
       => (v -> f (Term t v2))
       -> Term t v
       -> f (Term t v2)
collect f = go where
  go e = case e of
    Var (Right v) -> f v
    Var (Left ind) -> pure (Var (Left ind))
    Lit l -> pure (Lit l) -- not clear why can't just recyle LHS
    App fn arg -> App <$> go fn <*> go arg
    Ann e' t -> Ann <$> go e' <*> pure t
    Lam body -> Lam <$> go body

abstract1 :: Eq v => v -> Term t v -> Maybe (Term t v2)
abstract1 v = collect go where
  go v2 | v2 == v = Just bind0
  go _ = Nothing
