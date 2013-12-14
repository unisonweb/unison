{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE Rank2Types #-}

module Unison.Syntax.Term where

import Control.Applicative
import Data.Maybe
import Unison.Syntax.Var as V
import Unison.Syntax.DeBruijn as D

-- | Terms with literals in `l` and type annotations in `t`
data Term l t
  = Var V.Var
  | Lit l
  | App (Term l t) (Term l t)
  | Ann (Term l t) t
  | Lam (Term l t)
  deriving (Eq,Ord,Show)

abstract1 :: V.Var -> Term l t -> Maybe (Term l t)
abstract1 v = collect go where
  go v2 | v2 == v = Just (Var V.bound1)
  go _ = Nothing

abstract :: V.Var -> Term l t -> ([V.Var], Term l t)
abstract v = collect go where
  go v2 | v2 == v = ([], Var V.bound1)
  go v2 = ([v2], Var v2)
  go x = ([], Var x)

ap1 :: Term l t -> Term l t -> Maybe (Term l t)
ap1 (Lam body) t = Just (subst1 body t)
ap1 _ _ = Nothing

bound1 :: Term l t
bound1 = Var V.bound1

collect :: Applicative f
       => (V.Var -> f (Term l t))
       -> Term l t
       -> f (Term l t)
collect f = go where
  go e = case e of
    Var v -> f v
    Lit l -> pure (Lit l) -- not clear why can't just recyle LHS
    App fn arg -> App <$> go fn <*> go arg
    Ann e' t -> Ann <$> go e' <*> pure t
    Lam body -> Lam <$> go body

lam1 :: (Term l t -> Term l t) -> Term l t
lam1 f = let v = V.decr . V.decr $ V.bound1 -- unused
         in Lam . fromJust . abstract1 v . f $ Var v

-- subst1 f x
subst1 :: Term l t -> Term l t -> Term l t
subst1 = go D.bound1 where
  go ind body e = case body of
    Var v | v == ind -> e
    Var _ -> body
    Lit _ -> body
    App f arg -> App (go ind f e) (go ind arg e)
    Ann body' t -> Ann (go ind body' e) t
    Lam body' -> Lam (go (D.succ ind) body' e)

vars :: Term l t -> [V.Var]
vars e = getConst $ collect (\v -> Const [v]) e
