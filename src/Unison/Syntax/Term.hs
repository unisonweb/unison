{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE Rank2Types #-}

module Unison.Syntax.Term where

import Control.Applicative
import Unison.Syntax.Var as V

-- | Terms with literals in `l` and type annotations in `t`
data Term l t
  = Var V.Var
  | Lit l
  | App (Term l t) (Term l t)
  | Ann (Term l t) t
  | Lam (Term l t)
  deriving (Eq,Ord,Show)

abstract :: V.Var -> Term l t -> Term l t
abstract v = go V.bound1 where
  go _ l@(Lit _) = l
  go n (App f arg) = App (go n f) (go n arg)
  go n (Var v')  | v == v'   = Var n
  go _ x@(Var _) | otherwise = x
  go n (Ann e t) = Ann (go n e) t
  go n (Lam body) = Lam (go (V.succ n) body)

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
lam1 f = let v = V.decr V.bound1 -- unused
         in Lam . abstract v . f $ Var v

lam2 :: (Term l t -> Term l t -> Term l t) -> Term l t
lam2 f =
  let v = V.decr V.bound1 -- unused
      v2 = V.decr v
  in Lam (abstract v (Lam (abstract v2 $ f (Var v) (Var v2))))

lam3 :: (Term l t -> Term l t -> Term l t -> Term l t) -> Term l t
lam3 f =
  let v = V.decr V.bound1 -- unused
      v2 = V.decr v
      v3 = V.decr v2
  in Lam (abstract v (Lam (abstract v2 (Lam (abstract v3 $ f (Var v) (Var v2) (Var v3))))))

-- subst1 f x
subst1 :: Term l t -> Term l t -> Term l t
subst1 = go V.bound1 where
  go ind body e = case body of
    Var v | v == ind -> e
    Var _ -> body
    Lit _ -> body
    App f arg -> App (go ind f e) (go ind arg e)
    Ann body' t -> Ann (go ind body' e) t
    Lam body' -> Lam (go (V.succ ind) body' e)

vars :: Term l t -> [V.Var]
vars e = getConst $ collect (\v -> Const [v]) e

stripAnn :: Term l t -> (Term l t, Term l t -> Term l t)
stripAnn (Ann e t) = (e, \e' -> Ann e' t)
stripAnn e = (e, id)

-- arguments 'f x y z' == '[x, y, z]'
arguments :: Term l t -> [Term l t]
arguments (App f x) = arguments f ++ [x]
arguments _ = []

betaReduce :: Term l t -> Term l t
betaReduce (App (Lam f) arg) = subst1 f arg
betaReduce e = e

applyN :: Term l t -> [Term l t] -> Term l t
applyN f = foldl App f
