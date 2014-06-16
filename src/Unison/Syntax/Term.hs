{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE Rank2Types #-}

module Unison.Syntax.Term where

import Control.Applicative
import qualified Data.Text as Txt
import Unison.Syntax.Var as V
import qualified Unison.Syntax.Hash as H
import qualified Unison.Syntax.Type as T
import qualified Unison.Syntax.Term.Literal as L

-- | Terms in the Unison language
data Term
  = Var V.Var
  | Lit L.Literal
  | App Term Term
  | Ann Term T.Type
  | Lam Term
  deriving (Eq,Ord,Show)

abstract :: V.Var -> Term -> Term
abstract v = go V.bound1 where
  go _ l@(Lit _) = l
  go n (App f arg) = App (go n f) (go n arg)
  go n (Var v')  | v == v'   = Var n
  go _ x@(Var _) | otherwise = x
  go n (Ann e t) = Ann (go n e) t
  go n (Lam body) = Lam (go (V.succ n) body)

ap1 :: Term -> Term -> Maybe Term
ap1 (Lam body) t = Just (subst1 body t)
ap1 _ _ = Nothing

bound1 :: Term
bound1 = Var V.bound1

collect :: Applicative f
       => (V.Var -> f Term)
       -> Term
       -> f Term
collect f = go where
  go e = case e of
    Var v -> f v
    Lit l -> pure (Lit l) -- not clear why can't just recyle LHS
    App fn arg -> App <$> go fn <*> go arg
    Ann e' t -> Ann <$> go e' <*> pure t
    Lam body -> Lam <$> go body

lam1 :: (Term -> Term) -> Term
lam1 f = let v = V.decr V.bound1 -- unused
         in Lam . abstract v . f $ Var v

lam2 :: (Term -> Term -> Term) -> Term
lam2 f =
  let v = V.decr V.bound1 -- unused
      v2 = V.decr v
  in Lam (abstract v (Lam (abstract v2 $ f (Var v) (Var v2))))

lam3 :: (Term -> Term -> Term -> Term) -> Term
lam3 f =
  let v = V.decr V.bound1 -- unused
      v2 = V.decr v
      v3 = V.decr v2
  in Lam (abstract v (Lam (abstract v2 (Lam (abstract v3 $ f (Var v) (Var v2) (Var v3))))))

-- subst1 f x
subst1 :: Term -> Term -> Term
subst1 = go V.bound1 where
  go ind body e = case body of
    Var v | v == ind -> e
    Var _ -> body
    Lit _ -> body
    App f arg -> App (go ind f e) (go ind arg e)
    Ann body' t -> Ann (go ind body' e) t
    Lam body' -> Lam (go (V.succ ind) body' e)

vars :: Term -> [V.Var]
vars e = getConst $ collect (\v -> Const [v]) e

stripAnn :: Term -> (Term, Term -> Term)
stripAnn (Ann e t) = (e, \e' -> Ann e' t)
stripAnn e = (e, id)

-- arguments 'f x y z' == '[x, y, z]'
arguments :: Term -> [Term]
arguments (App f x) = arguments f ++ [x]
arguments _ = []

betaReduce :: Term -> Term
betaReduce (App (Lam f) arg) = subst1 f arg
betaReduce e = e

applyN :: Term -> [Term] -> Term
applyN f = foldl App f

number :: Double -> Term
number n = Lit (L.Number n)

string :: String -> Term
string s = Lit (L.String (Txt.pack s))

text :: Txt.Text -> Term
text s = Lit (L.String s)

-- | Computes the nameless hash of the given term
hash :: Term -> H.Hash
hash e = error "todo: Term.hash"

-- | Computes the nameless hash of the given terms, where
-- the terms may have mutual dependencies
hashes :: [Term] -> [H.Hash]
hashes e = error "todo: Term.hashes"

hashLit :: L.Literal -> H.Hash
hashLit (L.Hash h) = h
hashLit (L.Number n) = H.zero `H.append` H.hashDouble n
hashLit (L.String s) = H.one `H.append` H.hashText s
hashLit (L.Vector vec) = H.two `H.append` go vec where
  go vec = error "todo: hashLit vector"
