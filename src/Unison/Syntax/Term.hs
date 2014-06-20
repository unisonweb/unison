{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE Rank2Types #-}

module Unison.Syntax.Term where

import Control.Applicative
import qualified Data.Set as S
import qualified Data.Text as Txt
import qualified Data.Vector.Unboxed as V
import Unison.Syntax.Var as V
import qualified Unison.Syntax.Hash as H
import qualified Unison.Syntax.Type as T

-- | Literals in the Unison language
data Literal
  = Number Double
  | String Txt.Text
  | Vector (V.Vector Double)
  deriving (Eq,Ord,Show,Read)

-- | Terms in the Unison language
data Term
  = Var V.Var
  | Lit Literal
  | Con H.Hash -- ^ A constructor reference. @Con h `App` ...@ is by definition in normal form
  | Ref H.Hash
  | App Term Term
  | Ann Term T.Type
  | Lam Term
  deriving (Eq,Ord)

instance Show Term where
  show (Var v) = show v
  show (Ref v) = show v
  show (Lit l) = show l
  show (Con h) = show h
  show (App f x@(App _ _)) = show f ++ "(" ++ show x ++ ")"
  show (App f x) = show f ++ " " ++ show x
  show (Ann x t) = "(" ++ show x ++ " : " ++ show t ++ ")"
  show (Lam body) = "λ."++show body

abstract :: V.Var -> Term -> Term
abstract v = go V.bound1 where
  go _ l@(Lit _) = l
  go _ r@(Ref _) = r
  go _ c@(Con _) = c
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
    Ref h -> pure (Ref h)
    Con h -> pure (Con h)
    Lit l -> pure (Lit l)
    App fn arg -> App <$> go fn <*> go arg
    Ann e' t -> Ann <$> go e' <*> pure t
    Lam body -> Lam <$> go body

dependencies :: Term -> S.Set H.Hash
dependencies e = case e of
  Ref h -> S.singleton h
  Con h -> S.singleton h
  Var _ -> S.empty
  Lit _ -> S.empty
  App fn arg -> dependencies fn `S.union` dependencies arg
  Ann e _ -> dependencies e
  Lam body -> dependencies body

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
    App f arg -> App (go ind f e) (go ind arg e)
    Ann body' t -> Ann (go ind body' e) t
    Lam body' -> Lam (go (V.succ ind) body' e)
    Var v | v == ind -> e
    _ -> body

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
number n = Lit (Number n)

string :: String -> Term
string s = Lit (String (Txt.pack s))

text :: Txt.Text -> Term
text s = Lit (String s)

-- | Computes the nameless hash of the given term
hash :: Term -> H.Digest
hash _ = error "todo: Term.hash"

finalizeHash :: Term -> H.Hash
finalizeHash = H.finalize . hash

-- | Computes the nameless hash of the given terms, where
-- the terms may have mutual dependencies
hashes :: [Term] -> [H.Hash]
hashes _ = error "todo: Term.hashes"

hashLit :: Literal -> H.Digest
hashLit (Number n) = H.zero `H.append` H.double n
hashLit (String s) = H.one `H.append` H.text s
hashLit (Vector vec) = H.two `H.append` go vec where
  go _ = error "todo: hashLit vector"
