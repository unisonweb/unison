{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.Syntax.Term where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Data.Aeson.TH
import qualified Data.Aeson.Encode as JE
import qualified Data.Set as S
import qualified Data.Map as M
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
  deriving (Eq,Ord,Show)

-- | Terms in the Unison language
data Term
  = Var V.Var
  | Lit Literal
  | Con H.Hash -- ^ A constructor reference. @Con h `App` ...@ is by definition in normal form
  | Ref H.Hash
  | App Term Term
  | Ann Term T.Type
  | Lam V.Var Term
  deriving (Eq,Ord)

instance Show Term where
  show (Var v) = show v
  show (Ref v) = show v
  show (Lit l) = show l
  show (Con h) = show h
  show (App f x@(App _ _)) = show f ++ "(" ++ show x ++ ")"
  show (App f x) = show f ++ " " ++ show x
  show (Ann x t) = "(" ++ show x ++ " : " ++ show t ++ ")"
  show (Lam n body) = "(" ++ show n ++ " -> " ++ show body ++ ")"

maxV :: Term -> V.Var
maxV (App f x) = maxV f `max` maxV x
maxV (Ann x _) = maxV x
maxV (Lam n _) = n
maxV _         = V.decr V.bound1

lam1M :: Monad f => (Term -> f Term) -> f Term
lam1M f = return Lam `ap` n `ap` body
  where
    n               = liftM (V.succ . maxV) body
    body            = f =<< (liftM Var n)

lam1 :: (Term -> Term) -> Term
lam1 f = Lam n body
  where
    n = V.succ (maxV body)
    body = f (Var n)

lam2 :: (Term -> Term -> Term) -> Term
lam2 f = lam1 $ \x -> lam1 $ \y -> f x y

lam3 :: (Term -> Term -> Term -> Term) -> Term
lam3 f = lam1 $ \x -> lam1 $ \y -> lam1 $ \z -> f x y z

-- | Convert all 'Ref' constructors to the corresponding term
link :: (Applicative f, Monad f) => (H.Hash -> f Term) -> Term -> f Term
link env e = case e of
  -- recursively resolve all references, leaving alone any hashes
  -- that resolve to themselves, as these are considered primops
  Ref h -> env h >>= \e -> case e of
    Ref h' | h == h' -> pure $ Ref h'
    Con h' | h == h' -> pure $ Con h'
    e | S.null (dependencies e) -> pure $ e
    e | otherwise -> link env e
  App fn arg -> App <$> link env fn <*> link env arg
  Lam n body -> go <$> link env body
    where go body = lam1 $ \x -> betaReduce (Lam n body `App` x)
  _ -> pure e

dependencies :: Term -> S.Set H.Hash
dependencies e = case e of
  Ref h -> S.singleton h
  Con h -> S.singleton h
  Var _ -> S.empty
  Lit _ -> S.empty
  App fn arg -> dependencies fn `S.union` dependencies arg
  Ann e _ -> dependencies e
  Lam _ body -> dependencies body

freeVars :: Term -> S.Set V.Var
freeVars e = case e of
  Var v -> S.singleton v
  App fn arg -> freeVars fn `S.union` freeVars arg
  Ann e _ -> freeVars e
  Lam n body -> S.delete n (freeVars body)
  _ -> S.empty

isClosed :: Term -> Bool
isClosed e | S.null (freeVars e) = True
isClosed _ = False

stripAnn :: Term -> (Term, Term -> Term)
stripAnn (Ann e t) = (e, \e' -> Ann e' t)
stripAnn e = (e, id)

-- arguments 'f x y z' == '[x, y, z]'
arguments :: Term -> [Term]
arguments (App f x) = arguments f ++ [x]
arguments _ = []

-- | If the outermost term is a function application,
-- perform substitution of the argument into the body
betaReduce :: Term -> Term
betaReduce (App (Lam var f) arg) = go f where
  go :: Term -> Term
  go body = case body of
    App f x -> App (go f) (go x)
    Ann body t -> Ann (go body) t
    Lam n body | n == var  -> Lam n body -- this lambda shadows var; avoid substituting
               | otherwise -> Lam n (go body)
    Var v | v == var -> arg
    _ -> body
betaReduce e = e

-- | If the outermost term is a lambda of the form @\x -> f x@,
-- reduce this to @f@.
etaReduce :: Term -> Term
etaReduce (Lam n (App f n')) | Var n == n' = f
etaReduce e = e

-- | Repeatedly apply eta reduction until reaching fixed point
etaNormalForm :: Term -> Term
etaNormalForm (Lam n (App f n')) | Var n == n' = etaNormalForm f
etaNormalForm e = e

applyN :: Term -> [Term] -> Term
applyN f = foldl App f

number :: Double -> Term
number n = Lit (Number n)

string :: String -> Term
string s = Lit (String (Txt.pack s))

text :: Txt.Text -> Term
text s = Lit (String s)

hashCons :: Term -> Writer (M.Map H.Hash Term) Term
hashCons e =
  let closedHash = H.finalize . H.lazyBytes . JE.encode
      save e | isClosed e = let h = closedHash e in tell (M.singleton h e) >> pure (Ref h)
      save e = pure e
  in case etaNormalForm e of
    l@(Lit _) -> save l
    c@(Con _) -> pure c
    r@(Ref _) -> pure r
    v@(Var _) -> pure v
    Lam n body -> hashCons body >>=
      \body -> save (lam1 $ \x -> betaReduce (Lam n body `App` x))
    Ann e t   -> save =<< (Ann <$> hashCons e <*> pure t)
    App f x   -> save =<< (App <$> hashCons f <*> hashCons x)

-- | Computes the nameless hash of the given term
hash :: Term -> H.Digest
hash e = H.lazyBytes . JE.encode . fst . runWriter . hashCons $ e

finalizeHash :: Term -> H.Hash
finalizeHash = H.finalize . hash

-- | Computes the nameless hash of the given terms, where
-- the terms may have mutual dependencies
hashes :: [Term] -> [H.Hash]
hashes _ = error "todo: Term.hashes"

deriveJSON defaultOptions ''Literal
deriveJSON defaultOptions ''Term
