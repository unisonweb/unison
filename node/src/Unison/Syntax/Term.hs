{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.Syntax.Term where

import qualified Data.Foldable as Foldable
import Data.Traversable
import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Data.Aeson.TH
import qualified Data.Aeson.Encode as JE
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as Txt
import qualified Data.Vector as V
import Unison.Syntax.Var as V
import qualified Unison.Syntax.Distance as Distance
import qualified Unison.Syntax.Hash as H
import qualified Unison.Syntax.Reference as R
import qualified Unison.Syntax.Type as T

-- | Literals in the Unison language
data Literal
  = Number Double
  | String Txt.Text
  | Distance Distance.Distance
  deriving (Eq,Ord,Show)

-- | Terms in the Unison language
data Term
  = Var V.Var
  | Lit Literal
  | Blank -- An expression that has not been filled in, has type `forall a . a`
  | Ref R.Reference
  | App Term Term
  | Ann Term T.Type
  | Vector (V.Vector Term)
  | Lam V.Var Term
  deriving (Eq,Ord)

instance Show Term where
  show Blank = "_"
  show (Var v) = show v
  show (Ref v) = show v
  show (Lit l) = show l
  show (Vector v) = show v
  show (App f x@(App _ _)) = show f ++ "(" ++ show x ++ ")"
  show (App f x) = show f ++ " " ++ show x
  show (Ann x t) = "(" ++ show x ++ " : " ++ show t ++ ")"
  show (Lam n body) = "(" ++ show n ++ " -> " ++ show body ++ ")"

maxV :: Term -> V.Var
maxV (App f x) = maxV f `max` maxV x
maxV (Ann x _) = maxV x
maxV (Lam n _) = n
maxV (Vector v) | not (V.null v) = Foldable.foldl max (V.decr V.bound1) (fmap maxV v)
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
  Ref (R.Derived h) -> env h >>= \e -> case e of
    Ref (R.Derived h') | h == h' -> pure $ Ref (R.Derived h')
    e | S.null (dependencies e) -> pure $ e
    e | otherwise -> link env e
  App fn arg -> App <$> link env fn <*> link env arg
  Vector vs -> Vector <$> traverse (link env) vs
  Lam n body -> go <$> link env body
    where go body = lam1 $ \x -> betaReduce (Lam n body `App` x)
  _ -> pure e

dependencies :: Term -> S.Set H.Hash
dependencies e = case e of
  Ref (R.Derived h) -> S.singleton h
  Ref _ -> S.empty
  Var _ -> S.empty
  Lit _ -> S.empty
  Blank -> S.empty
  App fn arg -> dependencies fn `S.union` dependencies arg
  Ann e _ -> dependencies e
  Vector vs -> Foldable.foldMap dependencies vs
  Lam _ body -> dependencies body

freeVars :: Term -> S.Set V.Var
freeVars e = case e of
  Var v -> S.singleton v
  App fn arg -> freeVars fn `S.union` freeVars arg
  Ann e _ -> freeVars e
  Lam n body -> S.delete n (freeVars body)
  Vector vs -> Foldable.foldMap freeVars vs
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
    Vector vs -> Vector (fmap go vs)
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

-- | Order a collection of declarations such that no declaration
-- references hashes declared later in the returned list
topological :: (Ord h, Ord e) => (e -> S.Set h) -> M.Map h e -> [(h, e)]
topological dependencies terms = go S.empty (M.keys terms)
  where
    keys = M.keysSet terms
    go seen pending = case pending of
      [] -> []
      (h:pending) | S.member h seen -> go seen pending
      (h:pending) ->
        let e = maybe (error "unpossible") id $ M.lookup h terms
            seen' = S.insert h seen
            new = S.difference (dependencies e `S.intersection` keys) seen'
            pending' = pending ++ S.toList new
        in go seen' pending' ++ [(h,e)]

-- | Factor all closed subterms out into separate declarations, and
-- return a single term which contains 'Ref's into these declarations
-- The list of subterms are topologically sorted, so terms with
-- no dependencies appear first in the returned list, followed by
-- terms which depend on these dependencies
hashCons :: Term -> ((R.Reference, Term), [(H.Hash, Term)])
hashCons e = let (e', hs) = runWriter (go e) in finalize e' hs
  where
    finalize (Ref r) hs = ((r, Ref r), topological dependencies hs)
    finalize e hs = ((R.Derived (closedHash e), e), topological dependencies hs)
    closedHash = H.finalize . H.lazyBytes . JE.encode
    save e | isClosed e = let h = closedHash e in tell (M.singleton h e) >> pure (Ref (R.Derived h))
    save e = pure e
    go e = case etaNormalForm e of
      l@(Lit _) -> save l
      r@(Ref _) -> pure r
      v@(Var _) -> pure v
      Blank     -> pure Blank
      Lam n body -> go body >>=
        \body -> save (lam1 $ \x -> betaReduce (Lam n body `App` x))
      Ann e t   -> save =<< (Ann <$> go e <*> pure t)
      App f x   -> save =<< (App <$> go f <*> go x)
      Vector vs  -> save =<< (Vector <$> traverse go vs)

-- | Computes the nameless hash of the given term
hash :: Term -> H.Digest
hash e = H.lazyBytes . JE.encode . fst . fst . hashCons $ e

finalizeHash :: Term -> H.Hash
finalizeHash = H.finalize . hash

-- | Computes the nameless hash of the given terms, where
-- the terms may have mutual dependencies
hashes :: [Term] -> [H.Hash]
hashes _ = error "todo: Term.hashes"

deriveJSON defaultOptions ''Literal
deriveJSON defaultOptions ''Term
