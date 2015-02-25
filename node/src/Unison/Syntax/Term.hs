{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.Syntax.Term where

import qualified Data.Foldable as Foldable
import Data.Traversable
import Control.Applicative
import Control.Lens.TH
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
  | Lam Term
  deriving (Eq,Ord)

instance Show Term where
  show Blank = "_"
  show (Var v) = show v
  show (Ref v) = show v
  show (Lit l) = show l
  show (Vector v) = show v
  show (App f x@(App _ _)) = show f ++ " (" ++ show x ++ ")"
  show (App f x) = show f ++ " " ++ show x
  show (Ann x t) = "(" ++ show x ++ " : " ++ show t ++ ")"
  show lam@(Lam _) = "(λ. " ++ show inner ++ ")"
    where
      inner = go lam
      go v = case v of
        Lam body -> go body
        _ -> v

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
  Lam body -> Lam <$> link env body
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
  Lam body -> dependencies body

isClosed :: Term -> Bool
isClosed e = go V.bound1 e
  where
    go depth e = case e of
      Lit _ -> True
      Ref _ -> True
      Blank -> True
      Var v -> v < depth
      App f arg -> go depth f && go depth arg
      Ann e _ -> go depth e
      Vector vs -> Foldable.all (go depth) vs
      Lam body -> go (V.succ depth) body

{-
freeVars :: Term -> S.Set V.Var
freeVars e = case e of
  Var v -> S.singleton v
  App fn arg -> freeVars fn `S.union` freeVars arg
  Ann e _ -> freeVars e
  Lam n body -> S.delete n (freeVars body)
  Vector vs -> Foldable.foldMap freeVars vs
  _ -> S.empty
-}

newtype Scoped a = Scoped { unscope :: Term }

scoped :: Term -> Maybe (Scoped a)
scoped e | isClosed e = Just (Scoped e)
scoped _ = Nothing

lam :: Scoped (Maybe a) -> Scoped a
lam (Scoped body) = Scoped (Lam body)

var :: Scoped (Maybe a)
var = Scoped (Var V.bound1)

app :: Scoped a -> Scoped a -> Scoped a
app (Scoped f) (Scoped arg) = Scoped (f `App` arg)

ann :: Scoped a -> T.Type -> Scoped a
ann (Scoped e) t = Scoped (e `Ann` t)

weaken :: Scoped a -> Scoped (Maybe a)
weaken (Scoped s) = Scoped (go V.bound1 s)
  where
    go depth e = case e of
      Lit _ -> e
      Ref _ -> e
      Blank -> e
      Var v -> if v >= depth then Var (V.succ v) else e
      App f arg -> App (go depth f) (go depth arg)
      Ann e t -> Ann (go depth e) t
      Vector vs -> Vector (fmap (go depth) vs)
      Lam body -> Lam (go (V.succ depth) body)

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
betaReduce (App (Lam f) arg) = go V.bound1 f where
  go depth body = case body of
    App f x -> App (go depth f) (go depth x)
    Vector vs -> Vector (fmap (go depth) vs)
    Ann body t -> Ann (go depth body) t
    Lam body -> Lam (go (V.succ depth) body)
    Var v | v == depth -> arg
    _ -> body
betaReduce e = e

-- | If the outermost term is a lambda of the form @\x -> f x@,
-- reduce this to @f@.
etaReduce :: Term -> Term
etaReduce (Lam (App f (Var v))) | v == V.bound1 = f
etaReduce e = e

-- | Repeatedly apply @etaReduce@ until reaching fixed point.
etaNormalForm :: Term -> Term
etaNormalForm (Lam (App f (Var v))) | v == V.bound1 = etaNormalForm f
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
      Lam body -> go body >>= (\body -> save (Lam body))
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

makePrisms ''Term
