{-# LANGUAGE LambdaCase #-}
-- Based on: http://semantic-domain.blogspot.com/2015/03/abstract-binding-trees.html
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}


module U.Core.ABT where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Foldable as Foldable
import Prelude hiding (abs,cycle)
import U.Util.Hashable (Accumulate, Hashable1)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified U.Util.Hashable as Hashable
import Data.Functor (void)
import qualified Data.List as List
import qualified Data.Vector as Vector

data ABT f v r
  = Var v
  | Cycle r
  | Abs v r
  | Tm (f r) deriving (Show, Functor, Foldable, Traversable)

-- | At each level in the tree, we store the set of free variables and
-- a value of type `a`. Variables are of type `v`.
data Term f v a = Term { freeVars :: Set v, annotation :: a, out :: ABT f v (Term f v a) }
  deriving (Functor, Foldable, Traversable)

vmap :: (Functor f, Foldable f, Ord v') => (v -> v') -> Term f v a -> Term f v' a
vmap f (Term _ a out) = case out of
  Var v -> var a (f v)
  Tm fa -> tm a (fmap (vmap f) fa)
  Cycle r -> cycle a (vmap f r)
  Abs v body -> abs a (f v) (vmap f body)

transform :: (Ord v, Foldable g, Functor g)
          => (forall a. f a -> g a) -> Term f v a -> Term g v a
transform f t = case out t of
  Var v -> var (annotation t) v
  Abs v body -> abs (annotation t) v (transform f body)
  Tm subterms -> tm (annotation t) (fmap (transform f) (f subterms))
  Cycle body -> cycle (annotation t) (transform f body)

transformM :: (Ord v, Monad m, Traversable g)
          => (forall a. f a -> m (g a)) -> Term f v a -> m (Term g v a)
transformM f t = case out t of
  Var v -> pure $ var (annotation t) v
  Abs v body -> abs (annotation t) v <$> (transformM f body)
  Tm subterms -> tm (annotation t) <$> (traverse (transformM f) =<< f subterms)
  Cycle body -> cycle (annotation t) <$> (transformM f body)

abs :: Ord v => a -> v -> Term f v a -> Term f v a
abs a v body = Term (Set.delete v (freeVars body)) a (Abs v body)

var :: a -> v -> Term f v a
var a v = Term (Set.singleton v) a (Var v)

cycle :: a -> Term f v a -> Term f v a
cycle a t = Term (freeVars t) a (Cycle t)

tm :: (Foldable f, Ord v) => a -> f (Term f v a) -> Term f v a
tm a t = Term (Set.unions (fmap freeVars (Foldable.toList t))) a (Tm t)

-- | We ignore annotations in the `Term`, as these should never affect the
-- meaning of the term.
hash :: forall f v a h . (Functor f, Hashable1 f, Eq v, Show v, Ord h, Accumulate h)
    => Term f v a -> h
hash = hash' [] where
  hash' :: [Either [v] v] -> Term f v a -> h
  hash' env (Term _ _ t) = case t of
    Var v -> maybe die hashInt ind
      where lookup (Left cycle) = v `elem` cycle
            lookup (Right v') = v == v'
            ind = List.findIndex lookup env
            hashInt :: Int -> h
            hashInt i = Hashable.accumulate [Hashable.Nat $ fromIntegral i]
            die = error $ "unknown var in environment: " ++ show v
                        ++ " environment = " ++ show env
    Cycle (unabs -> (vs, t)) -> hash' (Left vs : env) t
    Abs v t -> hash' (Right v : env) t
    Tm t -> Hashable.hash1 (hashCycle env) (hash' env) t
  unabs :: Term f v a -> ([v], Term f v a)
  unabs = \case
    Term _ _ (Abs hd body) ->
      let (tl, body') = unabs body in (hd : tl, body')
    t -> ([], t)
  hashCycle :: [Either [v] v] -> [Term f v a] -> ([h], Term f v a -> h)
  hashCycle env@(Left cycle : envTl) ts | length cycle == length ts =
    let
      permute p xs = case Vector.fromList xs of xs -> map (xs Vector.!) p
      hashed = map (\(i,t) -> ((i,t), hash' env t)) (zip [0..] ts)
      pt = fst <$> List.sortOn snd hashed
      (p,ts') = unzip pt
    in case map Right (permute p cycle) ++ envTl of
      env -> (map (hash' env) ts', hash' env)
  hashCycle env ts = (map (hash' env) ts, hash' env)

-- Hash a strongly connected component and sort its definitions into a canonical order.
hashComponent ::
  (Functor f, Hashable1 f, Foldable f, Eq v, Show v, Ord v, Ord h, Accumulate h)
  => Map v (Term f v a) -> (h, [(v, Term f v a)])
hashComponent byName = let
  ts = Map.toList byName
  embeds = [ (v, void (transform Embed t)) | (v,t) <- ts ]
  vs = fst <$> ts
  -- make closed terms for each element of the component
  -- [ let x = ..., y = ..., in x
  -- , let x = ..., y = ..., in y ]
  -- so that we can then hash them (closed terms can be hashed)
  -- so that we can sort them by hash. this is the "canonical, name-agnostic"
  --   hash that yields the canonical ordering of the component.
  tms = [ (v, absCycle vs (tm () $ Component (snd <$> embeds) (var () v))) | v <- vs ]
  hashed  = [ ((v,t), hash t) | (v,t) <- tms ]
  sortedHashed = List.sortOn snd hashed
  overallHash = Hashable.accumulate (Hashable.Hashed . snd <$> sortedHashed)
  in (overallHash, [ (v, t) | ((v, _),_) <- sortedHashed, Just t <- [Map.lookup v byName] ])
  where
  absChain :: Ord v => [v] -> Term f v () -> Term f v ()
  absChain vs t = foldr (abs ()) t vs
  absCycle :: Ord v => [v] -> Term f v () -> Term f v ()
  absCycle vs t = cycle () $ absChain vs t

-- Implementation detail of hashComponent
data Component f a = Component [a] a | Embed (f a) deriving (Functor, Traversable, Foldable)
instance (Hashable1 f, Functor f) => Hashable1 (Component f) where
  hash1 hashCycle hash c = case c of
    Component as a -> let
      (hs, hash) = hashCycle as
      toks = Hashable.Hashed <$> hs
      in Hashable.accumulate $ (Hashable.Tag 1 : toks) ++ [Hashable.Hashed (hash a)]
    Embed fa -> Hashable.hash1 hashCycle hash fa
