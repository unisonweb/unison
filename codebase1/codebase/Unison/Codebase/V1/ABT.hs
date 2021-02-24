{-# LANGUAGE LambdaCase #-}
-- Based on: http://semantic-domain.blogspot.com/2015/03/abstract-binding-trees.html
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.V1.ABT where

import Data.Maybe (fromMaybe)
import qualified Data.Foldable as Foldable
import qualified Data.Set as Set
import Data.Set (Set)
import Prelude hiding (abs, cycle)
-- import U.Util.Hashable (Accumulate, Hashable1)
-- import qualified Data.Map as Map
-- import qualified U.Util.Hashable as Hashable
-- import Data.Functor (void)

data ABT f v r
  = Var v
  | Cycle r
  | Abs v r
  | Tm (f r)
  deriving (Functor, Foldable, Traversable)

-- | At each level in the tree, we store the set of free variables and
-- a value of type `a`. Variables are of type `v`.
data Term f v a = Term {freeVars :: Set v, annotation :: a, out :: ABT f v (Term f v a)}

-- | A class for variables.
--
--   * `Set.notMember (freshIn vs v) vs`:
--     `freshIn` returns a variable not used in the `Set`
class Ord v => Var v where
  freshIn :: Set v -> v -> v

extraMap :: Functor g => (forall k . f k -> g k) -> Term f v a -> Term g v a
extraMap p (Term fvs a sub) = Term fvs a (go p sub) where
  go :: Functor g => (forall k . f k -> g k) -> ABT f v (Term f v a) -> ABT g v (Term g v a)
  go p = \case
    Var v -> Var v
    Cycle r -> Cycle (extraMap p r)
    Abs v r -> Abs v (extraMap p r)
    Tm x -> Tm (fmap (extraMap p) (p x))

var :: a -> v -> Term f v a
var a v = Term (Set.singleton v) a (Var v)

abs :: Ord v => a -> v -> Term f v a -> Term f v a
abs a v body = Term (Set.delete v (freeVars body)) a (Abs v body)

tm :: (Foldable f, Ord v) => a -> f (Term f v a) -> Term f v a
tm a t = Term (Set.unions (fmap freeVars (Foldable.toList t))) a (Tm t)

cycle :: a -> Term f v a -> Term f v a
cycle a t = Term (freeVars t) a (Cycle t)

-- | `visit f t` applies an effectful function to each subtree of
-- `t` and sequences the results. When `f` returns `Nothing`, `visit`
-- descends into the children of the current subtree. When `f` returns
-- `Just t2`, `visit` replaces the current subtree with `t2`. Thus:
-- `visit (const Nothing) t == pure t` and
-- `visit (const (Just (pure t2))) t == pure t2`
visit ::
  (Traversable f, Applicative g, Ord v) =>
  (Term f v a -> Maybe (g (Term f v a))) ->
  Term f v a ->
  g (Term f v a)
visit f t = flip fromMaybe (f t) $ case out t of
  Var _ -> pure t
  Cycle body -> cycle (annotation t) <$> visit f body
  Abs x e -> abs (annotation t) x <$> visit f e
  Tm body -> tm (annotation t) <$> traverse (visit f) body

-- | Apply an effectful function to an ABT tree top down, sequencing the results.
visit' ::
  (Traversable f, Applicative g, Monad g, Ord v) =>
  (f (Term f v a) -> g (f (Term f v a))) ->
  Term f v a ->
  g (Term f v a)
visit' f t = case out t of
  Var _ -> pure t
  Cycle body -> cycle (annotation t) <$> visit' f body
  Abs x e -> abs (annotation t) x <$> visit' f e
  Tm body -> f body >>= (fmap (tm (annotation t)) . traverse (visit' f))
