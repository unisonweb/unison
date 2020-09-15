{-# LANGUAGE LambdaCase #-}
-- Based on: http://semantic-domain.blogspot.com/2015/03/abstract-binding-trees.html
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}

module U.Core.ABT where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Foldable as Foldable

data ABT f v r
  = Var v
  | Cycle r
  | Abs v r
  | Tm (f r) deriving (Show, Functor, Foldable, Traversable)

-- | At each level in the tree, we store the set of free variables and
-- a value of type `a`. Variables are of type `v`.
data Term f v a = Term { freeVars :: Set v, annotation :: a, out :: ABT f v (Term f v a) }

-- instance (Show1 f, Show v) => Show (Term f v a) where
--   -- annotations not shown
--   showsPrec p (Term _ _ out) = case out of
--     Var v -> \x -> "Var " ++ show v ++ x
--     Cycle body -> ("Cycle " ++) . showsPrec p body
--     Abs v body -> showParen True $ (show v ++) . showString ". " . showsPrec p body
--     Tm f -> showsPrec1 p f

extraMap :: Functor g => (forall k . f k -> g k) -> Term f v a -> Term g v a
extraMap p (Term fvs a sub) = Term fvs a (go p sub) where
  go :: Functor g => (forall k . f k -> g k) -> ABT f v (Term f v a) -> ABT g v (Term g v a)
  go p = \case
    Var v -> Var v
    Cycle r -> Cycle (extraMap p r)
    Abs v r -> Abs v (extraMap p r)
    Tm x -> Tm (fmap (extraMap p) (p x))

abs :: Ord v => a -> v -> Term f v a -> Term f v a
abs a v body = Term (Set.delete v (freeVars body)) a (Abs v body)

annotatedVar :: a -> v -> Term f v a
annotatedVar a v = Term (Set.singleton v) a (Var v)

cycle :: a -> Term f v a -> Term f v a
cycle a t = Term (freeVars t) a (Cycle t)

tm :: (Foldable f, Ord v) => a -> f (Term f v a) -> Term f v a
tm a t = Term (Set.unions (fmap freeVars (Foldable.toList t))) a (Tm t)
