-- Based on: http://semantic-domain.blogspot.com/2015/03/abstract-binding-trees.html

module U.Core.ABT where

import Control.Monad (join)
import qualified Data.Foldable as Foldable
import Data.Functor.Identity (Identity (runIdentity))
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Prelude hiding (abs, cycle)

data ABT f v r
  = Var v
  | Cycle r
  | Abs v r
  | Tm (f r)
  deriving (Show, Functor, Foldable, Traversable)

-- | At each level in the tree, we store the set of free variables and
-- a value of type `a`. Variables are of type `v`.
data Term f v a = Term {freeVars :: Set v, annotation :: a, out :: ABT f v (Term f v a)}
  deriving (Functor, Foldable, Generic, Traversable)

deriving instance (forall q. Show q => Show (f q), Show v, Show a) => Show (Term f v a)

amap :: (Functor f, Foldable f) => (a -> a') -> Term f v a -> Term f v a'
amap = fmap

vmap :: (Functor f, Foldable f, Ord v') => (v -> v') -> Term f v a -> Term f v' a
vmap f (Term _ a out) = case out of
  Var v -> var a (f v)
  Tm fa -> tm a (fmap (vmap f) fa)
  Cycle r -> cycle a (vmap f r)
  Abs v body -> abs a (f v) (vmap f body)

transform ::
  (Ord v, Foldable g, Functor g) =>
  (forall a. f a -> g a) ->
  Term f v a ->
  Term g v a
transform f t = case out t of
  Var v -> var (annotation t) v
  Abs v body -> abs (annotation t) v (transform f body)
  Tm subterms -> tm (annotation t) (fmap (transform f) (f subterms))
  Cycle body -> cycle (annotation t) (transform f body)

transformM ::
  (Ord v, Monad m, Traversable g) =>
  (forall a. f a -> m (g a)) ->
  Term f v a ->
  m (Term g v a)
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

-- * Traversals

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
  (Traversable f, Monad g, Ord v) =>
  (f (Term f v a) -> g (f (Term f v a))) ->
  Term f v a ->
  g (Term f v a)
visit' f t = case out t of
  Var _ -> pure t
  Cycle body -> cycle (annotation t) <$> visit' f body
  Abs x e -> abs (annotation t) x <$> visit' f e
  Tm body -> f body >>= (fmap (tm (annotation t)) . traverse (visit' f))

-- | Apply an effectful function to an ABT tree top down, sequencing the results.
visit_ ::
  (Traversable f, Monad g, Ord v) =>
  (f (Term f v a) -> g ()) ->
  Term f v a ->
  g (Term f v a)
visit_ f t = case out t of
  Var _ -> pure t
  Cycle body -> cycle (annotation t) <$> visit_ f body
  Abs x e -> abs (annotation t) x <$> visit_ f e
  Tm body -> f body >> tm (annotation t) <$> traverse (visit_ f) body

-- | `visit` specialized to the `Identity` effect.
visitPure ::
  (Traversable f, Ord v) =>
  (Term f v a -> Maybe (Term f v a)) ->
  Term f v a ->
  Term f v a
visitPure f = runIdentity . visit (fmap pure . f)

foreachSubterm ::
  (Traversable f, Applicative g) =>
  (Term f v a -> g b) ->
  Term f v a ->
  g [b]
foreachSubterm f e = case out e of
  Var _ -> pure <$> f e
  Cycle body -> (:) <$> f e <*> foreachSubterm f body
  Abs _ body -> (:) <$> f e <*> foreachSubterm f body
  Tm body ->
    (:)
      <$> f e
      <*> (join . Foldable.toList <$> traverse (foreachSubterm f) body)

subterms :: (Ord v, Traversable f) => Term f v a -> [Term f v a]
subterms t = runIdentity $ foreachSubterm pure t

-- * Patterns

pattern Var' :: v -> Term f v a
pattern Var' v <- Term _ _ (Var v)

pattern Cycle' :: [v] -> Term f v a -> Term f v a
pattern Cycle' vs t <- Term _ _ (Cycle (AbsN' vs t))

pattern AbsN' :: [v] -> Term f v a -> Term f v a
pattern AbsN' vs body <- (unabs -> (vs, body))

{-# COMPLETE AbsN' #-}

pattern Tm' :: f (Term f v a) -> Term f v a
pattern Tm' f <- Term _ _ (Tm f)

unabs :: Term f v a -> ([v], Term f v a)
unabs (Term _ _ (Abs hd body)) =
  let (tl, body') = unabs body in (hd : tl, body')
unabs t = ([], t)
