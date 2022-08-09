{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Unison.Util.Free where

import Unison.Prelude hiding (fold)

-- We would use another package for this if we knew of one.
-- Neither http://hackage.haskell.org/package/free
--     nor http://hackage.haskell.org/package/free-functors
--     nor http://hackage.haskell.org/package/freer
--     appear to have this.

data Free f a = Pure a | forall x. Bind (f x) (x -> Free f a)

eval :: f a -> Free f a
eval fa = Bind fa Pure

-- unfold :: (v -> f (Either a v)) -> v -> Free f a

fold :: Monad m => (forall x. f x -> m x) -> Free f a -> m a
fold f m = case m of
  Pure a -> pure a
  Bind x k -> f x >>= fold f . k

unfold :: (v -> Either a (f v)) -> v -> Free f a
unfold f seed = case f seed of
  Left a -> Pure a
  Right fv -> Bind fv (unfold f)

unfold' :: (v -> Free f (Either a v)) -> v -> Free f a
unfold' f seed = f seed >>= either Pure (unfold' f)

unfoldM ::
  (Traversable f, Applicative m, Monad m) =>
  (b -> m (Either a (f b))) ->
  b ->
  m (Free f a)
unfoldM f seed = do
  e <- f seed
  case e of
    Left a -> pure (Pure a)
    Right fb -> free <$> traverse (unfoldM f) fb

free :: Traversable f => f (Free f a) -> Free f a
free = go . sequence
  where
    go (Pure fa) = Bind fa Pure
    go (Bind fi f) = Bind fi (go . f)

foldWithIndex :: forall f m a. Monad m => (forall x. Int -> f x -> m x) -> Free f a -> m a
foldWithIndex f m = go 0 f m
  where
    go :: Int -> (forall x. Int -> f x -> m x) -> Free f a -> m a
    go starting f m = case m of
      Pure a -> pure a
      Bind x k -> (f starting x) >>= (go $ starting + 1) f . k

instance Functor (Free f) where
  fmap = liftM

instance Monad (Free f) where
  return = pure
  Pure a >>= f = f a
  Bind fx f >>= g = Bind fx (f >=> g)

instance Applicative (Free f) where
  pure = Pure
  (<*>) = ap

instance MonadTrans Free where lift = eval
