module Unison.Util.Maybe where

rewrite :: (a -> Maybe a) -> a -> a
rewrite f a =
  case f a of
    Nothing -> a
    Just a -> a

rewriteM :: Monad m => (a -> m (Maybe a)) -> a -> m a
rewriteM f a =
  f a >>= \case
    Nothing -> pure a
    Just a -> pure a
