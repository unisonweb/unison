module Unison.Util.TransitiveClosure where

import Unison.Prelude

import           Data.Functor.Identity    (runIdentity)
import qualified Data.Set                 as Set

transitiveClosure :: forall m a. (Monad m, Ord a)
                  => (a -> m (Set a))
                  -> Set a
                  -> m (Set a)
transitiveClosure getDependencies open =
  let go :: Set a -> [a] -> m (Set a)
      go closed [] = pure closed
      go closed (h:t) =
        if Set.member h closed
          then go closed t
        else do
          deps <- getDependencies h
          go (Set.insert h closed) (toList deps ++ t)
  in go Set.empty (toList open)
  
transitiveClosure' :: Ord a => (a -> Set a) -> Set a -> Set a
transitiveClosure' f as = runIdentity $ transitiveClosure (pure . f) as

transitiveClosure1 :: forall m a. (Monad m, Ord a)
                   => (a -> m (Set a)) -> a -> m (Set a)
transitiveClosure1 f a = transitiveClosure f (Set.singleton a)

transitiveClosure1' :: Ord a => (a -> Set a) -> a -> Set a
transitiveClosure1' f a = runIdentity $ transitiveClosure1 (pure . f) a
