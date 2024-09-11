module Unison.Util.TransitiveClosure where

import Data.Set qualified as Set
import Unison.Prelude

transitiveClosure ::
  forall m a.
  (Monad m, Ord a) =>
  (a -> m (Set a)) ->
  Set a ->
  m (Set a)
transitiveClosure getDependencies open =
  let go :: Set a -> [a] -> m (Set a)
      go closed [] = pure closed
      go closed (h : t) =
        if Set.member h closed
          then go closed t
          else do
            deps <- getDependencies h
            go (Set.insert h closed) (toList deps ++ t)
   in go Set.empty (toList open)
