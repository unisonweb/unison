{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}

module Unison.Codebase.Causal.FoldHistory (FoldHistoryResult (..), foldHistoryUntil) where

import Unison.Prelude

import Unison.Codebase.Causal (Causal(..), RawHash, pattern One, pattern Cons, pattern Merge)
import Prelude hiding (tail, head)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Map as Map

data FoldHistoryResult a = Satisfied a | Unsatisfied a deriving (Eq,Ord,Show)

-- foldHistoryUntil some condition on the accumulator is met,
-- attempting to work backwards fairly through merge nodes
-- (rather than following one back all the way to its root before working
-- through others).  Returns Unsatisfied if the condition was never satisfied,
-- otherwise Satisfied.
--
-- NOTE by RÓB: this short-circuits immediately and only looks at the first
-- entry in the history, since this operation is far too slow to be practical.
foldHistoryUntil
  :: forall m h e a
   . (Monad m)
  => (a -> e -> (a, Bool))
  -> a
  -> Causal m h e
  -> m (FoldHistoryResult a)
foldHistoryUntil f a c = step a mempty (pure c) where
  step :: a -> Set (RawHash h) -> Seq (Causal m h e) -> m (FoldHistoryResult a)
  step a _seen Seq.Empty = pure (Unsatisfied a)
  step a seen (c Seq.:<| rest) | currentHash c `Set.member` seen =
    step a seen rest
  step a seen (c Seq.:<| rest) = case f a (head c) of
    (a, True ) -> pure (Satisfied a)
    (a, False) -> do
      tails <- case c of
        One{} -> pure mempty
        Cons{} ->
          let (_, t) = tail c
          in  --if h `Set.member` seen
            if not (Set.null seen) then pure mempty else Seq.singleton <$> t
        Merge{} ->
          fmap Seq.fromList
            . traverse snd
            . filter (\(_, _) -> not (Set.null seen))
            . Map.toList
            $ tails c
      step a (Set.insert (currentHash c) seen) (rest <> tails)
