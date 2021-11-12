{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.Codebase.Causal
  ( Causal (..),
    RawHash (RawHash, unRawHash),
    head_,
    one,
    cons,
    cons',
    consDistinct,
    uncons,
    children,
    threeWayMerge,
    threeWayMerge',
    squashMerge',
    lca,
    stepDistinct,
    stepDistinctM,
    transform,
    unsafeMapHashPreserving,
    before,
    beforeHash,
  )
where

import Unison.Prelude

import qualified Control.Monad.Extra as Monad (anyM)
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Unison.Codebase.Branch.Type (Branch0, UnwrappedBranch)
import Unison.Codebase.Causal.Type
  ( Causal
      ( Cons,
        Merge,
        One,
        currentHash,
        head,
        tail,
        tails
      ),
    RawHash (RawHash, unRawHash),
    before,
    children,
    head_,
    lca,
  )
import qualified Unison.Hashing.V2.Convert as Hashing
import Prelude hiding (head, read, tail)
import qualified Unison.Codebase.Branch.Raw as Branch

-- A `squashMerge combine c1 c2` gives the same resulting `e`
-- as a `threeWayMerge`, but doesn't introduce a merge node for the
-- result. Instead, the resulting causal is a simple `Cons` onto `c2`
-- (or is equal to `c2` if `c1` changes nothing).
squashMerge'
  :: forall m
   . Monad m
  => (UnwrappedBranch m -> UnwrappedBranch m -> m (Maybe (UnwrappedBranch m)))
  -> (Branch0 m -> m (Branch0 m))
  -> (Maybe (Branch0 m) -> Branch0 m -> Branch0 m -> m (Branch0 m))
  -> UnwrappedBranch m
  -> UnwrappedBranch m
  -> m (UnwrappedBranch m)
squashMerge' lca discardHistory combine c1 c2 = do
  theLCA <- lca c1 c2
  let done newHead = consDistinct newHead c2
  case theLCA of
    Nothing -> done <$> combine Nothing (head c1) (head c2)
    Just lca
      | lca == c1 -> pure c2
      | lca == c2 -> done <$> discardHistory (head c1)
      | otherwise -> done <$> combine (Just $ head lca) (head c1) (head c2)

threeWayMerge :: forall m
   . Monad m
  => (Maybe (Branch0 m) -> Branch0 m -> Branch0 m -> m (Branch0 m))
  -> UnwrappedBranch m
  -> UnwrappedBranch m
  -> m (UnwrappedBranch m)
threeWayMerge = threeWayMerge' lca

threeWayMerge'
  :: forall m
   . Monad m
  => (UnwrappedBranch m -> UnwrappedBranch m -> m (Maybe (UnwrappedBranch m)))
  -> (Maybe (Branch0 m) -> Branch0 m -> Branch0 m -> m (Branch0 m))
  -> UnwrappedBranch m
  -> UnwrappedBranch m
  -> m (UnwrappedBranch m)
threeWayMerge' lca combine c1 c2 = do
  theLCA <- lca c1 c2
  case theLCA of
    Nothing -> done <$> combine Nothing (head c1) (head c2)
    Just lca
      | lca == c1 -> pure c2
      | lca == c2 -> pure c1
      | otherwise -> done <$> combine (Just $ head lca) (head c1) (head c2)
 where
  children =
    Map.fromList [(currentHash c1, pure c1), (currentHash c2, pure c2)]
  done :: Branch0 m -> UnwrappedBranch m
  done newHead =
    let h = Hashing.hashCausal newHead (Map.keysSet children)
    in Merge (RawHash h) newHead children

-- `True` if `h` is found in the history of `c` within `maxDepth` path length
-- from the tip of `c`
beforeHash :: forall m h e . Monad m => Word -> RawHash h -> Causal m h e -> m Bool
beforeHash maxDepth h c =
  Reader.runReaderT (State.evalStateT (go c) Set.empty) (0 :: Word)
  where
  go c | h == currentHash c = pure True
  go c = do
    currentDepth :: Word <- Reader.ask
    if currentDepth >= maxDepth
    then pure False
    else do
      seen <- State.get
      cs <- lift . lift $ toList <$> sequence (children c)
      let unseens = filter (\c -> c `Set.notMember` seen) cs
      State.modify' (<> Set.fromList cs)
      Monad.anyM (Reader.local (1+) . go) unseens

stepDistinct :: Applicative m => (Branch0 m -> Branch0 m) -> UnwrappedBranch m -> UnwrappedBranch m
stepDistinct f c = f (head c) `consDistinct` c

stepDistinctM
  :: (Applicative m, Functor n)
  => (Branch0 m -> n (Branch0 m)) -> UnwrappedBranch m -> n (UnwrappedBranch m)
stepDistinctM f c = (`consDistinct` c) <$> f (head c)

one :: Branch0 m -> UnwrappedBranch m
one e =
  let h = Hashing.hashCausal e mempty
  in One (RawHash h) e

cons :: Applicative m => Branch0 m -> UnwrappedBranch m -> UnwrappedBranch m
cons e tl = cons' e (currentHash tl) (pure tl)

cons' :: Branch0 m -> RawHash Branch.Raw -> m (UnwrappedBranch m) -> UnwrappedBranch m
cons' b0 hTail mTail =
  let h = Hashing.hashCausal b0 (Set.singleton hTail)
  in Cons (RawHash h) b0 (hTail, mTail)

consDistinct :: Applicative m => Branch0 m -> UnwrappedBranch m -> UnwrappedBranch m
consDistinct e tl =
  if head tl == e then tl
  else cons e tl

uncons :: Applicative m => Causal m h e -> m (Maybe (e, Causal m h e))
uncons c = case c of
  Cons _ e (_,tl) -> fmap (e,) . Just <$> tl
  _ -> pure Nothing

transform :: Functor m => (forall a . m a -> n a) -> Causal m h e -> Causal n h e
transform nt c = case c of
  One h e -> One h e
  Cons h e (ht, tl) -> Cons h e (ht, nt (transform nt <$> tl))
  Merge h e tls -> Merge h e $ Map.map (\mc -> nt (transform nt <$> mc)) tls

unsafeMapHashPreserving :: Functor m => (e -> e2) -> Causal m h e -> Causal m h e2
unsafeMapHashPreserving f c = case c of
  One h e -> One h (f e)
  Cons h e (ht, tl) -> Cons h (f e) (ht, unsafeMapHashPreserving f <$> tl)
  Merge h e tls -> Merge h (f e) $ Map.map (fmap $ unsafeMapHashPreserving f) tls

data FoldHistoryResult a = Satisfied a | Unsatisfied a deriving (Eq,Ord,Show)
