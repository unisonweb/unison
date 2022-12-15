{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.Codebase.Causal
  ( Causal (currentHash, head, tail, tails),
    pattern One,
    pattern Cons,
    pattern Merge,
    head_,
    one,
    cons,
    consDistinct,
    uncons,
    predecessors,
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

import qualified Control.Lens as Lens
import qualified Control.Monad.Extra as Monad (anyM)
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import qualified Data.Map as Map
import qualified Data.Set as Set
import U.Codebase.HashTags (CausalHash)
import Unison.Codebase.Causal.Type
  ( Causal
      ( UnsafeCons,
        UnsafeMerge,
        UnsafeOne,
        currentHash,
        head,
        tail,
        tails
      ),
    before,
    lca,
    predecessors,
    pattern Cons,
    pattern Merge,
    pattern One,
  )
import Unison.ContentAddressable (ContentAddressable)
import Unison.Hash (HashFor (HashFor))
import qualified Unison.Hashing.V2.Convert as Hashing
import Unison.Prelude
import Prelude hiding (head, read, tail)

-- | Focus the current head, keeping the hash up to date.
head_ :: ContentAddressable e => Lens.Lens' (Causal m e) e
head_ = Lens.lens getter setter
  where
    getter = head
    setter causal e =
      case causal of
        UnsafeOne {} -> one e
        UnsafeCons {tail} -> fromListM e [tail]
        UnsafeMerge {tails} -> mergeNode e tails

-- A `squashMerge combine c1 c2` gives the same resulting `e`
-- as a `threeWayMerge`, but doesn't introduce a merge node for the
-- result. Instead, the resulting causal is a simple `Cons` onto `c2`
-- (or is equal to `c2` if `c1` changes nothing).
squashMerge' ::
  forall m e.
  (Monad m, ContentAddressable e, Eq e) =>
  (Causal m e -> Causal m e -> m (Maybe (Causal m e))) ->
  (e -> m e) ->
  (Maybe e -> e -> e -> m e) ->
  Causal m e ->
  Causal m e ->
  m (Causal m e)
squashMerge' lca discardHistory combine c1 c2 = do
  theLCA <- lca c1 c2
  let done newHead = consDistinct newHead c2
  case theLCA of
    Nothing -> done <$> combine Nothing (head c1) (head c2)
    Just lca
      | lca == c1 -> pure c2
      | lca == c2 -> done <$> discardHistory (head c1)
      | otherwise -> done <$> combine (Just $ head lca) (head c1) (head c2)

threeWayMerge ::
  forall m e.
  (Monad m, ContentAddressable e) =>
  (Maybe e -> e -> e -> m e) ->
  Causal m e ->
  Causal m e ->
  m (Causal m e)
threeWayMerge = threeWayMerge' lca

threeWayMerge' ::
  forall m e.
  (Monad m, ContentAddressable e) =>
  (Causal m e -> Causal m e -> m (Maybe (Causal m e))) ->
  (Maybe e -> e -> e -> m e) ->
  Causal m e ->
  Causal m e ->
  m (Causal m e)
threeWayMerge' lca combine c1 c2 = do
  theLCA <- lca c1 c2
  case theLCA of
    Nothing -> done <$> combine Nothing (head c1) (head c2)
    Just lca
      | lca == c1 -> pure c2
      | lca == c2 -> pure c1
      | otherwise -> done <$> combine (Just $ head lca) (head c1) (head c2)
  where
    done :: e -> Causal m e
    done newHead = fromList newHead [c1, c2]

-- `True` if `h` is found in the history of `c` within `maxDepth` path length
-- from the tip of `c`
beforeHash :: forall m e. Monad m => Word -> CausalHash -> Causal m e -> m Bool
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
          cs <- lift . lift $ toList <$> sequence (predecessors c)
          let unseens = filter (\c -> c `Set.notMember` seen) cs
          State.modify' (<> Set.fromList cs)
          Monad.anyM (Reader.local (1 +) . go) unseens

stepDistinct :: (Applicative m, Eq e, ContentAddressable e) => (e -> e) -> Causal m e -> Causal m e
stepDistinct f c = f (head c) `consDistinct` c

stepDistinctM ::
  (Applicative m, Functor n, Eq e, ContentAddressable e) =>
  (e -> n e) ->
  Causal m e ->
  n (Causal m e)
stepDistinctM f c = (`consDistinct` c) <$> f (head c)

-- | Causal construction should go through here for uniformity;
-- with an exception for `one`, which avoids an Applicative constraint.
fromList :: (Applicative m, ContentAddressable e) => e -> [Causal m e] -> Causal m e
fromList e cs =
  fromListM e (map (\c -> (currentHash c, pure c)) cs)

-- | Construct a causal from a list of predecessors. The predecessors may be given in any order.
fromListM :: ContentAddressable e => e -> [(CausalHash, m (Causal m e))] -> Causal m e
fromListM e ts =
  case ts of
    [] -> UnsafeOne ch eh e
    [t] -> UnsafeCons ch eh e t
    _ -> UnsafeMerge ch eh e (Map.fromList ts)
  where
    (ch, eh) = (Hashing.hashCausal e (Set.fromList (map fst ts)))

-- | An optimized variant of 'fromListM' for when it is known we have 2+ predecessors (merge node).
mergeNode :: ContentAddressable e => e -> Map (CausalHash) (m (Causal m e)) -> Causal m e
mergeNode newHead predecessors =
  let (ch, eh) = Hashing.hashCausal newHead (Map.keysSet predecessors)
   in UnsafeMerge ch eh newHead predecessors

-- duplicated logic here instead of delegating to `fromList` to avoid `Applicative m` constraint.
one :: ContentAddressable e => e -> Causal m e
one e = UnsafeOne ch eh e
  where
    (ch, eh) = Hashing.hashCausal e mempty

cons :: (Applicative m, ContentAddressable e) => e -> Causal m e -> Causal m e
cons e tail = fromList e [tail]

consDistinct :: (Applicative m, Eq e, ContentAddressable e) => e -> Causal m e -> Causal m e
consDistinct e tl =
  if head tl == e
    then tl
    else cons e tl

uncons :: Applicative m => Causal m e -> m (Maybe (e, Causal m e))
uncons c = case c of
  Cons _ _ e (_, tl) -> fmap (e,) . Just <$> tl
  _ -> pure Nothing

-- it's okay to call "Unsafe"* here with the existing hashes because `nt` can't
-- affect `e`.
transform :: Functor m => (forall a. m a -> n a) -> Causal m e -> Causal n e
transform nt c = case c of
  One h eh e -> UnsafeOne h eh e
  Cons h eh e (ht, tl) -> UnsafeCons h eh e (ht, nt (transform nt <$> tl))
  Merge h eh e tls -> UnsafeMerge h eh e $ Map.map (\mc -> nt (transform nt <$> mc)) tls

-- "unsafe" because the hashes will be wrong if `f` affects aspects of `e` that impact hashing
unsafeMapHashPreserving :: forall m e e2. Functor m => (e -> e2) -> Causal m e -> Causal m e2
unsafeMapHashPreserving f c = case c of
  One h eh e -> UnsafeOne h (retagValueHash eh) (f e)
  Cons h eh e (ht, tl) -> UnsafeCons h (retagValueHash eh) (f e) (ht, unsafeMapHashPreserving f <$> tl)
  Merge h eh e tls -> UnsafeMerge h (retagValueHash eh) (f e) $ Map.map (fmap $ unsafeMapHashPreserving f) tls
  where
    retagValueHash = coerce @(HashFor e) @(HashFor e2)

data FoldHistoryResult a = Satisfied a | Unsatisfied a deriving (Eq, Ord, Show)
