{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.Codebase.Causal
  ( Causal(currentHash, head, tail, tails),
    pattern One,
    pattern Cons,
    pattern Merge,
    RawHash (RawHash, unRawHash),
    head_,
    one,
    cons,
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
    RawHash (RawHash, unRawHash),
    pattern One,
    pattern Cons,
    pattern Merge,
    before,
    children,
    head_,
    lca,
  )
import qualified Unison.Hashing.V2.Convert as Hashing
import Unison.Hashing.V2.Hashable (Hashable)
import Prelude hiding (head, read, tail)
import qualified Data.List.Extra as List

-- A `squashMerge combine c1 c2` gives the same resulting `e`
-- as a `threeWayMerge`, but doesn't introduce a merge node for the
-- result. Instead, the resulting causal is a simple `Cons` onto `c2`
-- (or is equal to `c2` if `c1` changes nothing).
squashMerge'
  :: forall m h e
   . (Monad m, Hashable e, Eq e)
  => (Causal m h e -> Causal m h e -> m (Maybe (Causal m h e)))
  -> (e -> m e)
  -> (Maybe e -> e -> e -> m e)
  -> Causal m h e
  -> Causal m h e
  -> m (Causal m h e)
squashMerge' lca discardHistory combine c1 c2 = do
  theLCA <- lca c1 c2
  let done newHead = consDistinct newHead c2
  case theLCA of
    Nothing -> done <$> combine Nothing (head c1) (head c2)
    Just lca
      | lca == c1 -> pure c2
      | lca == c2 -> done <$> discardHistory (head c1)
      | otherwise -> done <$> combine (Just $ head lca) (head c1) (head c2)

threeWayMerge :: forall m h e
   . (Monad m, Hashable e)
  => (Maybe e -> e -> e -> m e)
  -> Causal m h e
  -> Causal m h e
  -> m (Causal m h e)
threeWayMerge = threeWayMerge' lca

threeWayMerge'
  :: forall m h e
   . (Monad m, Hashable e)
  => (Causal m h e -> Causal m h e -> m (Maybe (Causal m h e)))
  -> (Maybe e -> e -> e -> m e)
  -> Causal m h e
  -> Causal m h e
  -> m (Causal m h e)
threeWayMerge' lca combine c1 c2 = do
  theLCA <- lca c1 c2
  case theLCA of
    Nothing -> done <$> combine Nothing (head c1) (head c2)
    Just lca
      | lca == c1 -> pure c2
      | lca == c2 -> pure c1
      | otherwise -> done <$> combine (Just $ head lca) (head c1) (head c2)
 where
  done :: e -> Causal m h e
  done newHead = fromList newHead [c1, c2]

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

stepDistinct :: (Applicative m, Eq e, Hashable e) => (e -> e) -> Causal m h e -> Causal m h e
stepDistinct f c = f (head c) `consDistinct` c

stepDistinctM
  :: (Applicative m, Functor n, Eq e, Hashable e)
  => (e -> n e) -> Causal m h e -> n (Causal m h e)
stepDistinctM f c = (`consDistinct` c) <$> f (head c)

-- | Causal construction should go through here for uniformity;
-- with an exception for `one`, which avoids an Applicative constraint.
fromList :: (Applicative m, Hashable e) => e -> [Causal m h e] -> Causal m h e
fromList e (List.nubOrdOn currentHash -> tails) = case tails of
  [] -> UnsafeOne h e
  t : [] -> UnsafeCons h e (tailPair t)
  _ : _ : _ -> UnsafeMerge h e (Map.fromList $ map tailPair tails)
  where
    tailPair c = (currentHash c, pure c)
    h = RawHash $ Hashing.hashCausal e (Set.fromList $ map currentHash tails)

-- duplicated logic here instead of delegating to `fromList` to avoid `Applicative m` constraint.
one :: Hashable e => e -> Causal m h e
one e = UnsafeOne h e
  where
    h = RawHash $ Hashing.hashCausal e mempty

cons :: (Applicative m, Hashable e) => e -> Causal m h e -> Causal m h e
cons e tail = fromList e [tail]

consDistinct :: (Applicative m, Eq e, Hashable e) => e -> Causal m h e -> Causal m h e
consDistinct e tl =
  if head tl == e then tl
  else cons e tl

uncons :: Applicative m => Causal m h e -> m (Maybe (e, Causal m h e))
uncons c = case c of
  Cons _ e (_,tl) -> fmap (e,) . Just <$> tl
  _ -> pure Nothing

-- it's okay to call "Unsafe"* here with the existing hashes because `nt` can't
-- affect `e`.
transform :: Functor m => (forall a . m a -> n a) -> Causal m h e -> Causal n h e
transform nt c = case c of
  One h e -> UnsafeOne h e
  Cons h e (ht, tl) -> UnsafeCons h e (ht, nt (transform nt <$> tl))
  Merge h e tls -> UnsafeMerge h e $ Map.map (\mc -> nt (transform nt <$> mc)) tls

-- "unsafe" because the hashes will be wrong if `f` affects aspects of `e` that impact hashing
unsafeMapHashPreserving :: Functor m => (e -> e2) -> Causal m h e -> Causal m h e2
unsafeMapHashPreserving f c = case c of
  One h e -> UnsafeOne h (f e)
  Cons h e (ht, tl) -> UnsafeCons h (f e) (ht, unsafeMapHashPreserving f <$> tl)
  Merge h e tls -> UnsafeMerge h (f e) $ Map.map (fmap $ unsafeMapHashPreserving f) tls

data FoldHistoryResult a = Satisfied a | Unsatisfied a deriving (Eq,Ord,Show)
