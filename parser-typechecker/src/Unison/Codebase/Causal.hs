{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.Codebase.Causal
  ( Causal (..),
    Raw (..),
    RawHash (..),
    head_,
    one,
    cons,
    cons',
    consDistinct,
    uncons,
    hash,
    predecessors,
    Deserialize,
    Serialize,
    cachedRead,
    threeWayMerge,
    threeWayMerge',
    squashMerge',
    lca,
    stepDistinct,
    stepDistinctM,
    sync,
    transform,
    unsafeMapHashPreserving,
    before,
    beforeHash,
  )
where

import Unison.Prelude

import qualified Control.Monad.Extra as Monad (anyM)
import Control.Monad.State (StateT)
import qualified Control.Monad.State as State
import qualified Control.Monad.Reader as Reader
import qualified Data.Map as Map
import Data.Sequence (ViewL (..))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified U.Util.Cache as Cache
import Unison.Hash (Hash)
import qualified Unison.Hash as Hash
import Unison.Hashable (Hashable)
import qualified Unison.Hashable as Hashable
import Prelude hiding (head, read, tail)
import qualified Control.Lens as Lens

{-
`Causal a` has 5 operations, specified algebraically here:

* `before : Causal m a -> Causal m a -> m Bool` defines a partial order on
            `Causal`.
* `head : Causal m a -> a`, which represents the "latest" `a` value in a causal
          chain.
* `one : a -> Causal m a`, satisfying `head (one hd) == hd`
* `cons : a -> Causal a -> Causal a`, satisfying `head (cons hd tl) == hd` and
          also `before tl (cons hd tl)`.
* `merge : CommutativeSemigroup a => Causal a -> Causal a -> Causal a`, which is
           commutative (but not associative) and satisfies:
  * `before c1 (merge c1 c2)`
  * `before c2 (merge c1 c2)`
* `sequence : Causal a -> Causal a -> Causal a`, which is defined as
              `sequence c1 c2 = cons (head c2) (merge c1 c2)`.
  * `before c1 (sequence c1 c2)`
  * `head (sequence c1 c2) == head c2`
-}

newtype RawHash a = RawHash { unRawHash :: Hash }
  deriving (Eq, Ord, Generic)

instance Show (RawHash a) where
  show = show . unRawHash

instance Show e => Show (Causal m h e) where
  show = \case
    One h e      -> "One " ++ (take 3 . show) h ++ " " ++ show e
    Cons h e t   -> "Cons " ++ (take 3 . show) h ++ " " ++ show e ++ " " ++ (take 3 . show) (fst t)
    Merge h e ts -> "Merge " ++ (take 3 . show) h ++ " " ++ show e ++ " " ++ (show . fmap (take 3 . show) . toList) (Map.keysSet ts)

-- h is the type of the pure data structure that will be hashed and used as
-- an index; e.g. h = Branch00, e = Branch0 m
data Causal m h e
  = One { currentHash :: RawHash h
        , head :: e
        }
  | Cons { currentHash :: RawHash h
         , head :: e
         , tail :: (RawHash h, m (Causal m h e))
         }
  -- The merge operation `<>` flattens and normalizes for order
  | Merge { currentHash :: RawHash h
          , head :: e
          , tails :: Map (RawHash h) (m (Causal m h e))
          }

-- | Focus the current head, keeping the hash up to date.
head_ :: Hashable e => Lens.Lens' (Causal m h e) e
head_ = Lens.lens getter setter
  where
    getter = head
    setter causal e =
      case causal of
        One {} -> one e
        Cons{tail=(rawHash, c)} -> cons' e rawHash c
        Merge{tails} -> mergeNode e tails

-- A serializer `Causal m h e`. Nonrecursive -- only responsible for
-- writing a single node of the causal structure.
data Raw h e
  = RawOne e
  | RawCons e (RawHash h)
  | RawMerge e (Set (RawHash h))

type Deserialize m h e = RawHash h -> m (Raw h e)

cachedRead :: MonadIO m
           => Cache.Cache (RawHash h) (Causal m h e)
           -> Deserialize m h e
           -> RawHash h -> m (Causal m h e)
cachedRead cache deserializeRaw h = Cache.lookup cache h >>= \case
  Nothing -> do
    raw <- deserializeRaw h
    causal <- pure $ case raw of
      RawOne e              -> One h e
      RawCons e tailHash    -> Cons h e (tailHash, read tailHash)
      RawMerge e tailHashes -> Merge h e $
        Map.fromList [(h, read h) | h <- toList tailHashes ]
    Cache.insert cache h causal
    pure causal
  Just causal -> pure causal
  where
    read = cachedRead cache deserializeRaw

type Serialize m h e = RawHash h -> Raw h e -> m ()

-- Sync a causal to some persistent store, stopping when hitting a Hash which
-- has already been written, according to the `exists` function provided.
sync
  :: forall m h e
   . Monad m
  => (RawHash h -> m Bool)
  -> Serialize (StateT (Set (RawHash h)) m) h e
  -> Causal m h e
  -> StateT (Set (RawHash h)) m ()
sync exists serialize c = do
  queued <- State.get
  itExists <- if Set.member (currentHash c) queued then pure True
              else lift . exists $ currentHash c
  unless itExists $ go c
 where
  go :: Causal m h e -> StateT (Set (RawHash h)) m ()
  go c = do
    queued <- State.get
    when (Set.notMember (currentHash c) queued) $ do
      State.modify (Set.insert $ currentHash c)
      case c of
        One currentHash head -> serialize currentHash $ RawOne head
        Cons currentHash head (tailHash, tailm) -> do
          -- write out the tail first, so what's on disk is always valid
          b <- lift $ exists tailHash
          unless b $ go =<< lift tailm
          serialize currentHash (RawCons head tailHash)
        Merge currentHash head tails -> do
          for_ (Map.toList tails) $ \(hash, cm) -> do
            b <- lift $ exists hash
            unless b $ go =<< lift cm
          serialize currentHash (RawMerge head (Map.keysSet tails))

instance Eq (Causal m h a) where
  a == b = currentHash a == currentHash b

instance Ord (Causal m h a) where
  a <= b = currentHash a <= currentHash b

instance Hashable (RawHash h) where
  tokens (RawHash h) = Hashable.tokens h

-- Find the lowest common ancestor of two causals.
lca :: Monad m => Causal m h e -> Causal m h e -> m (Maybe (Causal m h e))
lca a b =
  lca' (Seq.singleton $ pure a) (Seq.singleton $ pure b)

-- `lca' xs ys` finds the lowest common ancestor of any element of `xs` and any
-- element of `ys`.
-- This is a breadth-first search used in the implementation of `lca a b`.
lca'
  :: Monad m
  => Seq (m (Causal m h e))
  -> Seq (m (Causal m h e))
  -> m (Maybe (Causal m h e))
lca' = go Set.empty Set.empty where
  go seenLeft seenRight remainingLeft remainingRight =
    case Seq.viewl remainingLeft of
      Seq.EmptyL -> search seenLeft remainingRight
      a :< as    -> do
        left <- a
        if Set.member (currentHash left) seenRight
          then pure $ Just left
          -- Note: swapping position of left and right when we recurse so that
          -- we search each side equally. This avoids having to case on both
          -- arguments, and the order shouldn't really matter.
          else go seenRight
                  (Set.insert (currentHash left) seenLeft)
                  remainingRight
                  (as <> predecessors left)
  search seen remaining = case Seq.viewl remaining of
    Seq.EmptyL -> pure Nothing
    a :< as    -> do
      current <- a
      if Set.member (currentHash current) seen
        then pure $ Just current
        else search seen (as <> predecessors current)

predecessors :: Causal m h e -> Seq (m (Causal m h e))
predecessors (One _ _         ) = Seq.empty
predecessors (Cons  _ _ (_, t)) = Seq.singleton t
predecessors (Merge _ _ ts    ) = Seq.fromList $ Map.elems ts

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
  predecessors =
    Map.fromList [(currentHash c1, pure c1), (currentHash c2, pure c2)]
  done :: e -> Causal m h e
  done newHead = mergeNode newHead predecessors

mergeNode :: Hashable e => e -> Map (RawHash h) (m (Causal m h e)) -> Causal m h e
mergeNode newHead predecessors =
  Merge (RawHash (hash (newHead, Map.keys predecessors))) newHead predecessors

before :: Monad m => Causal m h e -> Causal m h e -> m Bool
before a b = (== Just a) <$> lca a b

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
      cs <- lift . lift $ toList <$> sequence (predecessors c)
      let unseens = filter (\c -> c `Set.notMember` seen) cs
      State.modify' (<> Set.fromList cs)
      Monad.anyM (Reader.local (1+) . go) unseens

hash :: Hashable e => e -> Hash
hash = Hashable.accumulate'

stepDistinct :: (Applicative m, Eq e, Hashable e) => (e -> e) -> Causal m h e -> Causal m h e
stepDistinct f c = f (head c) `consDistinct` c

stepDistinctM
  :: (Applicative m, Functor n, Eq e, Hashable e)
  => (e -> n e) -> Causal m h e -> n (Causal m h e)
stepDistinctM f c = (`consDistinct` c) <$> f (head c)

one :: Hashable e => e -> Causal m h e
one e = One (RawHash $ hash e) e

cons :: (Applicative m, Hashable e) => e -> Causal m h e -> Causal m h e
cons e tl = cons' e (currentHash tl) (pure tl)

cons' :: Hashable e => e -> RawHash h -> m (Causal m h e) -> Causal m h e
cons' e ht mt = Cons (RawHash $ hash [hash e, unRawHash ht]) e (ht, mt)

consDistinct :: (Applicative m, Eq e, Hashable e) => e -> Causal m h e -> Causal m h e
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
