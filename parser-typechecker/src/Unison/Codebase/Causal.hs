{-# LANGUAGE RankNTypes #-}

module Unison.Codebase.Causal where

import Control.Monad.State (StateT)
import qualified Control.Monad.State as State
import qualified Data.Map as Map
import Data.Sequence (ViewL (..))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Unison.Hash (Hash)
import Unison.Hashable (Hashable)
import qualified Unison.Hashable as Hashable
import Unison.Prelude
import qualified Unison.Util.Cache as Cache
import Prelude hiding
  ( head,
    read,
    tail,
  )

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

newtype RawHash a = RawHash {unRawHash :: Hash}
  deriving (Eq, Ord)

instance Show (RawHash a) where
  show = show . unRawHash

instance Show e => Show (Causal m h e) where
  show = \case
    One h e -> "One " ++ (take 3 . show) h ++ " " ++ show e
    Cons h e t -> "Cons " ++ (take 3 . show) h ++ " " ++ show e ++ " " ++ (take 3 . show) (fst t)
    Merge h e ts -> "Merge " ++ (take 3 . show) h ++ " " ++ show e ++ " " ++ (show . fmap (take 3 . show) . toList) (Map.keysSet ts)

-- h is the type of the pure data structure that will be hashed and used as
-- an index; e.g. h = Branch00, e = Branch0 m
data Causal m h e
  = One
      { currentHash :: RawHash h,
        head :: e
      }
  | Cons
      { currentHash :: RawHash h,
        head :: e,
        tail :: (RawHash h, m (Causal m h e))
      }
  | -- The merge operation `<>` flattens and normalizes for order
    Merge
      { currentHash :: RawHash h,
        head :: e,
        tails :: Map (RawHash h) (m (Causal m h e))
      }

-- Convert the Causal to an adjacency matrix for debugging purposes.
toGraph ::
  Monad m =>
  Set (RawHash h) ->
  Causal m h e ->
  m (Seq (RawHash h, RawHash h))
toGraph seen c = case c of
  One _ _ -> pure Seq.empty
  Cons h1 _ (h2, m) ->
    if Set.notMember h1 seen
      then do
        tail <- m
        g <- toGraph (Set.insert h1 seen) tail
        pure $ (h1, h2) Seq.<| g
      else pure Seq.empty
  Merge h _ ts ->
    if Set.notMember h seen
      then do
        tails <- sequence $ Map.elems ts
        gs <- Seq.fromList <$> traverse (toGraph (Set.insert h seen)) tails
        pure $ Seq.fromList ((h,) <$> Set.toList (Map.keysSet ts)) <> join gs
      else pure Seq.empty

-- A serializer `Causal m h e`. Nonrecursive -- only responsible for
-- writing a single node of the causal structure.
data Raw h e
  = RawOne e
  | RawCons e (RawHash h)
  | RawMerge e (Set (RawHash h))

rawHead :: Raw h e -> e
rawHead (RawOne e) = e
rawHead (RawCons e _) = e
rawHead (RawMerge e _) = e

-- Don't need to deserialize the `e` to calculate `before`.
data Tails h
  = TailsOne
  | TailsCons (RawHash h)
  | TailsMerge (Set (RawHash h))

type Deserialize m h e = RawHash h -> m (Raw h e)

cachedRead ::
  Monad m =>
  Cache.Cache m (RawHash h) (Causal m h e) ->
  Deserialize m h e ->
  RawHash h ->
  m (Causal m h e)
cachedRead cache deserializeRaw h =
  Cache.lookup cache h >>= \case
    Nothing -> do
      raw <- deserializeRaw h
      causal <- pure $ case raw of
        RawOne e -> One h e
        RawCons e tailHash -> Cons h e (tailHash, read tailHash)
        RawMerge e tailHashes ->
          Merge h e $
            Map.fromList [(h, read h) | h <- toList tailHashes]
      Cache.insert cache h causal
      pure causal
    Just causal -> pure causal
  where
    read = cachedRead cache deserializeRaw

type Serialize m h e = RawHash h -> Raw h e -> m ()

-- Sync a causal to some persistent store, stopping when hitting a Hash which
-- has already been written, according to the `exists` function provided.
sync ::
  forall m h e.
  Monad m =>
  (RawHash h -> m Bool) ->
  Serialize (StateT (Set (RawHash h)) m) h e ->
  Causal m h e ->
  StateT (Set (RawHash h)) m ()
sync exists serialize c = do
  queued <- State.get
  itExists <-
    if Set.member (currentHash c) queued
      then pure True
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
lca' ::
  Monad m =>
  Seq (m (Causal m h e)) ->
  Seq (m (Causal m h e)) ->
  m (Maybe (Causal m h e))
lca' = go Set.empty Set.empty
  where
    go seenLeft seenRight remainingLeft remainingRight =
      case Seq.viewl remainingLeft of
        Seq.EmptyL -> search seenLeft remainingRight
        a :< as -> do
          left <- a
          if Set.member (currentHash left) seenRight
            then pure $ Just left
            else -- Note: swapping position of left and right when we recurse so that
            -- we search each side equally. This avoids having to case on both
            -- arguments, and the order shouldn't really matter.

              go
                seenRight
                (Set.insert (currentHash left) seenLeft)
                remainingRight
                (as <> children left)
    search seen remaining = case Seq.viewl remaining of
      Seq.EmptyL -> pure Nothing
      a :< as -> do
        current <- a
        if Set.member (currentHash current) seen
          then pure $ Just current
          else search seen (as <> children current)

children :: Causal m h e -> Seq (m (Causal m h e))
children (One _ _) = Seq.empty
children (Cons _ _ (_, t)) = Seq.singleton t
children (Merge _ _ ts) = Seq.fromList $ Map.elems ts

-- A `squashMerge combine c1 c2` gives the same resulting `e`
-- as a `threeWayMerge`, but doesn't introduce a merge node for the
-- result. Instead, the resulting causal is a simple `Cons` onto `c2`
-- (or is equal to `c2` if `c1` changes nothing).
squashMerge ::
  forall m h e.
  (Monad m, Hashable e, Eq e) =>
  (Maybe e -> e -> e -> m e) ->
  Causal m h e ->
  Causal m h e ->
  m (Causal m h e)
squashMerge combine c1 c2 = do
  theLCA <- lca c1 c2
  let done newHead = consDistinct newHead c2
  case theLCA of
    Nothing -> done <$> combine Nothing (head c1) (head c2)
    Just lca
      | lca == c1 -> pure c2
      -- Pretty subtle: if we were to add this short circuit, then
      -- the history of c1's children would still make it into the result
      -- Calling `combine` will recursively call into `squashMerge`
      -- for the children, discarding their history before calling `done`
      -- on the parent.
      --   | lca == c2 -> pure $ done c1

      | otherwise -> done <$> combine (Just $ head lca) (head c1) (head c2)

threeWayMerge ::
  forall m h e.
  (Monad m, Hashable e) =>
  (Maybe e -> e -> e -> m e) ->
  Causal m h e ->
  Causal m h e ->
  m (Causal m h e)
threeWayMerge combine c1 c2 = do
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
    done :: e -> Causal m h e
    done newHead =
      Merge (RawHash (hash (newHead, Map.keys children))) newHead children

before :: Monad m => Causal m h e -> Causal m h e -> m Bool
before a b = (== Just a) <$> lca a b

hash :: Hashable e => e -> Hash
hash = Hashable.accumulate'

step :: (Applicative m, Hashable e) => (e -> e) -> Causal m h e -> Causal m h e
step f c = f (head c) `cons` c

stepDistinct :: (Applicative m, Eq e, Hashable e) => (e -> e) -> Causal m h e -> Causal m h e
stepDistinct f c = f (head c) `consDistinct` c

stepIf ::
  (Applicative m, Hashable e) =>
  (e -> Bool) ->
  (e -> e) ->
  Causal m h e ->
  Causal m h e
stepIf cond f c = if cond (head c) then step f c else c

stepM ::
  (Applicative m, Hashable e) => (e -> m e) -> Causal m h e -> m (Causal m h e)
stepM f c = (`cons` c) <$> f (head c)

stepDistinctM ::
  (Applicative m, Functor n, Eq e, Hashable e) =>
  (e -> n e) ->
  Causal m h e ->
  n (Causal m h e)
stepDistinctM f c = (`consDistinct` c) <$> f (head c)

one :: Hashable e => e -> Causal m h e
one e = One (RawHash $ hash e) e

cons :: (Applicative m, Hashable e) => e -> Causal m h e -> Causal m h e
cons e tl =
  Cons (RawHash $ hash [hash e, unRawHash . currentHash $ tl]) e (currentHash tl, pure tl)

consDistinct :: (Applicative m, Eq e, Hashable e) => e -> Causal m h e -> Causal m h e
consDistinct e tl =
  if head tl == e
    then tl
    else cons e tl

uncons :: Applicative m => Causal m h e -> m (Maybe (e, Causal m h e))
uncons c = case c of
  Cons _ e (_, tl) -> fmap (e,) . Just <$> tl
  _ -> pure Nothing

transform :: Functor m => (forall a. m a -> n a) -> Causal m h e -> Causal n h e
transform nt c = case c of
  One h e -> One h e
  Cons h e (ht, tl) -> Cons h e (ht, nt (transform nt <$> tl))
  Merge h e tls -> Merge h e $ Map.map (\mc -> nt (transform nt <$> mc)) tls

unsafeMapHashPreserving :: Functor m => (e -> e2) -> Causal m h e -> Causal m h e2
unsafeMapHashPreserving f c = case c of
  One h e -> One h (f e)
  Cons h e (ht, tl) -> Cons h (f e) (ht, unsafeMapHashPreserving f <$> tl)
  Merge h e tls -> Merge h (f e) $ Map.map (fmap $ unsafeMapHashPreserving f) tls

data FoldHistoryResult a = Satisfied a | Unsatisfied a deriving (Eq, Ord, Show)

-- foldHistoryUntil some condition on the accumulator is met,
-- attempting to work backwards fairly through merge nodes
-- (rather than following one back all the way to its root before working
-- through others).  Returns Unsatisfied if the condition was never satisfied,
-- otherwise Satisfied.
--
-- NOTE by RÓB: this short-circuits immediately and only looks at the first
-- entry in the history, since this operation is far too slow to be practical.
foldHistoryUntil ::
  forall m h e a.
  (Monad m) =>
  (a -> e -> (a, Bool)) ->
  a ->
  Causal m h e ->
  m (FoldHistoryResult a)
foldHistoryUntil f a c = step a mempty (pure c)
  where
    step :: a -> Set (RawHash h) -> Seq (Causal m h e) -> m (FoldHistoryResult a)
    step a _seen Seq.Empty = pure (Unsatisfied a)
    step a seen (c Seq.:<| rest)
      | currentHash c `Set.member` seen =
        step a seen rest
    step a seen (c Seq.:<| rest) = case f a (head c) of
      (a, True) -> pure (Satisfied a)
      (a, False) -> do
        tails <- case c of
          One {} -> pure mempty
          Cons {} ->
            let (_, t) = tail c
             in --if h `Set.member` seen
                if not (Set.null seen) then pure mempty else Seq.singleton <$> t
          Merge {} ->
            fmap Seq.fromList
              . traverse snd
              . filter (\(_, _) -> not (Set.null seen))
              . Map.toList
              $ tails c
        step a (Set.insert (currentHash c) seen) (rest <> tails)

hashToRaw ::
  forall m h e. Monad m => Causal m h e -> m (Map (RawHash h) [RawHash h])
hashToRaw c = go mempty [c]
  where
    go ::
      Map (RawHash h) [RawHash h] ->
      [Causal m h e] ->
      m (Map (RawHash h) [RawHash h])
    go output [] = pure output
    go output (c : queue) = case c of
      One h _ -> go (Map.insert h [] output) queue
      Cons h _ (htail, mctail) -> do
        ctail <- mctail
        go (Map.insert h [htail] output) (ctail : queue)
      Merge h _ mtails -> do
        tails <- sequence mtails
        go (Map.insert h (Map.keys tails) output) (toList tails ++ queue)
