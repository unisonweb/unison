{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Unison.Codebase.Causal2 where

import           Prelude                 hiding ( head
                                                , read
                                                , sequence
                                                )
import           Control.Applicative            ( liftA2 )
import           Control.Lens                   ( (<&>) )
import           Control.Monad                  ( when )
import           Control.Monad.Extra            ( ifM )
import           Control.Monad.Loops            ( anyM )
import           Data.List                      ( foldl1' )
import           Unison.Hash                    ( Hash )
-- import qualified Unison.Hash                   as H
import qualified Unison.Hashable               as Hashable
import           Unison.Hashable                ( Hashable )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import           Data.Foldable                  ( for_, toList )
import           Util                           ( bind2 )

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
           associative and commutative and satisfies:
  * `before c1 (merge c1 c2)`
  * `before c2 (merge c1 c2)`
* `sequence : Causal a -> Causal a -> Causal a`, which is defined as
              `sequence c1 c2 = cons (head c2) (merge c1 c2)`.
  * `before c1 (sequence c1 c2)`
  * `head (sequence c1 c2) == head c2`
-}

newtype RawHash a = RawHash { unRawHash :: Hash }
  deriving (Eq, Ord, Show)

-- h is the type of the pure data structure that will be hashed and used as
-- an index; e.g. h = Branch00, e = Branch0 m
data Causal m h e
  = One { currentHash :: RawHash h, head :: e }
  | Cons { currentHash :: RawHash h, head :: e, tail :: (RawHash h, m (Causal m h e)) }
  -- The merge operation `<>` flattens and normalizes for order
  | Merge { currentHash :: RawHash h, head :: e, tails :: Map (RawHash h) (m (Causal m h e)) }

-- A serializer `Causal m h e`. Nonrecursive -- only responsible for
-- writing a single node of the causal structure.
data Raw h e
  = RawOne e
  | RawCons e (RawHash h)
  | RawMerge e (Set (RawHash h))

-- Don't need to deserialize the `e` to calculate `before`.
data Tails h
  = TailsOne
  | TailsCons (RawHash h)
  | TailsMerge (Set (RawHash h))

type Deserialize m h e = RawHash h -> m (Raw h e)

read :: Functor m => Deserialize m h e -> RawHash h -> m (Causal m h e)
read d h = go <$> d h where
  go = \case
    RawOne e -> One h e
    RawCons e tailHash -> Cons h e (tailHash, read d tailHash)
    RawMerge e tailHashes ->
      Merge h e (Map.fromList [(h, read d h) | h <- toList tailHashes ])

type Serialize m h e = RawHash h -> Raw h e -> m ()

-- Sync a causal to some persistent store, stopping when hitting a Hash which
-- has already been written, according to the `exists` function provided.
sync :: Monad m => (RawHash h -> m Bool) -> Serialize m h e -> Causal m h e -> m ()
sync exists serialize c = do
  b <- exists (currentHash c)
  when (not b) $ go c
  where
    go c = case c of
      One currentHash head -> serialize currentHash $ RawOne head
      Cons currentHash head (tailHash, tailm) -> do
        -- write out the tail first, so what's on disk is always valid
        b <- exists tailHash
        when (not b) $ go =<< tailm
        serialize currentHash (RawCons head tailHash)
      Merge currentHash head tails -> do
        for_ (Map.toList tails) $ \(hash, cm) -> do
          b <- exists hash
          when (not b) $ go =<< cm
        serialize currentHash (RawMerge head (Map.keysSet tails))

instance Eq (Causal m h a) where
  a == b = currentHash a == currentHash b

instance Ord (Causal m h a) where
  a <= b = currentHash a <= currentHash b

instance Hashable (RawHash h) where
  tokens (RawHash h) = Hashable.tokens h

merge :: (Monad m, Semigroup e) => Causal m h e -> Causal m h e -> m (Causal m h e)
a `merge` b =
  ifM (before a b) (pure b) . ifM (before b a) (pure a) $ case (a, b) of
    (Merge _ _ tls, Merge _ _ tls2) -> merge0 $ Map.union tls tls2
    (Merge _ _ tls, b) -> merge0 $ Map.insert (currentHash b) (pure b) tls
    (b, Merge _ _ tls) -> merge0 $ Map.insert (currentHash b) (pure b) tls
    (a, b) ->
      merge0 $ Map.fromList [(currentHash a, pure a), (currentHash b, pure b)]
 where
 -- implementation detail, form a `Merge`
 merge0
   :: (Applicative m, Semigroup e) => Map (RawHash h) (m (Causal m h e)) -> m (Causal m h e)
 merge0 m =
   let e = if Map.null m
         then error "Causal.merge0 empty map"
         else foldl1' (liftA2 (<>)) (fmap head <$> Map.elems m)
       h = hash (Map.keys m) -- sorted order
   in  e <&> \e -> Merge (RawHash h) e m


mergeWithM :: forall m h e. Monad m => (e -> e -> m e) -> Causal m h e -> Causal m h e -> m (Causal m h e)
mergeWithM f a b =
  ifM (before a b) (pure b) . ifM (before b a) (pure a) $ case (a, b) of
  (Merge _ _ tls, Merge _ _ tls2) -> merge0 $ Map.union tls tls2
  (Merge _ _ tls, b) -> merge0 $ Map.insert (currentHash b) (pure b) tls
  (b, Merge _ _ tls) -> merge0 $ Map.insert (currentHash b) (pure b) tls
  (a, b) ->
    merge0 $ Map.fromList [(currentHash a, pure a), (currentHash b, pure b)]
  where
  -- implementation detail, form a `Merge`
  merge0 :: Map (RawHash h) (m (Causal m h e)) -> m (Causal m h e)
  merge0 m =
    let e :: m e
        e = if Map.null m
          then error "Causal.merge0 empty map"
          else foldl1' (bind2 f) (fmap head <$> Map.elems m)
          -- else foldlM1 f <$> (fmap head <$> Map.elems m)
        h = hash (Map.keys m) -- sorted order
    in  e <&> \e -> Merge (RawHash h) e m

-- Does `h2` incorporate all of `h1`?
before :: Monad m => Causal m h e -> Causal m h e -> m Bool
before h1 h2 = go h1 h2
 where
  -- stopping condition if both are equal
  go h1 h2 | h1 == h2 = pure True
  -- otherwise look through tails if they exist
  go _  (One _ _    ) = pure False
  go h1 (Cons _ _ tl) = snd tl >>= go h1
  -- `m1` is a submap of `m2`
  go (Merge _ _ m1) (Merge _ _ m2) | all (`Map.member` m2) (Map.keys m1) =
    pure True
  -- if not, see if `h1` is a subgraph of one of the tails
  go h1 (Merge _ _ tls) =
    (||) <$> pure (Map.member (currentHash h1) tls) <*> anyM (>>= go h1)
                                                             (Map.elems tls)
  -- Exponential algorithm of checking that all paths are present
  -- in `h2` isn't necessary because of how merges are flattened
  --go (Merge _ _ m1) h2@(Merge _ _ _)
  --  all (\h1 -> go h1 h2) (Map.elems m1)

hash :: Hashable e => e -> Hash
hash = Hashable.accumulate'

step :: (Applicative m, Hashable e) => (e -> e) -> Causal m h e -> Causal m h e
step f c = f (head c) `cons` c

stepDistinct :: (Applicative m, Eq e, Hashable e) => (e -> e) -> Causal m h e -> Causal m h e
stepDistinct f c = f (head c) `consDistinct` c

stepIf
  :: (Applicative m, Hashable e)
  => (e -> Bool)
  -> (e -> e)
  -> Causal m h e
  -> Causal m h e
stepIf cond f c = if (cond $ head c) then step f c else c

stepM
  :: (Applicative m, Hashable e) => (e -> m e) -> Causal m h e -> m (Causal m h e)
stepM f c = (`cons` c) <$> f (head c)

stepDistinctM
  :: (Applicative m, Eq e, Hashable e) => (e -> m e) -> Causal m h e -> m (Causal m h e)
stepDistinctM f c = (`consDistinct` c) <$> f (head c)

one :: Hashable e => e -> Causal m h e
one e = One (RawHash $ hash e) e

cons :: (Applicative m, Hashable e) => e -> Causal m h e -> Causal m h e
cons e tl =
  Cons (RawHash $ hash [hash e, unRawHash . currentHash $ tl]) e (currentHash tl, pure tl)

consDistinct :: (Applicative m, Eq e, Hashable e) => e -> Causal m h e -> Causal m h e
consDistinct e tl =
  if head tl == e then tl
  else cons e tl

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
