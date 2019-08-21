{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Unison.Codebase.Causal where

import Unison.Prelude

import           Prelude                 hiding ( head
                                                , tail
                                                , read
                                                )
import           Control.Lens                   ( (<&>) )
import           Control.Monad.Loops            ( anyM )
import           Data.List                      ( foldl1' )
import           Data.Sequence                  ( ViewL(..) )
import qualified Data.Sequence                 as Seq
import           Unison.Hash                    ( Hash )
import qualified Unison.Hashable               as Hashable
import           Unison.Hashable                ( Hashable )
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
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
  deriving (Eq, Ord)

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

-- A serializer `Causal m h e`. Nonrecursive -- only responsible for
-- writing a single node of the causal structure.
data Raw h e
  = RawOne e
  | RawCons e (RawHash h)
  | RawMerge e (Set (RawHash h))

rawHead :: Raw h e -> e
rawHead (RawOne e    ) = e
rawHead (RawCons  e _) = e
rawHead (RawMerge e _) = e

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
sync
  :: Monad m => (RawHash h -> m Bool) -> Serialize m h e -> Causal m h e -> m ()
sync exists serialize c = do
  b <- exists (currentHash c)
  unless b $ go c
 where
  go c = case c of
    One currentHash head -> serialize currentHash $ RawOne head
    Cons currentHash head (tailHash, tailm) -> do
      -- write out the tail first, so what's on disk is always valid
      b <- exists tailHash
      unless b $ go =<< tailm
      serialize currentHash (RawCons head tailHash)
    Merge currentHash head tails -> do
      for_ (Map.toList tails) $ \(hash, cm) -> do
        b <- exists hash
        unless b $ go =<< cm
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
  go Set.empty Set.empty (Seq.singleton $ pure a) . Seq.singleton $ pure b
 where
  go seenLeft seenRight remainingLeft remainingRight =
    case (Seq.viewl remainingLeft, Seq.viewl remainingRight) of
      (Seq.EmptyL, _         ) -> pure Nothing
      (_         , Seq.EmptyL) -> pure Nothing
      (a :< as   , b :< bs   ) -> do
        left <- a
        -- Have we seen the left node before on the right?
        if Set.member (currentHash left) seenRight
          then pure $ Just left
          else do
            right <- b
            -- Have we seen the right node before on the left?
            if Set.member (currentHash right) seenLeft
              then pure $ Just right
              -- Are these two previously unseen nodes the same?
              else if currentHash left == currentHash right
                then pure $ Just left
                -- Descend in to the children
                else case (left, right) of
                  (One h _, _) ->
                    go (Set.insert h seenLeft) seenRight as remainingRight
                  (_, One h _) ->
                    go seenLeft (Set.insert h seenRight) remainingLeft bs
                  _ -> descend (currentHash left)
                               (currentHash right)
                               (children left)
                               (children right)
       where
        descend h1 h2 r1 r2 = go (Set.insert h1 seenLeft)
                                 (Set.insert h2 seenRight)
                                 (as <> r1)
                                 (bs <> r2)

children :: Causal m h e -> Seq (m (Causal m h e))
children (One _ _         ) = Seq.empty
children (Cons  _ _ (_, t)) = Seq.singleton t
children (Merge _ _ ts    ) = Seq.fromList $ Map.elems ts

threeWayMerge
  :: forall m h e d
   . (Monad m, Hashable e)
  => (e -> e -> m e)
  -> (e -> e -> m d)
  -> (e -> d -> m e)
  -> Causal m h e
  -> Causal m h e
  -> m (Causal m h e)
threeWayMerge combine diff patch = mergeInternal merge0
 where
  merge0 :: Map (RawHash h) (m (Causal m h e)) -> m (Causal m h e)
  merge0 m =
    let
      k left right = do
        a           <- left
        b           <- right
        mayAncestor <- lca a b
        case mayAncestor of
          Nothing       -> mergeWithM combine a b
          Just ancestor -> do
            da      <- patch (head ancestor) =<< diff (head ancestor) (head a)
            db      <- patch (head ancestor) =<< diff (head ancestor) (head b)
            newHead <- head ancestor `combine` da >>= combine db
            let h = hash (newHead, Map.keys m)
            pure . Merge (RawHash h) newHead $ Map.fromList
              [(currentHash a, pure a), (currentHash b, pure b)]
    in  if Map.null m
          then error "Causal.threeWayMerge empty map"
          else foldl1' k $ Map.elems m

mergeInternal
  :: forall m h e
   . Monad m
  => (Map (RawHash h) (m (Causal m h e)) -> m (Causal m h e))
  -> Causal m h e
  -> Causal m h e
  -> m (Causal m h e)
mergeInternal f a b =
  ifM (before a b) (pure b) . ifM (before b a) (pure a) $ case (a, b) of
    (Merge _ _ tls, Merge _ _ tls2) -> f $ Map.union tls tls2
    (Merge _ _ tls, b) -> f $ Map.insert (currentHash b) (pure b) tls
    (b, Merge _ _ tls) -> f $ Map.insert (currentHash b) (pure b) tls
    (a, b) ->
      f $ Map.fromList [(currentHash a, pure a), (currentHash b, pure b)]

mergeWithM
  :: forall m h e
   . Monad m
  => (e -> e -> m e)
  -> Causal m h e
  -> Causal m h e
  -> m (Causal m h e)
mergeWithM f = mergeInternal merge0
 where
  -- implementation detail, form a `Merge`
  merge0 :: Map (RawHash h) (m (Causal m h e)) -> m (Causal m h e)
  merge0 m =
    let e :: m e
        e = if Map.null m
          then error "Causal.merge0 empty map"
          else foldl1' (bind2 f) (fmap head <$> Map.elems m)
        h = hash (Map.keys m) -- sorted order
    in  e <&> \e -> Merge (RawHash h) e m

-- Does `h2` incorporate all of `h1`?
before :: Monad m => Causal m h e -> Causal m h e -> m Bool
before = go
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
stepIf cond f c = if cond (head c) then step f c else c

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

uncons :: Applicative m => Causal m h e -> m (Maybe (e, Causal m h e))
uncons c = case c of
  Cons _ e (_,tl) -> fmap (e,) . Just <$> tl
  _ -> pure Nothing

transform :: Functor m => (forall a . m a -> n a) -> Causal m h e -> Causal n h e
transform nt c = case c of
  One h e -> One h e
  Cons h e (ht, tl) -> Cons h e (ht, nt (transform nt <$> tl))
  Merge h e tls -> Merge h e $ Map.map (\mc -> nt (transform nt <$> mc)) tls

-- foldHistoryUntil some condition on the accumulator is met,
-- attempting to work backwards fairly through merge nodes
-- (rather than following one back all the way to its root before working
-- through others).  Returns Unsatisfied if the condition was never satisfied,
-- otherwise Satisfied.
data FoldHistoryResult a = Satisfied a | Unsatisfied a deriving (Eq,Ord,Show)
foldHistoryUntil :: forall m h e a. (Monad m) => --(Show a, Show e) =>
  (a -> e -> (a, Bool)) -> a -> Causal m h e -> m (FoldHistoryResult a)
foldHistoryUntil f a c = step a mempty (pure c) where
  step :: a -> Set (RawHash h) -> Seq (Causal m h e) -> m (FoldHistoryResult a)
  --step a seen rest | trace ("step a=" ++ show a ++ " seen=" ++ (show . fmap (take 3 . show) . toList) seen ++ " rest=" ++ show rest) False = undefined
  step a _seen Seq.Empty = pure (Unsatisfied a)
  step a seen (c Seq.:<| rest) = case f a (head c) of
    (a, True) -> pure (Satisfied a)
    (a, False) -> do
      tails <- case c of
        One{} -> pure mempty
        Cons{} -> Seq.singleton <$> snd (tail c)
        Merge{} -> Seq.fromList <$> (sequenceA . toList . tails) c
      step a (Set.insert (currentHash c) seen) (rest <> tails)

hashToRaw ::
  forall m h e. Monad m => Causal m h e -> m (Map (RawHash h) [RawHash h])
hashToRaw c = go mempty [c] where
  go :: Map (RawHash h) [RawHash h] -> [Causal m h e]
                                    -> m (Map (RawHash h) [RawHash h])
  go output [] = pure output
  go output (c : queue) = case c of
    One h _ -> go (Map.insert h [] output) queue
    Cons h _ (htail, mctail) -> do
      ctail <- mctail
      go (Map.insert h [htail] output) (ctail : queue)
    Merge h _ mtails -> do
      tails <- sequence mtails
      go (Map.insert h (Map.keys tails) output) (toList tails ++ queue)
