-- | A left-unique relation.
module Unison.Util.BiMultimap
  ( BiMultimap,
    Unison.Util.BiMultimap.empty,

    -- ** Lookup
    lookupDom,
    lookupRan,
    lookupPreimage,

    -- ** Traverse
    unsafeTraverseDom,

    -- ** Maps
    domain,
    range,

    -- ** Sets
    ran,

    -- ** Insert
    insert,
    unsafeInsert,

    -- ** Union
    unsafeUnion,
  )
where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as Set.NonEmpty
import Unison.Prelude
import Unison.Util.Map qualified as Map

-- | A left-unique relation.
--
-- "Left-unique" means that for all @(x, y)@ in the relation, @y@ is related only to @x@.
data BiMultimap a b = BiMultimap
  { toMultimap :: !(Map a (NESet b)),
    toMapR :: !(Map b a)
  }
  deriving (Eq, Ord, Show)

-- | An empty left-unique relation.
empty :: (Ord a, Ord b) => BiMultimap a b
empty = BiMultimap mempty mempty

-- | Look up the set of @b@ related to an @a@.
--
-- /O(log a)/.
lookupDom :: Ord a => a -> BiMultimap a b -> Set b
lookupDom a =
  lookupDom_ a . domain

lookupDom_ :: Ord a => a -> Map a (NESet b) -> Set b
lookupDom_ x xs =
  maybe Set.empty Set.NonEmpty.toSet (Map.lookup x xs)

-- | Look up the @a@ related to a @b@.
--
-- /O(log b)/.
lookupRan :: Ord b => b -> BiMultimap a b -> Maybe a
lookupRan b (BiMultimap _ r) =
  Map.lookup b r

-- | Look up the preimage of a @b@, that is, the set of @b@ that are related to the same @a@ as the input @b@.
--
-- /O(log a + log b)
lookupPreimage :: (Ord a, Ord b) => b -> BiMultimap a b -> Set b
lookupPreimage y (BiMultimap domain range) =
  maybe Set.empty (\x -> lookupDom_ x domain) (Map.lookup y range)

-- | Traverse over the domain a left-unique relation.
--
-- The caller is responsible for maintaining left-uniqueness.
unsafeTraverseDom :: forall a b m x. (Monad m, Ord b, Ord x) => (a -> m b) -> BiMultimap a x -> m (BiMultimap b x)
unsafeTraverseDom f m =
  foldr g pure (Map.toList (domain m)) Unison.Util.BiMultimap.empty
  where
    g :: (a, NESet x) -> (BiMultimap b x -> m (BiMultimap b x)) -> (BiMultimap b x -> m (BiMultimap b x))
    g (a, xs) acc (BiMultimap domain0 range0) = do
      !b <- f a
      acc $! BiMultimap (Map.insert b xs domain0) (deriveRangeFromDomain b xs range0)

domain :: BiMultimap a b -> Map a (NESet b)
domain = toMultimap

range :: BiMultimap a b -> Map b a
range = toMapR

-- | Returns the range in the relation, as a Set, in its entirety.
--
-- /O(a)/.
ran :: BiMultimap a b -> Set b
ran =
  Map.keysSet . toMapR

-- | Insert a pair into a left-unique relation, maintaining left-uniqueness, preferring the latest inserted element.
--
-- That is, if a left-unique relation already contains the pair @(x, y)@, then inserting the pair @(z, y)@ will cause
-- the @(x, y)@ pair to be deleted.
insert :: (Ord a, Ord b) => a -> b -> BiMultimap a b -> BiMultimap a b
insert a b m@(BiMultimap l r) =
  case Map.alterF (upsertFunc a) b r of
    (Ignored, _) -> m
    (Inserted, r') -> BiMultimap l' r'
    (Replaced old, r') ->
      let l'' = Map.update (Set.NonEmpty.nonEmptySet . Set.NonEmpty.delete b) old l'
       in BiMultimap l'' r'
  where
    l' = Map.upsert (maybe (Set.NonEmpty.singleton b) (Set.NonEmpty.insert b)) a l

-- @upsertFunc x@ returns a function that upserts @x@, suitable for passing to @Map.alterF@.
upsertFunc :: Eq a => a -> Maybe a -> (UpsertResult a, Maybe a)
upsertFunc new existing =
  case existing of
    Nothing -> (Inserted, Just new)
    Just old
      | old == new -> (Ignored, existing)
      | otherwise -> (Replaced old, Just new)

data UpsertResult old
  = Ignored -- Ignored because an equivalent thing was already there
  | Inserted -- Inserted something new
  | Replaced old -- Replaced what was there, here's the old thing

-- | Like @insert x y@, but the caller is responsible maintaining left-uniqueness.
unsafeInsert :: (Ord a, Ord b) => a -> b -> BiMultimap a b -> BiMultimap a b
unsafeInsert x y (BiMultimap xs ys) =
  BiMultimap
    (Map.upsert (maybe (Set.NonEmpty.singleton y) (Set.NonEmpty.insert y)) x xs)
    (Map.insert y x ys)

-- | Union two left-unique relations together.
--
-- The caller is responsible for maintaining left-uniqueness.
unsafeUnion :: (Ord a, Ord b) => BiMultimap a b -> BiMultimap a b -> BiMultimap a b
unsafeUnion xs ys =
  BiMultimap
    (Map.unionWith Set.NonEmpty.union (toMultimap xs) (toMultimap ys))
    (Map.union (toMapR xs) (toMapR ys))

------------------------------------------------------------------------------------------------------------------------

-- @deriveRangeFromDomain x ys range@ is a helper that inserts @(x, y1)@, @(x, y2)@, ... into range @r@.
deriveRangeFromDomain :: Ord b => a -> NESet b -> Map b a -> Map b a
deriveRangeFromDomain x ys acc =
  foldr (flip Map.insert x) acc ys
{-# INLINE deriveRangeFromDomain #-}
