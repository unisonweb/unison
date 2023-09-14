-- | A left-unique relation.
module Unison.Util.BiMultimap
  ( BiMultimap,
    Unison.Util.BiMultimap.empty,

    -- ** Lookup
    lookupDom,
    lookupRan,

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
  { toMultimap :: Map a (NESet b),
    toMapR :: Map b a
  }
  deriving (Eq, Ord, Show)

-- | An empty left-unique relation.
empty :: (Ord a, Ord b) => BiMultimap a b
empty = BiMultimap mempty mempty

-- | Look up the set of @b@ related to an @a@.
--
-- /O(log a)/.
lookupDom :: Ord a => a -> BiMultimap a b -> Set b
lookupDom a (BiMultimap l _) =
  maybe Set.empty Set.NonEmpty.toSet (Map.lookup a l)

-- | Look up the @a@ related to a @b@.
--
-- /O(log b)/.
lookupRan :: Ord b => b -> BiMultimap a b -> Maybe a
lookupRan b (BiMultimap _ r) =
  Map.lookup b r

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

-- | Like @insert x y@, except the caller is responsible for ensuring that @y@ is not already related to a different
-- @x@.
unsafeInsert :: (Ord a, Ord b) => a -> b -> BiMultimap a b -> BiMultimap a b
unsafeInsert x y (BiMultimap xs ys) =
  BiMultimap
    (Map.upsert (maybe (Set.NonEmpty.singleton y) (Set.NonEmpty.insert y)) x xs)
    (Map.insert y x ys)

-- | Union two left-unique relations together. The caller is responsible for ensuring that for all @(x, y)@ in either
-- input relation, @y@ is only associated with only @x@ across both relations, i.e. the result is left-unique as well.
unsafeUnion :: (Ord a, Ord b) => BiMultimap a b -> BiMultimap a b -> BiMultimap a b
unsafeUnion xs ys =
  BiMultimap
    (Map.unionWith Set.NonEmpty.union (toMultimap xs) (toMultimap ys))
    (Map.union (toMapR xs) (toMapR ys))
