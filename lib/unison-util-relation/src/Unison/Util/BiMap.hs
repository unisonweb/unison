module Unison.Util.BiMap
  ( BiMap (..),
    empty,
    singleton,
    fromList,
    fromMap,
    toList,
    lookupL,
    lookupR,
    union,
    difference,
    insert,
    deleteL,
    deleteR,
    keysSetL,
    keysSetR,
  )
where

import Data.Map qualified as Map
import Data.Set (Set)
import Data.Tuple (swap)

-- | A bidirectional map between keys of type k and values of type v.
data BiMap k v = BiMap
  { forward :: Map.Map k v,
    backward :: Map.Map v k
  }
  deriving (Eq, Ord, Show)

-- | Combine two BiMaps. In case of key or value collisions, the entries from
-- the second BiMap take precedence.
instance (Ord k, Ord v) => Semigroup (BiMap k v) where
  BiMap f1 b1 <> BiMap f2 b2 =
    BiMap (Map.union f2 f1) (Map.union b2 b1)

instance (Ord k, Ord v) => Monoid (BiMap k v) where
  mempty = BiMap Map.empty Map.empty

empty :: (Ord k, Ord v) => BiMap k v
empty = mempty

singleton :: k -> v -> BiMap k v
singleton k v = BiMap (Map.singleton k v) (Map.singleton v k)

fromList :: (Ord k, Ord v) => [(k, v)] -> BiMap k v
fromList kvs =
  let forward = Map.fromList kvs
      backward = Map.fromList (swap <$> Map.toList forward)
   in BiMap {forward, backward}

fromMap :: (Ord k, Ord v) => Map.Map k v -> BiMap k v
fromMap f =
  let b = Map.fromList (swap <$> Map.toList f)
   in BiMap f b

toList :: BiMap k v -> [(k, v)]
toList (BiMap f _) = Map.toList f

lookupL :: (Ord k) => k -> BiMap k v -> Maybe v
lookupL k (BiMap f _) = Map.lookup k f

lookupR :: (Ord v) => v -> BiMap k v -> Maybe k
lookupR v (BiMap _ b) = Map.lookup v b

union :: (Ord k, Ord v) => BiMap k v -> BiMap k v -> BiMap k v
union = (<>)

difference :: (Ord k, Ord v) => BiMap k v -> BiMap k v -> BiMap k v
difference (BiMap f1 b1) (BiMap f2 b2) =
  let f' = Map.difference f1 f2
      b' = Map.difference b1 b2
   in BiMap f' b'

insert :: (Ord k, Ord v) => k -> v -> BiMap k v -> BiMap k v
insert k v (BiMap f b) =
  BiMap (Map.insert k v f) (Map.insert v k b)

deleteL :: (Ord k, Ord v) => k -> BiMap k v -> BiMap k v
deleteL k bm = case lookupL k bm of
  Nothing -> bm
  Just v ->
    BiMap
      (Map.delete k (forward bm))
      (Map.delete v (backward bm))

deleteR :: (Ord v, Ord k) => v -> BiMap k v -> BiMap k v
deleteR v bm = flipped $ deleteL v (flipped bm)

keysSetL :: BiMap k v -> Set k
keysSetL (BiMap f _) = Map.keysSet f

keysSetR :: BiMap k v -> Set v
keysSetR (BiMap _ b) = Map.keysSet b

flipped :: BiMap k v -> BiMap v k
flipped (BiMap f b) = BiMap b f
