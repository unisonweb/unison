module Unison.Util.BiMultimap where

import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Unison.Prelude

data BiMultimap a b = BiMultimap {toMultimap :: Map a (Set b), toMapR :: Map b a} deriving (Eq, Ord, Show)

empty :: (Ord a, Ord b) => BiMultimap a b
empty = BiMultimap mempty mempty

-- |
-- >>> insert 1 'a' empty
-- >>> insert 1 'b' (insert 1 'a' empty)
-- >>> insert 2 'b' (insert 1 'a' (insert 1 'b' empty))
-- >>> insert 2 'a' (insert 2 'b' (insert 1 'a' (insert 1 'b' empty)))
-- BiMultimap {toMultimap = fromList [(1,fromList "a")], toMapR = fromList [('a',1)]}
-- BiMultimap {toMultimap = fromList [(1,fromList "ab")], toMapR = fromList [('a',1),('b',1)]}
-- BiMultimap {toMultimap = fromList [(1,fromList "a"),(2,fromList "b")], toMapR = fromList [('a',1),('b',2)]}
-- BiMultimap {toMultimap = fromList [(2,fromList "ab")], toMapR = fromList [('a',2),('b',2)]}
insert :: (Ord a, Ord b) => a -> b -> BiMultimap a b -> BiMultimap a b
insert a b m@(BiMultimap l r) =
  let lInserted = Map.insertWith (<>) a (Set.singleton b) l
      lDeleted a' = Map.alter (\(fromJust -> s) -> let s' = Set.delete b s in if Set.null s' then Nothing else Just s') a' lInserted
      rInserted = Map.insert b a r
   in case Map.lookup b r of
        Just a' -> if a' == a then m else BiMultimap (lDeleted a') rInserted
        Nothing -> BiMultimap lInserted rInserted

-- | Like @insert x y@, except the caller is responsible for ensuring that @y@ is not already related to a different
-- @x@. If it is, the resulting relation will have an internal structural violation.
unsafeInsert :: (Ord a, Ord b) => a -> b -> BiMultimap a b -> BiMultimap a b
unsafeInsert x y (BiMultimap xs ys) =
  BiMultimap
    (Map.alter (Just . maybe (Set.singleton y) (Set.insert y)) x xs)
    (Map.insert y x ys)

lookupDom :: Ord a => a -> BiMultimap a b -> Set b
lookupDom a (BiMultimap l _) =
  fromMaybe Set.empty (Map.lookup a l)

lookupRan :: Ord b => b -> BiMultimap a b -> Maybe a
lookupRan b (BiMultimap _ r) =
  Map.lookup b r

-- | Returns the domain in the relation, as a Set, in its entirety.
--
-- /O(a)/.
ran :: BiMultimap a b -> Set b
ran =
  Map.keysSet . toMapR
