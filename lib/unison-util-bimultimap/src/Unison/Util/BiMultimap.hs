module Unison.Util.BiMultimap where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set

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

lookup :: Ord a => a -> BiMultimap a b -> Maybe (Set b)
lookup a (BiMultimap l _) = Map.lookup a l

lookupR :: Ord b => b -> BiMultimap a b -> Maybe a
lookupR b (BiMultimap _ r) = Map.lookup b r
