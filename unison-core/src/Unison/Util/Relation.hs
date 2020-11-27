{-# LANGUAGE ViewPatterns #-}

module Unison.Util.Relation where

import Data.Bifunctor (first, second)
import qualified Data.List as List
import qualified Data.Map as M
import qualified Data.Map as Map
import qualified Data.Set as S
import qualified Unison.Hashable as H
import Unison.Prelude hiding (empty, toList)
import Prelude hiding (filter, map, null)

-- |
-- This implementation avoids using @"Set (a,b)"@ because
-- it it is necessary to search for an item without knowing both @D@ and @R@.
--
-- In "Set", you must know both values to search.
--
-- Thus, we have are two maps to updated together.
--
-- 1. Always be careful with the associated set of the key.
--
-- 2. If you union two relations, apply union to the set of values.
--
-- 3. If you subtract, take care when handling the set of values.
--
-- As a multi-map, each key is associated with a Set of values v.
--
-- We do not allow the associations with the 'empty' Set.
data Relation a b = Relation
  { domain :: M.Map a (Set b),
    range :: M.Map b (Set a)
  }
  deriving (Eq, Ord)

instance (Show a, Show b) => Show (Relation a b) where
  show = show . toList

-- * Functions about relations

difference :: (Ord a, Ord b) => Relation a b -> Relation a b -> Relation a b
difference a b = fromList . S.toList $ diffSet
  where
    diffSet = S.difference seta setb
    seta = S.fromList . toList $ a
    setb = S.fromList . toList $ b

-- The size is calculated using the domain.

-- |  @size r@ returns the number of tuples in the relation.
size :: Relation a b -> Int
size r = M.foldr' ((+) . S.size) 0 (domain r)

-- | Construct a relation with no elements.
empty :: Relation a b
empty = Relation M.empty M.empty

-- |
-- The list must be formatted like: [(k1, v1), (k2, v2),..,(kn, vn)].
fromList :: (Ord a, Ord b) => [(a, b)] -> Relation a b
fromList xs =
  Relation
    { domain = M.fromListWith S.union $ snd2Set xs,
      range = M.fromListWith S.union $ flipAndSet xs
    }
  where
    snd2Set = List.map (\(x, y) -> (x, S.singleton y))
    flipAndSet = List.map (\(x, y) -> (y, S.singleton x))

-- |
-- Builds a List from a Relation.
toList :: Relation a b -> [(a, b)]
toList r =
  concatMap (\(x, y) -> zip (repeat x) (S.toList y)) (M.toList . domain $ r)

-- | Builds a Set from a Relation
toSet :: (Ord a, Ord b) => Relation a b -> S.Set (a, b)
toSet = S.fromList . toList

-- |
-- Builds a 'Relation' consiting of an association between: @x@ and @y@.
singleton :: a -> b -> Relation a b
singleton x y =
  Relation
    { domain = M.singleton x (S.singleton y),
      range = M.singleton y (S.singleton x)
    }

-- | The 'Relation' that results from the union of two relations: @r@ and @s@.
union :: (Ord a, Ord b) => Relation a b -> Relation a b -> Relation a b
union r s =
  Relation
    { domain = M.unionWith S.union (domain r) (domain s),
      range = M.unionWith S.union (range r) (range s)
    }

intersection :: (Ord a, Ord b) => Relation a b -> Relation a b -> Relation a b
intersection r s
  | size r > size s = intersection s r
  | otherwise = filter (\(a, b) -> member a b s) r

outerJoinDomMultimaps ::
  (Ord a, Ord b, Ord c) =>
  Relation a b ->
  Relation a c ->
  Map a (Set b, Set c)
outerJoinDomMultimaps b c =
  Map.fromList
    [(a, (lookupDom a b, lookupDom a c)) | a <- S.toList (dom b <> dom c)]

outerJoinRanMultimaps ::
  (Ord a, Ord b, Ord c) =>
  Relation a c ->
  Relation b c ->
  Map c (Set a, Set b)
outerJoinRanMultimaps a b = outerJoinDomMultimaps (swap a) (swap b)

innerJoinDomMultimaps ::
  (Ord a, Ord b, Ord c) =>
  Relation a b ->
  Relation a c ->
  Map a (Set b, Set c)
innerJoinDomMultimaps b c =
  Map.fromList
    [ (a, (lookupDom a b, lookupDom a c))
      | a <- S.toList $ dom b `S.intersection` dom c
    ]

innerJoinRanMultimaps ::
  (Ord a, Ord b, Ord c) =>
  Relation a c ->
  Relation b c ->
  Map c (Set a, Set b)
innerJoinRanMultimaps a b = innerJoinDomMultimaps (swap a) (swap b)

joinDom :: (Ord a, Ord b, Ord c) => Relation a b -> Relation a c -> Relation a (b, c)
joinDom b c = swap $ joinRan (swap b) (swap c)

-- joinRan [(1, 'x'), (2, 'x'), (3, 'z')] [(true, 'x'), (true, 'y'), (false, 'z')]
--      == [((1,true), 'x'), ((2,true), 'x'), ((3,false), 'z')]
joinRan :: (Ord a, Ord b, Ord c) => Relation a c -> Relation b c -> Relation (a, b) c
joinRan a b =
  fromList
    [ ((a, b), c)
      | c <- S.toList $ ran a `S.intersection` ran b,
        a <- S.toList $ lookupRan c a,
        b <- S.toList $ lookupRan c b
    ]

---------------------------------------------------------------

-- |
-- This fragment provided by:
--
-- @
-- \  Module      :  Data.Map
-- \  Copyright   :  (c) Daan Leijen 2002
-- \                 (c) Andriy Palamarchuk 2008
-- \  License     :  BSD-style
-- \  Maintainer  :  libraries\@haskell.org
-- \  Stability   :  provisional
-- \  Portability :  portable
-- @
foldlStrict :: (a -> b -> a) -> a -> [b] -> a
foldlStrict f z xs = case xs of
  [] -> z
  (x : xx) -> let z' = f z x in seq z' (foldlStrict f z' xx)

---------------------------------------------------------------

-- | Union a list of relations using the 'empty' relation.
unions :: (Ord a, Ord b) => [Relation a b] -> Relation a b
unions = foldlStrict union empty

-- | Insert a relation @ x @ and @ y @ in the relation @ r @
insert :: (Ord a, Ord b) => a -> b -> Relation a b -> Relation a b
insert x y r =
  -- r { domain = domain', range = range' }
  Relation domain' range'
  where
    domain' = M.insertWith S.union x (S.singleton y) (domain r)
    range' = M.insertWith S.union y (S.singleton x) (range r)

-- $deletenotes
--
-- The deletion is not difficult but is delicate:
--
-- @
--   r = { domain {  (k1, {v1a, v3})
--                 ,  (k2, {v2a})
--                 ,  (k3, {v3b, v3})
--                 }
--       , range   {  (v1a, {k1}
--                 ,  (v2a, {k2{
--                 ,  (v3 , {k1, k3}
--                 ,  (v3b, {k3}
--                 }
--      }
-- @
--
--   To delete (k,v) in the relation do:
--    1. Working with the domain:
--       1a. Delete v from the Set VS associated with k.
--       1b. If VS is empty, delete k in the domain.
--    2. Working in the range:
--       2a. Delete k from the Set VS associated with v.
--       2b. If VS is empty, delete v in the range.

-- |  Delete an association in the relation.
delete :: (Ord a, Ord b) => a -> b -> Relation a b -> Relation a b
delete x y r = r {domain = domain', range = range'}
  where
    domain' = M.update (erase y) x (domain r)
    range' = M.update (erase x) y (range r)
    erase e s = if S.singleton e == s then Nothing else Just $ S.delete e s

-- | The Set of values associated with a value in the domain.
lookupDom' :: Ord a => a -> Relation a b -> Maybe (Set b)
lookupDom' x r = M.lookup x (domain r)

-- | The Set of values associated with a value in the range.
lookupRan' :: Ord b => b -> Relation a b -> Maybe (Set a)
lookupRan' y r = M.lookup y (range r)

-- | True if the element @ x @ exists in the domain of @ r @.
memberDom :: Ord a => a -> Relation a b -> Bool
memberDom x r = isJust $ lookupDom' x r

-- | True if the element exists in the range.
memberRan :: Ord b => b -> Relation a b -> Bool
memberRan y r = isJust $ lookupRan' y r

filterDom :: (Ord a, Ord b) => (a -> Bool) -> Relation a b -> Relation a b
filterDom f r = S.filter f (dom r) <| r

filterRan :: (Ord a, Ord b) => (b -> Bool) -> Relation a b -> Relation a b
filterRan f r = r |> S.filter f (ran r)

filter :: (Ord a, Ord b) => ((a, b) -> Bool) -> Relation a b -> Relation a b
filter f = fromList . List.filter f . toList

-- | Restricts the relation to domain elements having multiple range elements
filterManyDom :: (Ord a, Ord b) => Relation a b -> Relation a b
filterManyDom r = filterDom (`manyDom` r) r

-- |
-- True if the relation @r@ is the 'empty' relation.
null :: Relation a b -> Bool
null r = M.null $ domain r

-- Before 2010/11/09 null::Ord b =>  Relation a b -> Bool

-- | True if the relation contains the association @x@ and @y@
member :: (Ord a, Ord b) => a -> b -> Relation a b -> Bool
member x y r = case lookupDom' x r of
  Just s -> S.member y s
  Nothing -> False

-- | True if the relation /does not/ contain the association @x@ and @y@
notMember :: (Ord a, Ord b) => a -> b -> Relation a b -> Bool
notMember x y r = not $ member x y r

-- | True if a value appears more than one time in the relation.
manyDom :: Ord a => a -> Relation a b -> Bool
manyDom a = (> 1) . S.size . lookupDom a

manyRan :: Ord b => b -> Relation a b -> Bool
manyRan b = (> 1) . S.size . lookupRan b

-- | Returns the domain in the relation, as a Set, in its entirety.
dom :: Relation a b -> Set a
dom r = M.keysSet (domain r)

-- | Returns the range of the relation, as a Set, in its entirety.
ran :: Relation a b -> Set b
ran r = M.keysSet (range r)

-- |
-- A compact set of sets the values of which can be @Just (Set x)@ or @Nothing@.
--
-- The cases of 'Nothing' are purged.
--
-- It is similar to 'concat'.
compactSet :: Ord a => Set (Maybe (Set a)) -> Set a
compactSet = S.fold (S.union . fromMaybe S.empty) S.empty

-- $selectops
--
-- Primitive implementation for the /right selection/ and /left selection/ operators.
--
-- PICA provides both operators:
--        '|>'  and  '<|'
-- and    '|$>' and '<$|'
--
-- in this library, for working with Relations and OIS (Ordered, Inductive Sets?).
--
-- PICA exposes the operators defined here, so as not to interfere with the abstraction
-- of the Relation type and because having access to Relation hidden components is a more
-- efficient implementation of the operation of restriction.
--
-- @
--     (a <$| b) r
--
--       denotes: for every element     @b@ from the Set      @B@,
--                select an element @a@     from the Set @A@     ,
--                              if  @a@
--                   is related to      @b@
--                   in @r@
-- @
--
-- @
--     (a |$> b) r
--
--       denotes: for every element @a@      from the Set @A@    ,
--                select an element     @b@  from the Set     @B@,
--                              if  @a@
--                   is related to      @b@
--                   in @r@
-- @
--
-- With regard to domain restriction and range restriction operators
-- of the language, those are described differently and return the domain or the range.

-- |
-- @(Case b <| r a)@
(<$|) :: (Ord a, Ord b) => Set a -> Set b -> Relation a b -> Set a
(as <$| bs) r = as `S.intersection` generarAS bs
  where
    generarAS = compactSet . S.map (`lookupRan'` r)

-- The subsets of the domain (a) associated with each @b@
-- such that @b@ in @B@ and (b) are in the range of the relation.
-- The expression 'S.map' returns a set of @Either (Set a)@.

-- |
-- @( Case a |> r b )@
(|$>) :: (Ord a, Ord b) => Set a -> Set b -> Relation a b -> Set b
(as |$> bs) r = bs `S.intersection` generarBS as
  where
    generarBS = compactSet . S.map (`lookupDom'` r)

-- | Domain restriction for a relation. Modeled on z.
(<|), restrictDom :: (Ord a, Ord b) => Set a -> Relation a b -> Relation a b
restrictDom = (<|)
s <| r =
  fromList $
    concatMap (\(x, y) -> zip (repeat x) (S.toList y)) (M.toList domain')
  where
    domain' = M.unions . List.map filtrar . S.toList $ s
    filtrar x = M.filterWithKey (\k _ -> k == x) dr
    dr = domain r -- just to memoize the value

-- | Range restriction for a relation. Modeled on z.
(|>), restrictRan :: (Ord a, Ord b) => Relation a b -> Set b -> Relation a b
restrictRan = (|>)
r |> t =
  fromList $
    concatMap (\(x, y) -> zip (S.toList y) (repeat x)) (M.toList range')
  where
    range' = M.unions . List.map filtrar . S.toList $ t
    filtrar x = M.filterWithKey (\k _ -> k == x) rr
    rr = range r -- just to memoize the value

-- Restrict the range to not include these `b`s
(||>) :: (Ord a, Ord b) => Relation a b -> Set b -> Relation a b
r ||> t = fromList [(a, b) | (a, b) <- toList r, not (b `S.member` t)]

subtractRan :: (Ord a, Ord b) => Set b -> Relation a b -> Relation a b
subtractRan = flip (||>)

-- Restrict the domain to not include these `a`
(<||), subtractDom :: (Ord a, Ord b) => Set a -> Relation a b -> Relation a b
s <|| r = fromList [(a, b) | (a, b) <- toList r, not (a `S.member` s)]
subtractDom = (<||)

-- Note:
--
--    As you have seen this implementation is expensive in terms
--    of storage. Information is registered twice.
--    For the operators |> and <| we follow a pattern used in
--    the @fromList@ constructor and @toList@ flattener:
--    It is enough to know one half of the Relation (the domain or
--    the range) to create to other half.

insertManyRan ::
  (Foldable f, Ord a, Ord b) => a -> f b -> Relation a b -> Relation a b
insertManyRan a bs r = foldl' (flip $ insert a) r bs

insertManyDom ::
  (Foldable f, Ord a, Ord b) => f a -> b -> Relation a b -> Relation a b
insertManyDom as b r = foldl' (flip $ flip insert b) r as

lookupRan :: Ord b => b -> Relation a b -> Set a
lookupRan b r = fromMaybe S.empty $ lookupRan' b r

lookupDom :: Ord a => a -> Relation a b -> Set b
lookupDom a r = fromMaybe S.empty $ lookupDom' a r

replaceDom :: (Ord a, Ord b) => a -> a -> Relation a b -> Relation a b
replaceDom a a' r =
  foldl' (\r b -> insert a' b $ delete a b r) r (lookupDom a r)

replaceRan :: (Ord a, Ord b) => b -> b -> Relation a b -> Relation a b
replaceRan b b' r =
  foldl' (\r a -> insert a b' $ delete a b r) r (lookupRan b r)

updateDom :: (Ord a, Ord b) => (a -> a) -> b -> Relation a b -> Relation a b
updateDom f b r =
  foldl' (\r a -> insert (f a) b $ delete a b r) r (lookupRan b r)

updateRan :: (Ord a, Ord b) => (b -> b) -> a -> Relation a b -> Relation a b
updateRan f a r =
  foldl' (\r b -> insert a (f b) $ delete a b r) r (lookupDom a r)

deleteRan :: (Ord a, Ord b) => b -> Relation a b -> Relation a b
deleteRan b r = foldl' (\r a -> delete a b r) r $ lookupRan b r

deleteDom :: (Ord a, Ord b) => a -> Relation a b -> Relation a b
deleteDom a r = foldl' (flip $ delete a) r $ lookupDom a r

deleteRanWhere :: (Ord a, Ord b) => (b -> Bool) -> a -> Relation a b -> Relation a b
deleteRanWhere f a r =
  foldl' (\r b -> if f b then delete a b r else r) r (lookupDom a r)

deleteDomWhere :: (Ord a, Ord b) => (a -> Bool) -> b -> Relation a b -> Relation a b
deleteDomWhere f b r =
  foldl' (\r a -> if f a then delete a b r else r) r (lookupRan b r)

map ::
  (Ord a, Ord b, Ord c, Ord d) =>
  ((a, b) -> (c, d)) ->
  Relation a b ->
  Relation c d
map f = fromList . fmap f . toList

-- aka first
mapDom :: (Ord a, Ord a', Ord b) => (a -> a') -> Relation a b -> Relation a' b
mapDom f = fromList . fmap (first f) . toList

-- aka second
mapRan :: (Ord a, Ord b, Ord b') => (b -> b') -> Relation a b -> Relation a b'
mapRan f = fromList . fmap (second f) . toList

fromMap :: (Ord a, Ord b) => Map a b -> Relation a b
fromMap = fromList . Map.toList

fromMultimap :: (Ord a, Ord b) => Map a (Set b) -> Relation a b
fromMultimap m =
  foldl' (\r (a, bs) -> insertManyRan a bs r) empty $ Map.toList m

toMultimap :: Relation a b -> Map a (Set b)
toMultimap = domain

-- Returns Nothing if Relation isn't one-to-one.
toMap :: Ord a => Relation a b -> Maybe (Map a b)
toMap r =
  let mm = toMultimap r
   in if all (\s -> S.size s == 1) mm
        then Just (S.findMin <$> mm)
        else Nothing

fromSet :: (Ord a, Ord b) => Set (a, b) -> Relation a b
fromSet = fromList . S.toList

fromManyRan ::
  (Foldable f, Ord a, Ord b) => a -> f b -> Relation a b
fromManyRan a bs = insertManyRan a bs mempty

fromManyDom ::
  (Foldable f, Ord a, Ord b) => f a -> b -> Relation a b
fromManyDom as b = insertManyDom as b mempty

swap :: Relation a b -> Relation b a
swap (Relation a b) = Relation b a

bimap ::
  (Ord a, Ord b, Ord c, Ord d) =>
  (a -> c) ->
  (b -> d) ->
  Relation a b ->
  Relation c d
bimap f g = fromList . fmap (\(a, b) -> (f a, g b)) . toList

instance (Ord a, Ord b) => Monoid (Relation a b) where
  mempty = empty
  mappend = (<>)

instance (Ord a, Ord b) => Semigroup (Relation a b) where
  (<>) = union

instance (H.Hashable a, H.Hashable b) => H.Hashable (Relation a b) where
  tokens = H.tokens . toList

toUnzippedMultimap ::
  Ord a => Ord b => Ord c => Relation a (b, c) -> Map a (Set b, Set c)
toUnzippedMultimap r = (\s -> (S.map fst s, S.map snd s)) <$> toMultimap r

collectRan ::
  Ord a =>
  Ord c =>
  (b -> Maybe c) ->
  Relation a b ->
  Relation a c
collectRan f r = fromList [(a, c) | (a, f -> Just c) <- toList r]
