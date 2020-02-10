{-
Copyright (c) 2018, Koji Miyazato

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Koji Miyazato nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 
from https://github.com/viercc/trie-simple
-}


{-# LANGUAGE DeriveTraversable #-}
module Unison.Util.TMap.Hidden(
  -- * Types
  TMap(..),
  -- * Queries
  match,
  lookup,
  member, notMember,
  null, count,
  keys, elems,
  maxDepth,
  minUniquePrefix,
  -- * Construction
  empty, just,
  singleton,

  -- * Single item modification
  insertWith, insert,
  deleteWith, delete,

  adjust, revise, update, alter,

  -- * Combine
  union, unionWith,
  intersection, intersectionWith,
  difference, differenceWith,
  appendWith,

  -- * Conversion
  toList, fromList,
  toAscList, fromAscList,
  toMap, fromMap,
  keysTSet, fromTSet,

  -- * Parsing
  toParser, toParser_, toParser__,

  -- * Traversing with keys
  traverseWithKey, mapWithKey, foldMapWithKey, foldrWithKey,

  -- * Internals
  Node(..),
  foldTMap,
)
where

import           Prelude                hiding (lookup, null)

import           Data.Functor.Const
import           Data.Functor.Identity

import           Data.Semigroup

import           Control.Applicative    hiding (empty)
import qualified Control.Applicative    as Ap (empty)

import           Control.Monad

import qualified Data.Foldable          as F
import qualified Data.List              as List (foldl')
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (fromMaybe, isJust, isNothing)

import           Unison.Util.TSet.Internal (TSet (..))
import qualified Unison.Util.TSet.Internal as TSet

import           Control.DeepSeq

data Node c a r = Node !(Maybe a) !(Map c r)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (NFData c, NFData a, NFData r) => NFData (Node c a r) where
  rnf (Node a e) = rnf a `seq` rnf e

-- | Mapping from @[c]@ to @a@ implemented as a trie.
--   This type serves almost same purpose with @Map [c] a@,
--   but can be looked up more efficiently.
newtype TMap c a = TMap { getNode :: Node c a (TMap c a) }
  deriving (Eq, Ord)

instance (Show c, Show a) => Show (TMap c a) where
  showsPrec p t = showParen (p > 10) $ showString "fromList " . showsPrec 11 (toList t)

instance (NFData c, NFData a) => NFData (TMap c a) where
  rnf (TMap node) = rnf node

-- * Queries

-- | Perform matching against a @TMap@.
--
--   @match xs tmap@ returns two values. First value is the result of
--   'lookup'. Second value is another @TMap@, which holds mapping between
--   all pair of @ys@ and @b@, such that @tmap@ maps @(xs ++ ys)@ to @b@.
match :: (Ord c) => [c] -> TMap c a -> (Maybe a, TMap c a)
match []     t@(TMap (Node ma _)) = (ma, t)
match (c:cs)   (TMap (Node _  e)) =
  case Map.lookup c e of
    Nothing -> (Nothing, empty)
    Just t' -> match cs t'

-- | @lookup xs tmap@ returns @Just a@ if @tmap@ contains mapping
--   from @xs@ to @a@, and returns @Nothing@ if not.
lookup :: (Ord c) => [c] -> TMap c a -> Maybe a
lookup cs = fst . match cs

member, notMember :: (Ord c) => [c] -> TMap c a -> Bool
member cs = isJust . lookup cs
notMember cs = isNothing . lookup cs

-- | Tests if given map is empty.
null :: TMap c a -> Bool
null (TMap (Node ma e)) = isNothing ma && Map.null e
{- Ensure all @TMap@ values exposed to users have no
   redundant node. -}

-- | Returns number of entries.
--
--   Note that this operation takes O(number of nodes),
--   unlike O(1) of 'Map.size'.
count :: TMap c a -> Int
count = F.length

-- | Returns list of key strings, in ascending order.
keys :: TMap c a -> [[c]]
keys = foldTMap keys'
  where
    keys' (Node ma e) =
      [ [] | isJust ma ] ++
      [ c:cs' | (c,css') <- Map.toList e, cs' <- css' ]

-- | Returns list of values, in ascending order by its key.
elems :: TMap c a -> [a]
elems = F.toList

-- * Construction

-- | Empty @TMap@.
empty :: TMap c a
empty = TMap (Node Nothing Map.empty)

-- | @TMap@ which contains only one entry from the empty string to @a@.
just :: a -> TMap c a
just a = TMap (Node (Just a) Map.empty)

-- | @singleton xs a@ is a @TMap@ which contains only one entry
--   from @xs@ to @a@.
singleton :: [c] -> a -> TMap c a
singleton cs a0 = foldr cons (just a0) cs

cons :: c -> TMap c a -> TMap c a
cons c t = TMap (Node Nothing (Map.singleton c t))

-- * Single-item modification

-- | Inserts an entry of key and value pair.
--
--   Already existing value will be overwritten, i.e.
--   > insert = insertWith (const a)
insert :: (Ord c) => [c] -> a -> TMap c a -> TMap c a
insert cs a = revise (const a) cs

-- | Deletes an entry with given key.
--
--   > delete = update (const Nothing)
delete :: (Ord c) => [c] -> TMap c a -> TMap c a
delete = update (const Nothing)

-- | @insertWith op xs a tmap@ inserts an key (@xs@) and value (@a@) pair
--   to the @tmap@. If @tmap@ already has an entry with key equals to
--   @xs@, its value @b@ is replaced with @op a b@.
--
--   > insertWith op cs a = revise (maybe a (op a)) cs
insertWith :: (Ord c) => (a -> a -> a) -> [c] -> a -> TMap c a -> TMap c a
insertWith f cs a = revise (maybe a (f a)) cs

-- | Deletes an entry with given key, conditionally.
--
--   @deleteWith f xs b@ looks up an entry with key @xs@, and if such entry
--   is found, evaluate @f b a@ with its value @a@. If it returned @Nothing@,
--   the entry is deleted. Otherwise, if it returned @Just a'@, the value of
--   the entry is replaced with @a'@.
--
--   > deleteWith f cs b = update (f b) cs
deleteWith :: (Ord c) => (b -> a -> Maybe a) -> [c] -> b -> TMap c a -> TMap c a
deleteWith f cs b = update (f b) cs

-- | Apply a function to the entry with given key.
adjust :: (Ord c) => (a -> a) -> [c] -> TMap c a -> TMap c a
adjust f = F.foldr step base
  where
    base (TMap (Node ma e)) = TMap (Node (f <$> ma) e)
    step x xs (TMap (Node ma e)) =
      let e' = Map.adjust xs x e
      in TMap (Node ma e')
{-# INLINE adjust #-}

-- | Apply a function @f@ to the entry with given key. If there is no such
--   entry, insert an entry with value @f Nothing@.
revise :: (Ord c) => (Maybe a -> a) -> [c] -> TMap c a -> TMap c a
revise f = fst . F.foldr step (base, just (f Nothing))
  where
    base (TMap (Node ma e)) = TMap (Node (Just (f ma)) e)
    step x (inserter', xs') =
      let inserter (TMap (Node ma e)) =
            let e' = Map.insertWith (const inserter') x xs' e
            in TMap (Node ma e')
      in (inserter, cons x xs')
{-# INLINE revise #-}

-- | Apply a function @f@ to the entry with given key. If @f@ returns
--   @Nothing@, that entry is deleted.
update :: (Ord c) => (a -> Maybe a) -> [c] -> TMap c a -> TMap c a
update f cs = fromMaybe empty . update_ f cs
{-# INLINE update #-}

update_ :: (Ord c) => (a -> Maybe a) -> [c] -> TMap c a -> Maybe (TMap c a)
update_ f = F.foldr step base
  where
    base (TMap (Node ma e)) =
      let ma' = ma >>= f
      in if isNothing ma' && Map.null e
           then Nothing
           else Just $ TMap (Node ma' e)
    step x xs (TMap (Node ma e)) =
      let e' = Map.update xs x e
      in if isNothing ma && Map.null e'
           then Nothing
           else Just $ TMap (Node ma e')
{-# INLINE update_ #-}

-- | Apply a function @f@ to the entry with given key. This function @alter@
--   is the most generic version of 'adjust', 'revise', 'update'.
-- 
--   * You can insert new entry by returning @Just a@ from @f Nothing@.
--   * You can delete existing entry by returning @Nothing@ from
--     @f (Just a)@.
--
--   This function always evaluates @f Nothing@ in addition to determine
--   operation applied to given key.
--   If you never use `alter` on a missing key, consider using 'update' instead.
alter :: (Ord c) => (Maybe a -> Maybe a) -> [c] -> TMap c a -> TMap c a
alter f =
  case f Nothing of
    Nothing -> update (f . Just)
    Just f0 -> \cs -> fromMaybe empty . alter_ f f0 cs
{-# INLINE alter #-}

alter_ :: (Ord c) => (Maybe a -> Maybe a) -> a -> [c] -> TMap c a -> Maybe (TMap c a)
alter_ f f0 = fst . F.foldr step (base, just f0)
  where
    base (TMap (Node ma e)) =
      let ma' = f ma
      in if isNothing ma' && Map.null e
           then Nothing
           else Just $ TMap (Node ma' e)
    step x (alterer', xs') =
      let alterer (TMap (Node ma e)) =
            let e' = Map.alter (maybe (Just xs') alterer') x e
            in if isNothing ma && Map.null e'
                 then Nothing
                 else Just $ TMap (Node ma e')
      in (alterer, cons x xs')
{-# INLINE alter_ #-}

-- * Combine
union :: (Ord c) => TMap c a -> TMap c a -> TMap c a
union = unionWith const

unionWith :: (Ord c) => (a -> a -> a) -> TMap c a -> TMap c a -> TMap c a
unionWith f = go
  where
    go (TMap (Node mat et)) (TMap (Node mau eu)) =
      let maz = case (mat, mau) of
            (Nothing, Nothing) -> Nothing
            (Just at, Nothing) -> Just at
            (Nothing, Just au) -> Just au
            (Just at, Just au) -> Just (f at au)
          ez = Map.unionWith go et eu
      in TMap (Node maz ez)

intersection :: (Ord c) => TMap c a -> TMap c b -> TMap c a
intersection = intersectionWith (\a _ -> Just a)

intersectionWith :: (Ord c) =>
  (a -> b -> Maybe r) -> TMap c a -> TMap c b -> TMap c r
intersectionWith f x y = fromMaybe empty $ go x y
  where
    go (TMap (Node ma ex)) (TMap (Node mb ey)) =
      if isNothing mr && Map.null ez
        then Nothing
        else Just $ TMap (Node mr ez)
      where
        mr = do a <- ma
                b <- mb
                f a b
        emz = Map.intersectionWith go ex ey
        ez = Map.mapMaybe id emz

difference :: (Ord c) => TMap c a -> TMap c b -> TMap c a
difference = differenceWith (\_ _ -> Nothing)

differenceWith :: (Ord c) =>
  (a -> b -> Maybe a) -> TMap c a -> TMap c b -> TMap c a
differenceWith f x y = fromMaybe empty $ go x y
  where
    go (TMap (Node ma ex)) (TMap (Node mb ey)) =
      if isNothing mr && Map.null ez
        then Nothing
        else Just $ TMap (Node mr ez)
      where
        mr = case (ma, mb) of
          (Nothing, _)       -> Nothing
          (Just a,  Nothing) -> Just a
          (Just a,  Just b)  -> f a b
        ez = Map.differenceWith go ex ey

{- |
Make new @TMap@ from two @TMap@s. Constructed @TMap@
has keys which are concatenation of any combination from
two input maps.
Corresponding values for these keys are combined with given function
of type @(x -> y -> z)@. If two different concatenations yield
a same key, corresponding values for these keys are combined with
a 'Semigroup' operation @<>@.
There is no guarantees on which order duplicate values are combined with @<>@.
So it must be commutative semigroup to get a stable result.
===== Example
> let x = fromList [("a", 1), ("aa", 2)]     :: TMap Char (Sum Int)
>     y = fromList [("aa", 10), ("aaa", 20)] :: TMap Char (Sum Int)
>
> appendWith (*) x y =
>   fromList [ ("aaa", 1 * 10)
>            , ("aaaa", 1 * 20 + 2 * 10)
>            , ("aaaaa", 2 * 20) ]
-}
appendWith :: (Ord c, Semigroup z) => (x -> y -> z) ->
  TMap c x -> TMap c y -> TMap c z
appendWith f x y =
  if null y
    then empty
    else go x
  where
    go (TMap (Node Nothing e)) =
      let e' = Map.map go e
      in TMap (Node Nothing e')
    go (TMap (Node (Just ax) e)) =
      let TMap (Node maz e') = fmap (f ax) y
          e'' = Map.map go e
          e''' = Map.unionWith (unionWith (<>)) e' e''
      in TMap (Node maz e''')

-- * Instances

instance Functor (TMap c) where
  fmap f = go
    where
      go (TMap (Node ma e)) = TMap (Node (fmap f ma) (fmap go e))

instance Foldable (TMap c) where
  foldMap f = go
    where
      go (TMap (Node ma e)) = case ma of
        Nothing -> foldMap go e
        Just a  -> f a `mappend` foldMap go e

instance Traversable (TMap c) where
  traverse f = go
    where
      go (TMap (Node a e)) = TMap <$> (Node <$> traverse f a <*> traverse go e)

-- | 'unionWith'-based
instance (Ord c, Semigroup a) => Semigroup (TMap c a) where
  (<>) = unionWith (<>)
  stimes n = fmap (stimes n)

-- | 'unionWith'-based
instance (Ord c, Semigroup a) => Monoid (TMap c a) where
  mempty = empty
  mappend = (<>)

-- * Conversion

toList :: TMap c a -> [([c], a)]
toList = foldrWithKey (\k a r -> (k,a) : r) []

fromList :: Ord c => [([c], a)] -> TMap c a
fromList = List.foldl' (flip (uncurry insert)) empty

toAscList :: TMap c a -> [([c], a)]
toAscList = toList

fromAscList :: Eq c => [([c], a)] -> TMap c a
fromAscList [] = empty
fromAscList [(cs, a)] = singleton cs a
fromAscList pairs =
  let (ma, gs) = group_ pairs
      e = Map.fromDistinctAscList $ map (fmap fromAscList) gs
  in TMap (Node ma e)

group_ :: Eq c => [([c], a)] -> (Maybe a, [ (c, [ ([c], a) ]) ] )
group_ = foldr step (Nothing, [])
  where
    step ([], a) (ma, gs) = (ma <|> Just a, gs)
    step (c:cs, a) (ma, gs) = case gs of
      (d,ps'):rest | c == d  -> (ma, (d, (cs,a):ps'):rest)
      _            -> (ma, (c, [(cs,a)]):gs)

toMap :: TMap c a -> Map [c] a
toMap = Map.fromDistinctAscList . toAscList

fromMap :: (Eq c) => Map [c] a -> TMap c a
fromMap = fromAscList . Map.toAscList

keysTSet :: TMap c a -> TSet c
keysTSet = foldTMap keysTSet'
  where
    keysTSet' (Node ma e) =
      TSet (TSet.Node (isJust ma) e)

fromTSet :: ([c] -> a) -> TSet c -> TMap c a
fromTSet f = go []
  where
    go q (TSet (TSet.Node a e)) =
      let e' = Map.mapWithKey (\c -> go (c:q)) e
          a' = if a then Just (f (reverse q)) else Nothing
      in TMap (Node a' e')

-- * Parsing

toParser :: Alternative f =>
     (c -> f c') -- ^ char
  -> f eot       -- ^ eot
  -> TMap c a -> f ([c'], a)
toParser f eot = foldTMap toParser'
  where
    toParser' (Node ma e) =
      maybe Ap.empty (\a -> ([], a) <$ eot) ma <|>
      F.asum [ consFst <$> f c <*> p' | (c, p') <- Map.toAscList e ]

    consFst c (cs, a) = (c:cs, a)

toParser_ :: Alternative f =>
     (c -> f c') -- ^ char
  -> f eot       -- ^ eot
  -> TMap c a -> f a
toParser_ f eot = foldTMap toParser'
  where
    toParser' (Node ma e) =
      maybe Ap.empty (<$ eot) ma <|>
      F.asum [ f c *> p' | (c, p') <- Map.toAscList e ]

toParser__ :: Alternative f =>
     (c -> f c') -- ^ char
  -> f eot       -- ^ eot
  -> TMap c a -> f ()
toParser__ f eot = void . toParser_ f eot

-- * Traversing with keys

-- | Same semantics to following defintion, but have
--   more efficient implementation.
--
-- > traverseWithKey f = fmap fromAscList .
-- >                     traverse (\(cs,a) -> (,) cs <$> f cs a) .
-- >                     toAscList
traverseWithKey :: (Applicative f) =>
  ([c] -> a -> f b) -> TMap c a -> f (TMap c b)
traverseWithKey f = go []
  where
    go q (TMap (Node ma e)) =
      let step c = go (c : q)
          e' = Map.traverseWithKey step e
          mb = maybe (pure Nothing)
                     (\a -> Just <$> f (reverse q) a)
                     ma
      in TMap <$> (Node <$> mb <*> e')

-- | Same semantics to following defintion, but have
--   more efficient implementation.
--
-- > traverseWithKey f = fromAscList .
-- >                     map (\(cs,a) -> (cs,  f cs a)) .
-- >                     toAscList
mapWithKey :: ([c] -> a -> b) -> TMap c a -> TMap c b
mapWithKey f = runIdentity . traverseWithKey (\k a -> Identity (f k a))

-- | Same semantics to following defintion, but have
--   more efficient implementation.
--
-- > foldMapWithKey f = foldMap (uncurry f) . toAscList
foldMapWithKey :: (Monoid r) => ([c] -> a -> r) -> TMap c a -> r
foldMapWithKey f = getConst . traverseWithKey (\k a -> Const (f k a))

-- | Same semantics to following defintion, but have
--   more efficient implementation.
--
-- > foldrWithKey f z = foldr (uncurry f) z . toAscList
foldrWithKey :: ([c] -> a -> r -> r) -> r -> TMap c a -> r
foldrWithKey f z (TMap (Node ma e)) =
  case ma of
    Nothing -> r
    Just a  -> f [] a r
  where
    r = Map.foldrWithKey (\c subTrie s ->
          foldrWithKey (f . (c:)) s subTrie) z e

-- * Other operations

foldTMap :: (Node c a r -> r) -> TMap c a -> r
foldTMap f = go
  where go (TMap node) = f (fmap go node)

maxDepth :: TMap c a -> Int
maxDepth (TMap (Node _ m)) = case Map.elems m of 
  []    -> 0
  elems -> 1 + (maximum $ fmap maxDepth elems)

minUniquePrefix :: TMap c a -> Int
minUniquePrefix = plumb 0 0
  where
    plumb lastBranch currentDepth (TMap (Node _ m)) = case Map.elems m of 
      []     -> lastBranch
      [elem] -> plumb lastBranch (currentDepth + 1) elem 
      elems  -> maximum $ fmap (plumb currentDepth (currentDepth + 1)) elems