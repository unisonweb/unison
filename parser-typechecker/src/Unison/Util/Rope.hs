{-# Language FunctionalDependencies #-}
{-# Language DeriveFoldable #-}

module Unison.Util.Rope 
  (chunks, singleton, one, map, traverse, null, flatten, two, cons, uncons, snoc, unsnoc, 
   Sized(..), Take(..), Drop(..), Reverse(..), Index(..), Rope,
   ) 
where

import Prelude hiding (drop,take,reverse,map,traverse,null)
import Data.Foldable (toList)
import Control.DeepSeq (NFData(..))

-- | Size-balanced binary tree of chunks. 
-- The `Int` in the `Two` constructor is a cached size of that subtree.
data Rope a
  = Empty
  | One {-# unpack #-} !a
  | Two {-# unpack #-} !Int !(Rope a) !(Rope a)
  deriving Foldable

chunks :: Rope a -> [a]
chunks = toList

singleton, one :: Sized a => a -> Rope a
one a | size a == 0 = Empty
one a = One a
singleton = one

map :: Sized b => (a -> b) -> Rope a -> Rope b
map f = \case
  Empty -> Empty
  One a -> one (f a)
  Two _ l r -> two (map f l) (map f r)

traverse :: (Applicative f, Sized b) => (a -> f b) -> Rope a -> f (Rope b)
traverse f = \case
  Empty -> pure Empty
  One a -> one <$> f a
  Two _ l r -> two <$> traverse f l <*> traverse f r

-- typeclasses used for abstracting over the chunk type 
class Sized a where size :: a -> Int
class Take a  where take :: Int -> a -> a
class Drop a  where drop :: Int -> a -> a
class Index a elem where index :: Int -> a -> Maybe elem
class Reverse a where reverse :: a -> a

instance Sized a => Sized (Rope a) where
  size = \case
    Empty -> 0
    One a -> size a
    Two n _ _ -> n

null :: Sized a => Rope a -> Bool
null r = size r == 0

flatten :: Monoid a => Rope a -> a
flatten = mconcat . toList

instance (Sized a, Semigroup a) => Semigroup (Rope a) where (<>) = mappend
instance (Sized a, Semigroup a) => Monoid (Rope a) where
  mempty = Empty
  mappend r1 r2 = case (r1,r2) of
    (Empty, k) -> k
    (k, Empty) -> k
    (One a0, k2) -> cons' (size a0) a0 k2
    (k1, One aN) -> snoc' k1 (size aN) aN
    (k1@(Two sz1 l1 r1), k2@(Two sz2 l2 r2))
      | sz1 * 2 >= sz2 && sz2 * 2 >= sz1 -> Two (sz1 + sz2) k1 k2
      | sz1 > sz2                        ->  l1 <> (r1 <> k2)
      | otherwise                        -> (k1 <> l2) <> r2

instance Reverse a => Reverse (Rope a) where
  reverse = \case
    One a -> One (reverse a)
    Two sz l r -> Two sz (reverse r) (reverse l)
    Empty -> Empty

two :: Sized a => Rope a -> Rope a -> Rope a
two r1 r2 = Two (size r1 + size r2) r1 r2

threshold :: Int
threshold = 32

cons :: (Sized a, Semigroup a) => a -> Rope a -> Rope a
cons a r =
  if size a == 0 then r
  else cons' (size a) a r

snoc :: (Sized a, Semigroup a) => Rope a -> a -> Rope a
snoc as a =
  if size a == 0 then as
  else snoc' as (size a) a

cons' :: (Sized a, Semigroup a) => Int -> a -> Rope a -> Rope a
cons' 0 _ as = as 
cons' sz0 a0 as = go a0 as
  where
  go a0 as = case as of
    Empty -> One a0
    One a1 -> case sz0 + size a1 of
      n | n <= threshold -> One (a0 <> a1)
        | otherwise      -> Two n (One a0) as
    Two sz One{} _ -> Two (sz0+sz) (One a0) as
    Two _ l r -> go a0 l <> r

snoc' :: (Sized a, Semigroup a) => Rope a -> Int -> a -> Rope a
snoc' as 0 _ = as 
snoc' as szN aN = go as aN 
  where
  go as aN = case as of
    Empty -> One aN
    One a0 -> case size a0 + szN of
      n | n <= threshold -> One (a0 <> aN)
        | otherwise      -> Two n as (One aN)
    Two sz One{} _ -> Two (sz+szN) as (One aN)
    Two _ l r -> l <> go r aN

instance (Sized a, Index a ch) => Index (Rope a) ch where
  index i = \case
    One a -> index i a
    Two sz l r
      | i < size l -> index i l
      | i >= sz    -> Nothing
      | otherwise  -> index (i - size l) r
    Empty -> Nothing

instance (Sized a, Semigroup a, Take a) => Take (Rope a) where
  -- this avoids rebalancing the tree, which is more efficient
  -- when walking a large rope from left to right via take/drop
  take n as = case as of
    One a -> if n <= 0 then Empty else one (take n a)
    Two sz l r
      | n < size l -> take n l
      | n >= sz    -> as
      | otherwise  -> two l (take (n - size l) r) -- don't rebalance
    Empty -> Empty

instance (Sized a, Semigroup a, Drop a) => Drop (Rope a) where
  -- this avoids rebalancing the tree, which is more efficient
  -- when walking a large rope from left to right via take/drop
  drop n as = case as of
    One a -> if n >= size a then Empty else one (drop n a)
    Two sz l r
      | n >= size l -> drop (n - size l) r
      | n >= sz     -> Empty
      | otherwise   -> two (drop n l) r -- don't rebalance
    Empty -> Empty

uncons :: Sized a => Rope a -> Maybe (a, Rope a)
uncons = \case
  Empty -> Nothing
  One a -> Just (a, Empty)
  Two _ l r -> case uncons l of
    Nothing -> uncons r
    Just (hd,tl) -> Just (hd, two tl r)

unsnoc :: Sized a => Rope a -> Maybe (Rope a, a)
unsnoc = \case
  Empty -> Nothing
  One a -> Just (Empty, a)
  Two _ l r -> case unsnoc r of
    Nothing -> unsnoc l
    Just (init,last) -> Just (two l init, last)

-- Produces two lists of chunks where the chunks have the same length
alignChunks :: (Sized a, Take a, Drop a) => [a] -> [a] -> ([a], [a])
alignChunks bs1 bs2 = (cs1, cs2)
  where
  cs1 = alignTo bs1 bs2
  cs2 = alignTo bs2 cs1
  alignTo bs1 [] = bs1
  alignTo []  _  = []
  alignTo (hd1:tl1) (hd2:tl2)
    | len1 == len2 = hd1 : alignTo tl1 tl2
    | len1 < len2  = hd1 : alignTo tl1 (drop len1 hd2 : tl2)
    | otherwise    = -- len1 > len2
                     let (hd1',hd1rem) = (take len2 hd1, drop len2 hd1)
                     in hd1' : alignTo (hd1rem : tl1) tl2
    where
      len1 = size hd1
      len2 = size hd2

instance (Sized a, Take a, Drop a, Eq a) => Eq (Rope a) where
  b1 == b2 | size b1 == size b2 =
    uncurry (==) (alignChunks (chunks b1) (chunks b2))
  _ == _ = False

-- Lexicographical ordering
instance (Sized a, Take a, Drop a, Ord a) => Ord (Rope a) where
  b1 `compare` b2 = uncurry compare (alignChunks (chunks b1) (chunks b2))

instance NFData a => NFData (Rope a) where 
  rnf Empty = ()
  rnf (One a) = rnf a
  rnf (Two _ l r) = rnf l `seq` rnf r
