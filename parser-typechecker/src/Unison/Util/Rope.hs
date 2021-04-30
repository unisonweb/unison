{-# Language FunctionalDependencies #-}
{-# Language DeriveFoldable #-}

module Unison.Util.Rope where

import Prelude hiding (drop,take,reverse,map)

data Rope a
  = Empty
  | One {-# unpack #-} !a
  | Two {-# unpack #-} !Int !(Rope a) !(Rope a)
  deriving Foldable

map :: Sized b => (a -> b) -> Rope a -> Rope b
map f = \case
  Empty -> Empty
  One a -> One (f a)
  Two _ l r -> two (map f l) (map f r)

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

instance (Semigroup a, Sized a) => Semigroup (Rope a) where (<>) = mappend
instance (Semigroup a, Sized a) => Monoid (Rope a) where
  mempty = Empty
  mappend r1 r2 = case (r1,r2) of
    (Empty, k) -> k
    (k, Empty) -> k
    (One a0, k2) -> cons a0 k2
    (k1, One aN) -> snoc k1 aN
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
threshold = 256

cons :: (Semigroup a, Sized a) => a -> Rope a -> Rope a
cons a0 as = case as of
  Empty -> One a0
  One a1 ->
    case size a0 + size a1 of
      n | n <= threshold -> One (a0 <> a1)
        | otherwise      -> Two n (One a0) (One a1)
  Two _ l r -> cons a0 l <> r

snoc :: (Semigroup a, Sized a) => Rope a -> a -> Rope a
snoc as aN = case as of
  Empty -> One aN
  One a0 -> case size a0 + size aN of
    n | n <= threshold -> One (a0 <> aN)
      | otherwise      -> Two n (One a0) (One aN)
  Two _ l r -> l <> snoc r aN

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
    One a -> if n <= 0 then Empty else One (take n a)
    Two sz l r
      | n < size l -> take n l
      | n >= sz    -> as
      | otherwise  -> two l (take (n - size l) r) -- don't rebalance
    Empty -> Empty

instance (Sized a, Semigroup a, Drop a) => Drop (Rope a) where
  -- this avoids rebalancing the tree, which is more efficient
  -- when walking a large rope from left to right via take/drop
  drop n as = case as of
    One a -> if n >= size a then Empty else One (drop n a)
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
