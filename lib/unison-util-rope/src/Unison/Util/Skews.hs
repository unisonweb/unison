{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}

-- | A double-ended sequence made of two skew binary random-access lists
-- back to back.
--
-- The front list holds the first elements in sequence order.  The back list
-- holds the last elements in reverse order.  'cons' conses onto the front,
-- 'snoc' conses onto the back, and 'uncons' / 'unsnoc' pop from the matching
-- list.  When the list an 'uncons' (or 'unsnoc') needs is empty, half of the
-- other list (the half nearest the wanted end) is split off, reversed, and
-- becomes the new, non-empty list.
--
-- * worst-case O(1) 'cons', 'snoc', 'size', and 'uncons' / 'unsnoc' while the
--   wanted side is non-empty
-- * 'uncons' / 'unsnoc' on an empty side costs O(n) for that one operation;
--   because half the elements move, any sequence of m operations costs
--   O(m + n) in total (amortised O(1), for ephemeral use)
-- * O(log n) 'index'
-- * 'drop' i is O(log n) when the cut falls in the front list (always, for
--   a deque built with 'cons'), and O(n - i) when it falls in the back list.
--   'take' i is O(log n) when the cut falls in the back list and O(i)
--   when it falls in the front list.
-- * O(min(m, n)) 'append' of sizes m and n (the smaller is merged into the
--   bigger, element by element); O(n) 'fromList', 'toList'
module Unison.Util.Skews
  ( Skews,
    empty,
    size,
    cons,
    snoc,
    uncons,
    unsnoc,
    index,
    append,
    take,
    drop,
    fromList,
    toList,
  )
where

import Data.Foldable qualified as F
import Data.List qualified as L
import Prelude hiding (drop, take)

------------------------------------------------------------------------
-- Skew binary random-access list (strict): cons / uncons O(1), index O(log n).

data Tree a = Leaf !a | Node !a !(Tree a) !(Tree a) -- preorder: root, left, right

-- trees in increasing weight (2^k - 1); only the first two may be equal
data Skew a = Nil | Cons !Int !(Tree a) !(Skew a) -- weight, tree, rest

skCons :: a -> Skew a -> Skew a
skCons x (Cons w1 t1 (Cons w2 t2 r))
  | w1 == w2 = Cons (1 + w1 + w2) (Node x t1 t2) r
skCons x s = Cons 1 (Leaf x) s

-- the head and the rest; the list must be non-empty
skUncons :: Skew a -> (a, Skew a)
skUncons Nil = error "Skews: internal error (empty skew list)"
skUncons (Cons _ (Leaf x) r) = (x, r)
skUncons (Cons w (Node x t1 t2) r) = let w' = w `div` 2 in (x, Cons w' t1 (Cons w' t2 r))

-- element i (0-based), which must be in range
skIndex :: Int -> Skew a -> a
skIndex _ Nil = error "Skews: internal error (index out of range)"
skIndex i (Cons w t r)
  | i < w = go w i t
  | otherwise = skIndex (i - w) r
  where
    go _ 0 (Leaf x) = x
    go _ 0 (Node x _ _) = x
    go w' j (Node _ a b) =
      let h = w' `div` 2
       in if j <= h then go h (j - 1) a else go h (j - 1 - h) b
    go _ _ (Leaf _) = error "Skews: internal error (bad tree)"

-- Folds.  The order of a skew list is preorder: root, left subtree, right
-- subtree, then the next tree.  The front list is in sequence order; the back
-- list is in reverse sequence order.

-- lazy right fold, in list order
skFoldr :: (a -> r -> r) -> r -> Skew a -> r
skFoldr _ z Nil = z
skFoldr f z (Cons _ t r) = tree t (skFoldr f z r)
  where
    tree (Leaf x) rest = f x rest
    tree (Node x l q) rest = f x (tree l (tree q rest))

-- left fold, in list order
skFoldl :: (r -> a -> r) -> r -> Skew a -> r
skFoldl _ z Nil = z
skFoldl f z (Cons _ t r) = skFoldl f (tree z t) r
  where
    tree acc (Leaf x) = f acc x
    tree acc (Node x l q) = tree (tree (f acc x) l) q

-- strict left fold, in list order
skFoldl' :: (r -> a -> r) -> r -> Skew a -> r
skFoldl' _ !z Nil = z
skFoldl' f !z (Cons _ t r) = skFoldl' f (tree z t) r
  where
    tree !acc (Leaf x) = f acc x
    tree !acc (Node x l q) =
      let !a1 = f acc x
          !a2 = tree a1 l
       in tree a2 q

-- strict left fold, in reverse list order (last element first)
skRevFoldl' :: (r -> a -> r) -> r -> Skew a -> r
skRevFoldl' _ !z Nil = z
skRevFoldl' f !z (Cons _ t r) = let !z1 = skRevFoldl' f z r in tree z1 t
  where
    tree !acc (Leaf x) = f acc x
    tree !acc (Node x l q) =
      let !a1 = tree acc q
          !a2 = tree a1 l
       in f a2 x

-- lazy right fold, in reverse list order
skRevFoldr :: (a -> r -> r) -> r -> Skew a -> r
skRevFoldr f = skFoldl (flip f)

-- Drop the first i elements (list order), 0 <= i <= length.  O(log n): skip
-- whole trees, then walk down one tree; every subtree passed over on the
-- way becomes a tree of the result.
skDrop :: Int -> Skew a -> Skew a
skDrop 0 s = s
skDrop _ Nil = Nil
skDrop i (Cons w t r)
  | i >= w = skDrop (i - w) r
  | otherwise = down i w t r
  where
    down 0 w' t' r' = Cons w' t' r'
    down j w' (Node _ t1 t2) r' =
      let h = w' `div` 2
       in if j <= h then down (j - 1) h t1 (Cons h t2 r') else down (j - 1 - h) h t2 r'
    down _ _ (Leaf _) _ = error "Skews: internal error (drop)"

-- Keep the first k elements (list order), 0 <= k <= length.  O(k): the
-- elements are consed back on from the k-th down to the first.
skTake :: Int -> Skew a -> Skew a
skTake k s = prefixRevFoldl' k (flip skCons) Nil s

-- strict left fold over the first k elements, taken last to first
prefixRevFoldl' :: Int -> (r -> a -> r) -> r -> Skew a -> r
prefixRevFoldl' k0 f z0 s0 = go k0 s0 []
  where
    -- collect the whole trees that fit (last one first), then the partial tree
    go k (Cons w t r) acc
      | k >= w = go (k - w) r ((w, t) : acc)
      | k > 0 = wholes (part k w t z0) acc
    go _ _ acc = wholes z0 acc
    wholes !z [] = z
    wholes !z ((_, t) : ts) = wholes (full z t) ts
    full !acc (Leaf x) = f acc x
    full !acc (Node x l q) =
      let !a1 = full acc q
          !a2 = full a1 l
       in f a2 x
    -- the first r elements (0 < r <= w) of a tree of weight w, last to first
    part r w t acc | r >= w = full acc t
    part r w (Node x l q) acc
      | r == 1 = f acc x
      | r - 1 <= h = let !a1 = part (r - 1) h l acc in f a1 x
      | otherwise =
          let !a1 = part (r - 1 - h) h q acc
              !a2 = full a1 l
           in f a2 x
      where
        h = w `div` 2
    part _ _ (Leaf _) _ = error "Skews: internal error (take)"

-- The same elements in the opposite order.  O(n).
skRev :: Skew a -> Skew a
skRev = skFoldl' (flip skCons) Nil

------------------------------------------------------------------------
-- The deque

-- | Front count, front list (in order), back count, back list (reversed).
data Skews a = Skews !Int !(Skew a) !Int !(Skew a)

empty :: Skews a
empty = Skews 0 Nil 0 Nil

-- | O(1).
size :: Skews a -> Int
size (Skews nf _ nb _) = nf + nb

-- | O(1).
cons :: a -> Skews a -> Skews a
cons x (Skews nf f nb b) = Skews (nf + 1) (skCons x f) nb b

-- | O(1).
snoc :: Skews a -> a -> Skews a
snoc (Skews nf f nb b) x = Skews nf f (nb + 1) (skCons x b)

-- | O(1) while the front is non-empty; otherwise moves the half of the back
-- list that is nearest the front.
uncons :: Skews a -> Maybe (a, Skews a)
uncons (Skews nf f nb b)
  | nf > 0 = case skUncons f of (x, f') -> Just (x, Skews (nf - 1) f' nb b)
  | nb == 0 = Nothing
  | otherwise =
      let h = (nb + 1) `div` 2
       in case skUncons (skRev (skDrop (nb - h) b)) of
            (x, f') -> Just (x, Skews (h - 1) f' (nb - h) (skTake (nb - h) b))

-- | O(1) while the back is non-empty; otherwise moves the half of the front
-- list that is nearest the back.
unsnoc :: Skews a -> Maybe (Skews a, a)
unsnoc (Skews nf f nb b)
  | nb > 0 = case skUncons b of (x, b') -> Just (Skews nf f (nb - 1) b', x)
  | nf == 0 = Nothing
  | otherwise =
      let h = (nf + 1) `div` 2
       in case skUncons (skRev (skDrop (nf - h) f)) of
            (x, b') -> Just (Skews (nf - h) (skTake (nf - h) f) (h - 1) b', x)

-- | O(log n).
index :: Int -> Skews a -> Maybe a
index i (Skews nf f nb b)
  | i < 0 || i >= nf + nb = Nothing
  | i < nf = Just (skIndex i f)
  | otherwise = Just (skIndex (nb - 1 - (i - nf)) b)

-- | O(n), lazy.
toList :: Skews a -> [a]
toList = F.foldr (:) []

-- | O(n).
fromList :: [a] -> Skews a
fromList = L.foldl' snoc empty

-- | Merges the smaller into the bigger, one element at a time: O(min(m, n)).
append :: Skews a -> Skews a -> Skews a
append a b
  | size a >= size b = L.foldl' snoc a (toList b)
  | otherwise = L.foldl' (flip cons) b (reverse (toList a))

-- | The first i elements.  O(log n) if they reach into the back list;
-- O(i) if they lie within the front list.
take :: Int -> Skews a -> Skews a
take i d@(Skews nf f nb b)
  | i <= 0 = empty
  | i >= nf + nb = d
  | i >= nf = Skews nf f (i - nf) (skDrop (nb - (i - nf)) b)
  | otherwise = Skews i (skTake i f) 0 Nil

-- | All but the first i elements.  O(log n) if the cut lies within the front
-- list; O(n - i) if it falls in the back list.
drop :: Int -> Skews a -> Skews a
drop i d@(Skews nf f nb b)
  | i <= 0 = d
  | i >= nf + nb = empty
  | i <= nf = Skews (nf - i) (skDrop i f) nb b
  | otherwise = let k = nf + nb - i in Skews 0 Nil k (skTake k b)

------------------------------------------------------------------------
-- Instances

instance Semigroup (Skews a) where
  (<>) = append

instance Monoid (Skews a) where
  mempty = empty

instance Foldable Skews where
  foldr f z (Skews _ fr _ b) = skFoldr f (skRevFoldr f z b) fr
  foldl f z (Skews _ fr _ b) = skFoldr (flip f) (skFoldl f z fr) b
  foldl' f z (Skews _ fr _ b) = skRevFoldl' f (skFoldl' f z fr) b
  foldMap f = F.foldr (\x r -> f x <> r) mempty
  toList = toList
  length = size
  null d = size d == 0
