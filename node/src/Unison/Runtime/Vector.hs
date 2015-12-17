module Unison.Runtime.Vector where

-- |
-- Fast sequence type based on skewed array-indexed tries
-- with large branching factor, `B`.
--
-- Asymptotics (assuming vector of size `N`):
--   O(1) worst-case access to indices [0,B)
--   O(1) average-case access to indices [N-B,N)
--   O(log_B(i)) worst-case access to index i
--   O(1) amortized snoc, O(log_B(N)) worst case
--
-- Some inspiration stolen from: http://julesjacobs.github.io/2014/11/11/immutable-vectors-csharp.html

import Data.List hiding (init,length)
import Prelude hiding (init,length)
import qualified Data.Vector as V

arity :: Int
arity = 64

data Vector a =
  Vector { length :: !Int, hd :: !(V.Vector a), tl :: (Vector (V.Vector a)), buf :: !(V.Vector a) }

empty :: Vector a
empty = Vector 0 V.empty empty V.empty

isEmpty :: Vector a -> Bool
isEmpty v = length v == 0

snoc :: Vector a -> a -> Vector a
snoc (Vector n hd tl buf) a = case buf `V.snoc` a of
  buf | V.length buf /= arity -> Vector (n+1) hd tl buf
      | n == arity-1          -> Vector (n+1) buf tl V.empty
      | otherwise             -> Vector (n+1) hd (tl `snoc` buf) V.empty

unsnoc :: Vector a -> Maybe (Vector a, a)
unsnoc v | isEmpty v = Nothing
unsnoc v = Just (init v, unsafeLast v)

unsafeIndex :: Vector a -> Int -> a
unsafeIndex (Vector _ hd tl buf) i = case i of
  _ | i < V.length hd -> hd `V.unsafeIndex` i
  _ | i >= V.length hd + length tl * arity -> buf `V.unsafeIndex` (i - (length tl)*arity - V.length hd)
  _ -> case (i - V.length hd) `divMod` arity of
         (bucket,offset) -> tl `unsafeIndex` bucket `V.unsafeIndex` offset

unsafeLast :: Vector a -> a
unsafeLast v = unsafeIndex v (length v - 1)

last :: Vector a -> Maybe a
last v | isEmpty v = Nothing
last v = Just $ unsafeLast v

modifyLast :: (a -> a) -> Vector a -> Vector a
modifyLast f v | isEmpty v = v
               | otherwise = init v `snoc` f (unsafeLast v)

-- | Drop the last element from this vector. Returns itself if empty.
init :: Vector a -> Vector a
init v@(Vector n hd tl buf) = case V.null buf of
  False -> Vector (n-1) hd tl (V.init buf)
  _ | n == V.length hd -> Vector (n-1) V.empty tl (V.init hd)
  _ | n == 0           -> v
  _                    -> Vector (n-1) hd (init tl) (V.init (unsafeLast tl))

dropRightWhile :: (a -> Bool) -> Vector a -> Vector a
dropRightWhile f v | isEmpty v || not (f (unsafeLast v)) = v
dropRightWhile f v = dropRightWhile f (init v)

toList :: Vector a -> [a]
toList v = map (unsafeIndex v) [0 .. length v - 1]

fromList :: [a] -> Vector a
fromList = foldl' snoc empty

instance Show a => Show (Vector a) where
  show v = show (toList v)

instance Eq a => Eq (Vector a) where
  v1 == v2 = toList v1 == toList v2

instance Ord a => Ord (Vector a) where
  v1 `compare` v2 = toList v1 `compare` toList v2

instance Monoid (Vector a) where
  mempty = empty
  mappend (Vector 0 _ _ _) v2 = v2
  mappend v1@(Vector n1 hd1 tl1 buf1) v2@(Vector n2 hd2 tl2 buf2) =
    if V.null buf1 then Vector (n1+n2) hd1 (tl1 `snoc` hd2 `mappend` tl2) buf2
    else foldl' snoc v1 (toList v2)

instance Functor Vector where
  fmap f (Vector n hd tl buf) = Vector n (fmap f hd) (fmap (fmap f) tl) (fmap f buf)

instance Foldable Vector where
  foldMap f = foldl' (\acc a -> acc `mappend` f a) mempty
  foldl' f z v = foldl' f z (toList v)

instance Traversable Vector where
  traverse f v = fromList <$> traverse f (toList v)
