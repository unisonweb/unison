module Unison.Runtime.Vector where

import Data.List hiding (length)
import Prelude hiding (length)
import qualified Data.Vector as V

arity :: Int
arity = 128

data Vector a =
  Vector { length :: !Int, hd :: !(V.Vector a), tl :: (Vector (V.Vector a)), buf :: !(V.Vector a) }

empty :: Vector a
empty = Vector 0 V.empty empty V.empty

snoc :: Vector a -> a -> Vector a
snoc (Vector n hd tl buf) a =
  case buf `V.snoc` a of
    buf | V.length buf /= arity -> Vector (n+1) hd tl buf
    buf | n == arity-1 -> Vector (n+1) buf tl V.empty
    buf -> Vector (n+1) hd (tl `snoc` buf) V.empty

unsafeIndex :: Vector a -> Int -> a
unsafeIndex (Vector _ hd tl buf) i = case i of
  _ | i < V.length hd -> hd `V.unsafeIndex` i
  _ | i >= V.length hd + length tl * arity -> buf `V.unsafeIndex` (i - (length tl)*arity - V.length hd)
  _ -> case (i - V.length hd) `divMod` arity of
         (bucket,offset) -> tl `unsafeIndex` bucket `V.unsafeIndex` offset

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
