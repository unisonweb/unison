{-# Language ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.Util.Bytes where

import Unison.Prelude hiding (empty)

import Data.Monoid (Sum(..))
-- import Prelude hiding (drop)
import System.IO.Unsafe (unsafeDupablePerformIO)
-- import Data.ByteArray.Methods (unsafeDoIO)
-- import Data.ByteArray.Types
import Data.Memory.PtrMethods (memCompare, memEqual)
-- import Data.Memory.Internal.Compat
import Foreign.Ptr (plusPtr)
import Prelude hiding (length, take, drop)
import qualified Data.FingerTree as T
import qualified Data.ByteArray as B

-- Bytes type represented as a finger tree of ByteStrings.
-- Can be efficiently sliced and indexed, using the byte count
-- annotation at each subtree.
newtype Bytes = Bytes (T.FingerTree (Sum Int) (View B.Bytes))

null :: Bytes -> Bool
null (Bytes bs) = T.null bs

empty :: Bytes
empty = Bytes mempty

fromArray :: B.ByteArrayAccess ba => ba -> Bytes
fromArray = snoc empty

toArray :: forall bo . B.ByteArray bo => Bytes -> bo
toArray b = B.concat (map B.convert (chunks b) :: [bo])

size :: Bytes -> Int
size (Bytes bs) = getSum (T.measure bs)

chunks :: Bytes -> [View B.Bytes]
chunks (Bytes b) = toList b

cons :: B.ByteArrayAccess ba => ba -> Bytes -> Bytes
cons b bs | B.null b = bs
cons b (Bytes bs) = Bytes (view (B.convert b) T.<| bs)

snoc :: B.ByteArrayAccess ba => Bytes -> ba -> Bytes
snoc bs b | B.null b = bs
snoc (Bytes bs) b = Bytes (bs T.|> view (B.convert b))

flatten :: Bytes -> Bytes
flatten b = snoc mempty (B.concat (chunks b) :: B.Bytes)

take :: Int -> Bytes -> Bytes
take n (Bytes bs) = go (T.split (> Sum n) bs) where
  go (ok, s) = Bytes $ case T.viewl s of
    last T.:< _ ->
      if T.measure ok == Sum n then ok
      else ok T.|> takeView (n - getSum (T.measure ok)) last
    _ -> ok

drop :: Int -> Bytes -> Bytes
drop n b0@(Bytes bs) = go (T.dropUntil (> Sum n) bs) where
  go s = Bytes $ case T.viewl s of
    head T.:< tail ->
      if (size b0 - getSum (T.measure s)) == n then s
      else dropView (n - (size b0 - getSum (T.measure s))) head T.<| tail
    _ -> s

at :: Int -> Bytes -> Maybe Word8
at i bs = case drop i bs of
  -- note: chunks guaranteed nonempty (see `snoc` and `cons` implementations)
  Bytes (T.viewl -> hd T.:< _) -> Just (B.index hd 0)
  _ -> Nothing

toWord8s :: Bytes -> [Word8]
toWord8s bs = chunks bs >>= B.unpack

fromWord8s :: [Word8] -> Bytes
fromWord8s bs = fromArray (view $ B.pack bs :: View B.Bytes)

instance Monoid Bytes where
  mempty = Bytes mempty
  mappend (Bytes b1) (Bytes b2) = Bytes (b1 `mappend` b2)

instance Semigroup Bytes where (<>) = mappend

instance T.Measured (Sum Int) (View B.Bytes) where
  measure b = Sum (B.length b)

instance Show Bytes where
  show bs = show (toWord8s bs)

-- Produces two lists where the chunks have the same length
alignChunks :: B.ByteArrayAccess ba => [View ba] -> [View ba] -> ([View ba], [View ba])
alignChunks bs1 bs2 = (cs1, cs2)
  where
  cs1 = alignTo bs1 bs2
  cs2 = alignTo bs2 cs1
  alignTo :: B.ByteArrayAccess ba => [View ba] -> [View ba] -> [View ba]
  alignTo bs1 [] = bs1
  alignTo []  _  = []
  alignTo (hd1:tl1) (hd2:tl2)
    | len1 == len2 = hd1 : alignTo tl1 tl2
    | len1 < len2  = hd1 : alignTo tl1 (dropView len1 hd2 : tl2)
    | otherwise    = -- len1 > len2
                     let (hd1',hd1rem) = (takeView len2 hd1, dropView len2 hd1)
                     in hd1' : alignTo (hd1rem : tl1) tl2
    where
      len1 = B.length hd1
      len2 = B.length hd2

instance Eq Bytes where
  b1 == b2 | size b1 == size b2 =
    uncurry (==) (alignChunks (chunks b1) (chunks b2))
  _ == _ = False

-- Lexicographical ordering
instance Ord Bytes where
  b1 `compare` b2 = uncurry compare (alignChunks (chunks b1) (chunks b2))

--
-- Forked from: http://hackage.haskell.org/package/memory-0.15.0/docs/src/Data.ByteArray.View.html
-- which is already one of our dependencies. Forked because the view
-- type in the memory package doesn't expose its constructor which makes
-- it impossible to implement take and drop.
--
-- Module      : Data.ByteArray.View
-- License     : BSD-style
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : stable
-- Portability : Good

view :: B.ByteArrayAccess bs => bs -> View bs
view bs = View 0 (B.length bs) bs

takeView, dropView :: B.ByteArrayAccess bs => Int -> View bs -> View bs
takeView k (View i n bs) = View i (min k n) bs
dropView k (View i n bs) = View (i + (k `min` n)) (n - (k `min` n)) bs

data View bytes = View
  { viewOffset :: !Int
  , viewSize   :: !Int
  , unView     :: !bytes
  }

instance B.ByteArrayAccess bytes => Eq (View bytes) where
  v1 == v2 = viewSize v1 == viewSize v2 && unsafeDupablePerformIO (
    B.withByteArray v1 $ \ptr1 ->
    B.withByteArray v2 $ \ptr2 -> memEqual ptr1 ptr2 (viewSize v1))

instance B.ByteArrayAccess bytes => Ord (View bytes) where
  compare v1 v2 = unsafeDupablePerformIO $
    B.withByteArray v1 $ \ptr1 ->
    B.withByteArray v2 $ \ptr2 -> do
      ret <- memCompare ptr1 ptr2 (min (viewSize v1) (viewSize v2))
      return $ case ret of
        EQ | B.length v1 >  B.length v2 -> GT
           | B.length v1 <  B.length v2 -> LT
           | B.length v1 == B.length v2 -> EQ
        _                               -> ret

instance B.ByteArrayAccess bytes => Show (View bytes) where
  show v = show (B.unpack v)

instance B.ByteArrayAccess bytes => B.ByteArrayAccess (View bytes) where
  length = viewSize
  withByteArray v f = B.withByteArray (unView v) $
    \ptr -> f (ptr `plusPtr` (viewOffset v))

