{-# Language ViewPatterns #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.Util.Bytes where

import Data.Char
import Data.Memory.PtrMethods (memCompare, memEqual)
import Data.Monoid (Sum(..))
import Foreign.Ptr (plusPtr)
import System.IO.Unsafe (unsafeDupablePerformIO)
import Unison.Prelude hiding (ByteString, empty)
import Basement.Block (Block)
import qualified Data.ByteArray as B
import qualified Data.ByteArray.Encoding as BE
import qualified Data.ByteString.Lazy as LB
import qualified Data.FingerTree as T
import qualified Data.Text as Text
import qualified Unison.Util.Rope as R

-- Block is just `newtype Block a = Block ByteArray#`
type ByteString = Block Word8

-- Bytes type represented as a finger tree of ByteStrings.
-- Can be efficiently sliced and indexed, using the byte count
-- annotation at each subtree.
newtype Bytes = Bytes (R.Rope (View ByteString)) deriving (Eq,Ord)

null :: Bytes -> Bool
null (Bytes bs) = R.null bs

empty :: Bytes
empty = Bytes mempty

fromArray :: B.ByteArrayAccess ba => ba -> Bytes
fromArray = snoc empty

toArray :: forall bo . B.ByteArray bo => Bytes -> bo
toArray b = B.concat (map B.convert (chunks b) :: [bo])

toView :: B.ByteArrayAccess ba => ba -> View ByteString
toView b = view (B.convert b)

viewToArray :: B.ByteArray bo => View ByteString -> bo
viewToArray = B.convert

toLazyByteString :: Bytes -> LB.ByteString
toLazyByteString b = LB.fromChunks $ map B.convert $ chunks b

size :: Bytes -> Int
size (Bytes bs) = R.size bs

chunks :: Bytes -> [View ByteString]
chunks (Bytes b) = toList b

fromChunks :: [View ByteString] -> Bytes
fromChunks = foldl' snocView empty

snocView :: Bytes -> View ByteString -> Bytes
snocView bs b | B.null b = bs
snocView (Bytes bs) b = Bytes (bs `R.snoc` b)

cons :: B.ByteArrayAccess ba => ba -> Bytes -> Bytes
cons b bs | B.null b = bs
cons b (Bytes bs) = Bytes (view (B.convert b) `R.cons` bs)

snoc :: B.ByteArrayAccess ba => Bytes -> ba -> Bytes
snoc bs b | B.null b = bs
snoc (Bytes bs) b = Bytes (bs `R.snoc` view (B.convert b))

flatten :: Bytes -> Bytes
flatten b = snoc mempty (B.concat (chunks b) :: ByteString)

take :: Int -> Bytes -> Bytes
take n (Bytes bs) = Bytes (R.take n bs)

drop :: Int -> Bytes -> Bytes
drop n (Bytes bs) = Bytes (R.drop n bs)

at :: Int -> Bytes -> Maybe Word8
at i (Bytes bs) = R.index i bs

toBase16 :: Bytes -> Bytes
toBase16 bs = foldl' step empty (chunks bs) where
  step bs b = snoc bs (BE.convertToBase BE.Base16 b :: ByteString)

fromBase16 :: Bytes -> Either Text.Text Bytes
fromBase16 bs = case traverse convert (chunks bs) of
  Left e -> Left (Text.pack e)
  Right bs -> Right (fromChunks (map view bs))
  where
    convert b = BE.convertFromBase BE.Base16 b :: Either String ByteString

toBase32, toBase64, toBase64UrlUnpadded :: Bytes -> Bytes
toBase32 = toBase BE.Base32
toBase64 = toBase BE.Base64
toBase64UrlUnpadded = toBase BE.Base64URLUnpadded

fromBase32, fromBase64, fromBase64UrlUnpadded :: Bytes -> Either Text.Text Bytes
fromBase32 = fromBase BE.Base32
fromBase64 = fromBase BE.Base64
fromBase64UrlUnpadded = fromBase BE.Base64URLUnpadded

fromBase :: BE.Base -> Bytes -> Either Text.Text Bytes
fromBase e bs = case BE.convertFromBase e (toArray bs :: ByteString) of
  Left e -> Left (Text.pack e)
  Right b -> Right $ snocView empty (view b)

toBase :: BE.Base -> Bytes -> Bytes
toBase e bs = snoc empty (BE.convertToBase e (toArray bs :: ByteString) :: ByteString)

toWord8s :: Bytes -> [Word8]
toWord8s bs = chunks bs >>= B.unpack

fromWord8s :: [Word8] -> Bytes
fromWord8s bs = fromArray (view $ B.pack bs :: View ByteString)

instance Monoid Bytes where
  mempty = Bytes mempty
  mappend (Bytes b1) (Bytes b2) = Bytes (b1 `mappend` b2)

instance Semigroup Bytes where (<>) = mappend

instance T.Measured (Sum Int) (View ByteString) where
  measure b = Sum (B.length b)

instance Show Bytes where
  show bs = toWord8s (toBase16 bs) >>= \w -> [chr (fromIntegral w)]

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

instance R.Take (View bytes) where
  take k (View i n bs) = View i (min k n) bs

instance R.Drop (View bytes) where
  drop k (View i n bs) = View (i + (k `min` n)) (n - (k `min` n)) bs

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

instance Semigroup (View ByteString) where
  b1 <> b2 = view (B.convert b1 <> B.convert b2 :: ByteString)

instance B.ByteArrayAccess bytes => Show (View bytes) where
  show v = show (B.unpack v)

instance B.ByteArrayAccess bytes => B.ByteArrayAccess (View bytes) where
  length = viewSize
  withByteArray v f = B.withByteArray (unView v) $
    \ptr -> f (ptr `plusPtr` (viewOffset v))

instance B.ByteArrayAccess bytes => R.Sized (View bytes) where
  size = B.length

instance B.ByteArrayAccess bytes => R.Index (View bytes) Word8 where
  index i a = if i >= 0 && i < R.size a then Just (B.index a i) else Nothing

