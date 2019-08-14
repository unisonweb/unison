{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.Util.Bytes where

import Unison.Prelude hiding (empty)

import Data.Monoid (Sum(..))
import Prelude hiding (drop)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.FingerTree as T

-- Bytes type represented as a finger tree of ByteStrings.
-- Can be efficiently sliced and indexed, using the byte count
-- annotation at each subtree.
newtype Bytes = Bytes (T.FingerTree (Sum Int) B.ByteString)

null :: Bytes -> Bool
null (Bytes bs) = T.null bs

empty :: Bytes
empty = Bytes mempty

fromByteString :: B.ByteString -> Bytes
fromByteString = snoc empty

toByteString :: Bytes -> B.ByteString
toByteString b = B.concat (chunks b)

size :: Bytes -> Int
size (Bytes bs) = getSum (T.measure bs)

chunks :: Bytes -> [B.ByteString]
chunks (Bytes b) = toList b

cons :: B.ByteString -> Bytes -> Bytes
cons b bs | B.null b = bs
cons b (Bytes bs) = Bytes (b T.<| bs)

snoc :: Bytes -> B.ByteString -> Bytes
snoc bs b | B.null b = bs
snoc (Bytes bs) b = Bytes (bs T.|> b)

flatten :: Bytes -> Bytes
flatten b = snoc mempty (B.concat (chunks b))

take :: Int -> Bytes -> Bytes
take n (Bytes bs) = go (T.split (> Sum n) bs) where
  go (ok, s) = Bytes $ case T.viewl s of
    last T.:< _ ->
      if T.measure ok == Sum n then ok
      else ok T.|> B.take (n - getSum (T.measure ok)) last
    _ -> ok

drop :: Int -> Bytes -> Bytes
drop n b0@(Bytes bs) = go (T.dropUntil (> Sum n) bs) where
  go s = Bytes $ case T.viewl s of
    head T.:< tail ->
      if (size b0 - getSum (T.measure s)) == n then s
      else B.drop (n - (size b0 - getSum (T.measure s))) head T.<| tail
    _ -> s

at :: Int -> Bytes -> Maybe Word8
at i bs = case drop i bs of
  Bytes (T.viewl -> hd T.:< _) -> Just (B.head hd)
  _ -> Nothing

toWord8s :: Bytes -> [Word8]
toWord8s bs = catMaybes [ at i bs | i <- [0..(size bs - 1)] ]

fromWord8s :: [Word8] -> Bytes
fromWord8s bs = fromByteString (B.pack bs)

instance Monoid Bytes where
  mempty = Bytes mempty
  mappend (Bytes b1) (Bytes b2) = Bytes (b1 `mappend` b2)

instance Semigroup Bytes where (<>) = mappend

instance T.Measured (Sum Int) B.ByteString where
  measure b = Sum (B.length b)

instance Show Bytes where
  show bs = show (toWord8s bs)

instance Eq Bytes where
  b1 == b2 | size b1 == size b2 = go b1 b2
    where
    go b1 b2 = BL.fromChunks (chunks b1) == BL.fromChunks (chunks b2)
  _ == _ = False

-- Lexicographical ordering
instance Ord Bytes where
  b1 `compare` b2 =
    BL.fromChunks (chunks b1) `compare` BL.fromChunks (chunks b2)
