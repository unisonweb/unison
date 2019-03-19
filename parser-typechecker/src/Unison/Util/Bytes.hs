{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.Util.Bytes where

import Data.Maybe
import Data.Foldable
import Data.Monoid (Sum(..))
import Data.Word (Word8)
import Prelude hiding (drop)
import qualified Data.ByteString as B
import qualified Data.FingerTree as T

newtype Bytes = Bytes (T.FingerTree (Sum Int) B.ByteString)

null :: Bytes -> Bool
null (Bytes bs) = T.null bs

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
take n (Bytes bs) = go (T.takeUntil (> Sum n) bs) where
  go s = Bytes $ case T.viewr s of
    sl T.:> last ->
      if T.measure sl == Sum n then s
      else sl T.|> B.take (n - getSum (T.measure sl)) last
    _ -> s

drop :: Int -> Bytes -> Bytes
drop n b0@(Bytes bs) = go (T.dropUntil (> Sum n) bs) where
  go s = Bytes $ case T.viewl s of
    head T.:< tail ->
      if T.measure tail == Sum n then s
      else B.drop (n - (size b0 - getSum (T.measure s))) head T.<| tail
    _ -> s

at :: Int -> Bytes -> Maybe Word8
at i bs = case drop i bs of
  Bytes (T.viewl -> hd T.:< _) -> Just (B.head hd)
  _ -> Nothing

toWord8s :: Bytes -> [Word8]
toWord8s bs = catMaybes [ at i bs | i <- [0..(size bs - 1)] ]

instance Monoid Bytes where
  mempty = Bytes mempty
  mappend (Bytes b1) (Bytes b2) = Bytes (b1 `mappend` b2)

instance Semigroup Bytes where (<>) = mappend

instance T.Measured (Sum Int) B.ByteString where
  measure b = Sum (B.length b)

