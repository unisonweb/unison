{-# Language BangPatterns #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.Util.Text where

import Data.List (unfoldr)
import qualified Data.Text
import qualified Unison.Util.Bytes as B
import qualified Unison.Util.Rope as R
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Vector.Unboxed as V
import Data.Word
import Data.Char (chr)

-- Text type represented as a `Rope` of chunks
type Text = R.Rope Chunk

data Chunk
  = Word7s (V.Vector Word8)   -- All <= 127
  | Word16s (V.Vector Word16) -- All <= maxBound
  | Word32s (V.Vector Word32) -- All <= maxBound

instance R.Sized Chunk where
  size (Word7s cs) = V.length cs
  size (Word16s cs) = V.length cs
  size (Word32s cs) = V.length cs

instance R.Drop Chunk where
  drop n (Word7s cs) = Word7s (V.drop n cs)
  drop n (Word16s cs) = Word16s (V.drop n cs)
  drop n (Word32s cs) = Word32s (V.drop n cs)

instance R.Take Chunk where
  take n (Word7s cs) = Word7s (V.take n cs)
  take n (Word16s cs) = Word16s (V.take n cs)
  take n (Word32s cs) = Word32s (V.take n cs)

instance R.Index Chunk Char where
  index n (Word7s cs) = chr . fromIntegral <$> (cs V.!? n)
  index n (Word16s cs) = chr . fromIntegral <$> (cs V.!? n)
  index n (Word32s cs) = chr . fromIntegral <$> (cs V.!? n)

instance R.Reverse Chunk where
  reverse (Word7s cs) = Word7s (V.reverse cs)
  reverse (Word16s cs) = Word16s (V.reverse cs)
  reverse (Word32s cs) = Word32s (V.reverse cs)

instance Semigroup Chunk where (<>) = mappend
instance Monoid Chunk where
  mempty = Word7s V.empty
  mappend (Word7s cs) (Word7s cs2) = Word7s (cs <> cs2)
  mappend (Word7s cs) (Word16s cs2) = Word16s (V.map fromIntegral cs <> cs2)
  mappend (Word7s cs) (Word32s cs2) = Word32s (V.map fromIntegral cs <> cs2)

  mappend (Word16s cs) (Word7s cs2) = Word16s (cs <> V.map fromIntegral cs2)
  mappend (Word16s cs) (Word16s cs2) = Word16s (cs <> cs2)
  mappend (Word16s cs) (Word32s cs2) = Word32s (V.map fromIntegral cs <> cs2)

  mappend (Word32s cs) (Word32s cs2) = Word32s (cs <> cs2)
  mappend (Word32s cs) (Word16s cs2) = Word32s (cs <> V.map fromIntegral cs2)
  mappend (Word32s cs) (Word7s cs2) = Word32s (cs <> V.map fromIntegral cs2)

take :: Int -> Text -> Text
take = R.take

drop :: Int -> Text -> Text
drop = R.drop

at :: Int -> Text -> Maybe Char
at = R.index

reverse :: Text -> Text
reverse = R.reverse

fromUtf8 :: B.Bytes -> Maybe Text
fromUtf8 bs =
  if B.isAscii bs then Just (R.map toChunk (B.rope bs))
  else undefined bs
  where
    toChunk c = Word7s (B.viewToArray c)

  -- R.singleton <$>
  -- (T.fromByteString .
  --  ByteString.concat .
  --  LazyByteString.toChunks .
  --  B.toLazyByteString) bs

{-
dropWhile :: (Char -> Bool) -> Text -> Text
dropWhile f = let
  go t = case R.uncons t of
    Nothing -> mempty
    Just (hd, t) ->
      let hd' = T.dropWhile f hd in
      if T.null hd' then go t
      else hd' `R.cons` t
  in go
{-# INLINE dropWhile #-}

dropWhileEnd :: (Char -> Bool) -> Text -> Text
dropWhileEnd f = let
  go t = case R.unsnoc t of
    Nothing -> mempty
    Just (t, last) ->
      let last' = T.dropWhileEnd f last in
      if T.null last' then go t
      else t `R.snoc` last'
  in go
{-# INLINE dropWhileEnd #-}

takeWhile :: (Char -> Bool) -> Text -> Text
takeWhile f = let
  go :: Text -> Text -> Text
  go !acc t = case R.uncons t of
    Nothing -> mempty
    Just (hd, t) ->
      let hd' = T.takeWhile f hd in
      if R.size hd == R.size hd' then go (acc `R.snoc` hd) t
      else acc `R.snoc` hd'
  in go mempty
{-# INLINE takeWhile #-}

takeWhileEnd :: (Char -> Bool) -> Text -> Text
takeWhileEnd f = let
  go :: Text -> Text -> Text
  go !acc t = case R.unsnoc t of
    Nothing -> mempty
    Just (t, last) ->
      let last' = T.takeWhileEnd f last in
      if R.size last == R.size last' then go (last `R.cons` acc) t
      else last' `R.cons` acc
  in go mempty
{-# INLINE takeWhileEnd #-}

fromString :: String -> Text
fromString s = let
  go !acc s = case splitAt R.threshold s of
    ([],_) -> acc
    (hd,tl) -> go (acc `R.snoc` T.fromString hd) tl
  in go mempty s

fromText :: Data.Text.Text -> Text
fromText s = let
  go !acc s = case Data.Text.splitAt R.threshold s of
    (t,_) | Data.Text.null t -> acc
    (hd,tl) -> go (acc `R.snoc` T.fromText hd) tl
  in go mempty s

toText :: Text -> Data.Text.Text
toText t = Data.Text.concat (T.toText <$> unfoldr R.uncons t)

toUtf8 :: Text -> B.Bytes
toUtf8 t = B.Bytes (R.map (B.toView . T.toByteString) t)

-- todo: there's a more direct but fiddly implementation of this
-- which doesn't go through strict bytestring
fromUtf8 :: B.Bytes -> Maybe Text
fromUtf8 bs = R.singleton <$>
  (T.fromByteString .
   ByteString.concat .
   LazyByteString.toChunks .
   B.toLazyByteString) bs
-}
