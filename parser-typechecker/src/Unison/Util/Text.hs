{-# Language BangPatterns #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.Util.Text where

-- import Data.List (unfoldr)
import qualified Data.Text as T
import qualified Unison.Util.Bytes as B
import qualified Unison.Util.Rope as R
-- import qualified Data.ByteString as ByteString
-- import qualified Data.ByteString.Lazy as LazyByteString

-- Text type represented as a `Rope` of chunks
type Text = R.Rope Chunk

type Chunk = T.Text 

instance R.Sized Chunk where size = T.length 
instance R.Drop Chunk where drop = T.drop
instance R.Take Chunk where take = T.take
instance R.Index Chunk Char where 
  index n t | n < T.length t = Just (T.index t n)
            | otherwise      = Nothing
instance R.Reverse Chunk where reverse = T.reverse

take :: Int -> Text -> Text
take = R.take

drop :: Int -> Text -> Text
drop = R.drop

at :: Int -> Text -> Maybe Char
at = R.index

reverse :: Text -> Text
reverse = R.reverse

fromUtf8 :: B.Bytes -> Maybe Text
fromUtf8 _bs = undefined

-- toUtf8 :: Text -> B.Bytes
-- toUtf8 t = B.Bytes (R.map (B.toView . T.toByteString) t)

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
-}
