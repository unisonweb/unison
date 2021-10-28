{-# Language BangPatterns #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.Util.Text where

import Data.List (unfoldr)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Unison.Util.Bytes as B
import qualified Unison.Util.Rope as R

-- Text type represented as a `Rope` of chunks
newtype Text = Text (R.Rope Chunk)

data Chunk = Chunk {-# unpack #-} !Int {-# unpack #-} !T.Text

chunkToText :: Chunk -> T.Text
chunkToText (Chunk _ t) = t

chunk :: T.Text -> Chunk
chunk t = Chunk (T.length t) t 

instance Semigroup Chunk where (<>) = mappend
instance Monoid Chunk where
  mempty = Chunk 0 mempty
  mappend l r = Chunk (R.size l + R.size r) (chunkToText l <> chunkToText r) 

instance R.Sized Chunk where size (Chunk n _) = n 
instance R.Drop Chunk where 
  drop k c@(Chunk n t) 
    | k >= n = mempty
    | k <= 0 = c 
    | otherwise = Chunk (n-k) (T.drop k t)
instance R.Take Chunk where 
  take k c@(Chunk n t)
    | k >= n = c 
    | k <= 0 = mempty 
    | otherwise = Chunk k (T.take k t)
instance R.Index Chunk Char where
  index i (Chunk n t) | i < n     = Just (T.index t i)
                      | otherwise = Nothing

instance R.Reverse Chunk where 
  reverse (Chunk n t) = Chunk n (T.reverse t)

take :: Int -> Text -> Text
take n (Text t) = Text (R.take n t)

drop :: Int -> Text -> Text
drop n (Text t) = Text (R.drop n t)

at :: Int -> Text -> Maybe Char
at n (Text t) = R.index n t

reverse :: Text -> Text
reverse (Text t) = Text (R.reverse t)

fromUtf8 :: B.Bytes -> Either String Text
fromUtf8 bs = 
  case T.decodeUtf8' (B.toByteString bs) of
    Right t -> Right (fromText t)
    Left e -> Left (show e)

toUtf8 :: Text -> B.Bytes
toUtf8 (Text t) = B.Bytes (R.map (B.chunkFromByteString . T.encodeUtf8 . chunkToText) t)

fromText :: T.Text -> Text
fromText s = let
  go !acc s = case T.splitAt 512 s of
    (t,_) | T.null t -> acc
    (hd,tl) -> go (acc `R.snoc` chunk hd) tl
  in Text (go mempty s)

fromString :: String -> Text
fromString = fromText . T.pack

toText :: Text -> T.Text
toText (Text t) = T.concat (chunkToText <$> unfoldr R.uncons t)
{-# inline toText #-}

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


-}
