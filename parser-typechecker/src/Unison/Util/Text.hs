{-# Language BangPatterns #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.Util.Text where

-- import Data.List (unfoldr)
import qualified Data.Text as T
import qualified Unison.Util.Bytes as B
import qualified Unison.Util.Rope as R

-- Text type represented as a `Rope` of chunks
newtype Text = Text (R.Rope Chunk)

data Chunk = Chunk {-# unpack #-} !Int !T.Text Chunk Chunk 

chunkToText :: Chunk -> T.Text
chunkToText (Chunk _ t _ _) = t

chunk :: T.Text -> Chunk
chunk t = sizedChunk (T.length t) t 

sizedChunk :: Int -> T.Text -> Chunk
sizedChunk 0 _ = Chunk 0 mempty mempty mempty
sizedChunk n t = Chunk n t l r where
  (lt,rt) = T.splitAt (n `div` 2) t
  l = sizedChunk (n `div` 2) lt
  r = sizedChunk (n - (n `div` 2)) rt

instance Semigroup Chunk where (<>) = mappend
instance Monoid Chunk where
  mempty = Chunk 0 mempty mempty mempty
  mappend l r = sizedChunk (R.size l + R.size r) (chunkToText l <> chunkToText r) 

instance R.Sized Chunk where size (Chunk n _ _ _) = n 
instance R.Drop Chunk where drop = undefined "todo"
instance R.Take Chunk where take = undefined "todo"
instance R.Index Chunk Char where 
  index i t = go i (R.size t) t where 
     go !i !s (Chunk _ t l r) 
        | i >= s = Nothing
        | i <= 8 = Just (T.index t i)
        | i < s `div` 2 = go i (s `div` 2) l
        | otherwise = go (i - (s `div` 2)) (s `div` 2) r

instance R.Reverse Chunk where reverse t = sizedChunk (R.size t) (T.reverse (chunkToText t))

take :: Int -> Text -> Text
take n (Text t) = Text (R.take n t)

drop :: Int -> Text -> Text
drop n (Text t) = Text (R.drop n t)

at :: Int -> Text -> Maybe Char
at n (Text t) = R.index n t

reverse :: Text -> Text
reverse (Text t) = Text (R.reverse t)

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
