{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.Util.Text where

import Data.Foldable (toList)
import Data.List (foldl', unfoldr)
import Data.String (IsString (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Internal as T
import qualified Data.Text.Unsafe as T (Iter (..), iter)
import qualified Unison.Util.Bytes as B
import qualified Unison.Util.Rope as R
import Prelude hiding (drop, replicate, take)

-- Text type represented as a `Rope` of chunks
newtype Text = Text (R.Rope Chunk)
  deriving stock (Eq, Ord)
  deriving newtype (Semigroup, Monoid)

data Chunk = Chunk {-# UNPACK #-} !Int {-# UNPACK #-} !T.Text

empty :: Text
empty = Text mempty

one, singleton :: Char -> Text
one ch = Text (R.one (chunk (T.singleton ch)))
singleton = one

appendUnbalanced :: Text -> Text -> Text
appendUnbalanced (Text t1) (Text t2) = Text (R.two t1 t2)

threshold :: Int
threshold = 512

replicate :: Int -> Text -> Text
replicate n t | size t * n < threshold = Text (R.one (chunk (T.replicate n (toText t))))
replicate 0 _ = mempty
replicate 1 t = t
replicate n t =
  replicate (n `div` 2) t <> replicate (n - (n `div` 2)) t

chunkToText :: Chunk -> T.Text
chunkToText (Chunk _ t) = t

chunk :: T.Text -> Chunk
chunk t = Chunk (T.length t) t

take :: Int -> Text -> Text
take n (Text t) = Text (R.take n t)

drop :: Int -> Text -> Text
drop n (Text t) = Text (R.drop n t)

uncons :: Text -> Maybe (Char, Text)
uncons t | size t == 0 = Nothing
uncons t = (,drop 1 t) <$> at 0 t

unsnoc :: Text -> Maybe (Text, Char)
unsnoc t | size t == 0 = Nothing
unsnoc t = (take (size t - 1) t,) <$> at (size t - 1) t

unconsChunk :: Text -> Maybe (Chunk, Text)
unconsChunk (Text r) = (\(a, b) -> (a, Text b)) <$> R.uncons r

unsnocChunk :: Text -> Maybe (Text, Chunk)
unsnocChunk (Text r) = (\(a, b) -> (Text a, b)) <$> R.unsnoc r

at :: Int -> Text -> Maybe Char
at n (Text t) = R.index n t

size :: Text -> Int
size (Text t) = R.size t

reverse :: Text -> Text
reverse (Text t) = Text (R.reverse t)

toUppercase :: Text -> Text
toUppercase (Text t) = Text (R.map up t)
  where
    up (Chunk n t) = Chunk n (T.toUpper t)

toLowercase :: Text -> Text
toLowercase (Text t) = Text (R.map down t)
  where
    down (Chunk n t) = Chunk n (T.toLower t)

fromUtf8 :: B.Bytes -> Either String Text
fromUtf8 bs =
  case T.decodeUtf8' (B.toByteString bs) of
    Right t -> Right (fromText t)
    Left e -> Left (show e)

toUtf8 :: Text -> B.Bytes
toUtf8 (Text t) = B.Bytes (R.map (B.chunkFromByteString . T.encodeUtf8 . chunkToText) t)

fromText :: T.Text -> Text
fromText s | T.null s = mempty
fromText s = Text (go (chunk <$> T.chunksOf threshold s))
  where
    go = foldl' R.snoc mempty

pack :: String -> Text
pack = fromText . T.pack
{-# INLINE pack #-}

toString, unpack :: Text -> String
toString (Text bs) = toList bs >>= (T.unpack . chunkToText)
{-# INLINE toString #-}
{-# INLINE unpack #-}
unpack = toString

toText :: Text -> T.Text
toText (Text t) = T.concat (chunkToText <$> unfoldr R.uncons t)
{-# INLINE toText #-}

-- Drop with both a maximum size and a predicate. Yields actual number of
-- dropped characters.
--
-- Unavailable from text package.
dropTextWhileMax :: (Char -> Bool) -> Int -> T.Text -> (Int, T.Text)
dropTextWhileMax p n t@(T.Text arr off len) = loop 0 0
  where
    loop !i !j
      | j >= len = (i, T.empty)
      | i < n, p c = loop (i + 1) (j + d)
      | otherwise = (i, T.Text arr (off + j) (len - j))
      where
        T.Iter c d = T.iter t j
{-# INLINE [1] dropTextWhileMax #-}

dropWhileMax :: (Char -> Bool) -> Int -> Text -> (Int, Text)
dropWhileMax p = go 0
  where
    go !total !d t
      | d <= 0 = (total, t)
      | Just (chunk, t) <- unconsChunk t =
          case dropTextWhileMax p d (chunkToText chunk) of
            (i, rest)
              | T.null rest, i < d -> go (total + i) (d - i) t
              | T.null rest -> (total + i, t)
              | otherwise -> (total + i, fromText rest <> t)
      | otherwise = (total, empty)
{-# INLINE dropWhileMax #-}

instance Eq Chunk where (Chunk n a) == (Chunk n2 a2) = n == n2 && a == a2

instance Ord Chunk where (Chunk _ a) `compare` (Chunk _ a2) = compare a a2

instance Semigroup Chunk where (<>) = mappend

instance Monoid Chunk where
  mempty = Chunk 0 mempty
  mappend l r = Chunk (R.size l + R.size r) (chunkToText l <> chunkToText r)

instance R.Sized Chunk where size (Chunk n _) = n

instance R.Drop Chunk where
  drop k c@(Chunk n t)
    | k >= n = mempty
    | k <= 0 = c
    | otherwise = Chunk (n - k) (T.drop k t)

instance R.Take Chunk where
  take k c@(Chunk n t)
    | k >= n = c
    | k <= 0 = mempty
    | otherwise = Chunk k (T.take k t)

instance R.Index Chunk Char where
  unsafeIndex i (Chunk _ t) = T.index t i

instance R.Reverse Chunk where
  reverse (Chunk n t) = Chunk n (T.reverse t)

instance R.Sized Text where size (Text t) = R.size t

instance Show Text where
  show t = show (toText t)

instance IsString Text where
  fromString = pack
