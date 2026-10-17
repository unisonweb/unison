{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- The JIT's C helpers read and build these constructors (unison-runtime's
-- cbits/jit_rt.c), so their layout must be the same in every build: fields are
-- unpacked only when optimizing.
{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}

module Unison.Util.Text where

import Data.Digest.Murmur64 (Hash64, Hashable64 (..))
import Data.Foldable (toList)
import Data.List (unfoldr)
import Data.List qualified as L
import Data.String (IsString (..))
import Data.Text qualified as T
import Data.Text.Array qualified as TA
import Data.Text.Encoding qualified as T
import Data.Text.Internal qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Unsafe qualified as T (Iter (..), iter)
import Data.Word (Word64)
import GHC.Exts (Int (I#), MutableByteArray#, RealWorld, casIntArray#, copyByteArray#, newByteArray#, readIntArray#, unsafeCoerce#, unsafeFreezeByteArray#, writeIntArray#)
import GHC.IO (IO (IO), unsafeDupablePerformIO)
import Unison.Util.Bytes qualified as B
import Unison.Util.Rope qualified as R
import Prelude hiding (drop, replicate, take)

-- Text type represented as a `Rope` of chunks
newtype Text = Text (R.Rope Chunk)
  deriving stock (Eq, Ord)
  deriving newtype (Semigroup, Monoid)

-- A chunk: its character count, its capacity and its text. Text is a UTF-8
-- slice of a byte array, so a chunk unpacks to the array, the count, the
-- capacity, the byte offset and the byte length; the JIT's C helpers read and
-- build that layout (cbits/jit_rt.c, the text RopeKind).
--
-- Slack. An array that 'joinChunks' allocates is larger than its contents, so
-- that the next append can write into it instead of copying: word 0 of the
-- array holds its used byte length, the contents start at byte 8, and the
-- chunk's capacity is the array's size in bytes. A chunk owns the array's tail
-- exactly when its end is the used length; an append to such a chunk advances
-- the used length by compare-and-swap, so of two appends to the same chunk the
-- first writes in place and the second copies. Bytes within a chunk's slice
-- are never written again, so a chunk's text is as immutable as any other. A
-- chunk cut from a text or taken from a Data.Text has capacity 0 and is
-- always copied.
data Chunk = Chunk {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !T.Text

empty :: Text
empty = Text mempty

one, singleton :: Char -> Text
one ch = Text (R.one (chunk (T.singleton ch)))
singleton = one

threshold :: Int
threshold = 512

replicate :: Int -> Text -> Text
replicate n t | size t * n < threshold = Text (R.one (chunk (T.replicate n (toText t))))
replicate 0 _ = mempty
replicate 1 t = t
replicate n t =
  replicate (n `div` 2) t <> replicate (n - (n `div` 2)) t

toLazyText :: Text -> TL.Text
toLazyText (Text t) = TL.fromChunks (chunkToText <$> toList t)

fromLazyText :: TL.Text -> Text
fromLazyText =
  Text . foldl' (\t -> R.snoc t . chunk) mempty . TL.toChunks

chunkToText :: Chunk -> T.Text
chunkToText (Chunk _ _ t) = t

chunk :: T.Text -> Chunk
chunk t = Chunk (T.length t) 0 t

-- | A chunk's capacity: the size of its array when it was allocated with slack, else 0.
chunkCapacity :: Chunk -> Int
chunkCapacity (Chunk _ cap _) = cap

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
    up (Chunk n _ t) = Chunk n 0 (T.toUpper t)

toLowercase :: Text -> Text
toLowercase (Text t) = Text (R.map down t)
  where
    down (Chunk n _ t) = Chunk n 0 (T.toLower t)

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

fromTextUnchunked :: T.Text -> Text
fromTextUnchunked t = Text . R.one $ chunk t

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

indexOf :: Text -> Text -> Maybe Word64
indexOf "" _ = Just 0
indexOf needle haystack =
  case TL.breakOn needle' haystack' of
    (_, "") -> Nothing
    (prefix, _) -> Just (fromIntegral (TL.length prefix))
  where
    needle' = toLazyText needle
    haystack' = toLazyText haystack

-- | Return the ordinal representation of a number in English.
--   A number ending with '1' must finish with 'st'
--   A number ending with '2' must finish with 'nd'
--   A number ending with '3' must finish with 'rd'
--   _except_ for 11, 12, and 13 which must finish with 'th'
ordinal :: (IsString s) => Int -> s
ordinal n = do
  let s = show n
  fromString $
    s ++ case L.drop (L.length s - 2) s of
      ['1', '1'] -> "th"
      ['1', '2'] -> "th"
      ['1', '3'] -> "th"
      _ -> case last s of
        '1' -> "st"
        '2' -> "nd"
        '3' -> "rd"
        _ -> "th"

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

-- Adds the codepoints of the Text into a murmur hash. This depends only
-- on the characters, so adding texts chunked in different ways, or
-- multiple texts that would concatenate to the same text will give the
-- same result.
hash64AddText :: Text -> Hash64 -> Hash64
hash64AddText (Text tx) h = foldl' addChunk h tx
  where
    addChunk h (Chunk _ _ tx) = T.foldl' (flip hash64Add) h tx

foldMapChunks :: (Monoid r) => (T.Text -> r) -> Text -> r
foldMapChunks f (Text tx) = foldMap (f . chunkToText) tx

instance Eq Chunk where (Chunk n _ a) == (Chunk n2 _ a2) = n == n2 && a == a2

instance Ord Chunk where (Chunk _ _ a) `compare` (Chunk _ _ a2) = compare a a2

instance Semigroup Chunk where
  l@(Chunk n _ _) <> r@(Chunk m _ _)
    | n == 0 = r
    | m == 0 = l
    | otherwise = joinChunks l r
  {-# INLINE (<>) #-}

-- | Both chunks' characters as one chunk: in place when the left chunk owns
-- its array's tail and has room (see 'Chunk'), else in a fresh array.
joinChunks :: Chunk -> Chunk -> Chunk
joinChunks (Chunk n cap (T.Text (TA.ByteArray arr) off len)) (Chunk m _ (T.Text (TA.ByteArray arr2) off2 len2)) =
  unsafeDupablePerformIO (IO go)
  where
    !end = off + len
    !need = len + len2
    !(I# off#) = off
    !(I# off2#) = off2
    !(I# len#) = len
    !(I# len2#) = len2
    !(I# end#) = end
    !(I# newEnd#) = end + len2
    go s0
      | cap > 0,
        end + len2 <= cap,
        marr <- unsafeCoerce# arr :: MutableByteArray# RealWorld =
          case readIntArray# marr 0# s0 of
            (# s1, used# #)
              | I# used# == end ->
                  case casIntArray# marr 0# end# newEnd# s1 of
                    (# s2, old# #)
                      | I# old# == end ->
                          case copyByteArray# arr2 off2# marr end# len2# s2 of
                            s3 -> (# s3, Chunk (n + m) cap (T.Text (TA.ByteArray arr) off (len + len2)) #)
                      | otherwise -> copy s2
              | otherwise -> copy s1
      | otherwise = copy s0
    copy s0 = case B.slackCapacity cap need of
      0 -> (# s0, Chunk (n + m) 0 (T.Text (TA.ByteArray arr) off len <> T.Text (TA.ByteArray arr2) off2 len2) #)
      newCap@(I# newCap#) ->
        let !(I# used#) = 8 + need
            !(I# after#) = 8 + len
         in case newByteArray# newCap# s0 of
              (# s1, marr #) -> case writeIntArray# marr 0# used# s1 of
                s2 -> case copyByteArray# arr off# marr 8# len# s2 of
                  s3 -> case copyByteArray# arr2 off2# marr after# len2# s3 of
                    s4 -> case unsafeFreezeByteArray# marr s4 of
                      (# s5, farr #) -> (# s5, Chunk (n + m) newCap (T.Text (TA.ByteArray farr) 8 need) #)
{-# NOINLINE joinChunks #-}

instance Monoid Chunk where
  mempty = Chunk 0 0 mempty

instance R.Sized Chunk where size (Chunk n _ _) = n

instance R.Drop Chunk where
  drop k c@(Chunk n _ t)
    | k >= n = mempty
    | k <= 0 = c
    | otherwise = Chunk (n - k) 0 (T.drop k t)

instance R.Take Chunk where
  take k c@(Chunk n _ t)
    | k >= n = c
    | k <= 0 = mempty
    | otherwise = Chunk k 0 (T.take k t)

instance R.Index Chunk Char where
  unsafeIndex i (Chunk _ _ t) = T.index t i

instance R.Reverse Chunk where
  reverse (Chunk n _ t) = Chunk n 0 (T.reverse t)

instance R.Sized Text where size (Text t) = R.size t

instance Show Text where
  show t = show (toText t)

instance IsString Text where
  fromString = pack
