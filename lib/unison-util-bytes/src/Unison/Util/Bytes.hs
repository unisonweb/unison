{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Unison.Util.Bytes
  ( Bytes (..),
    Chunk (..),
    fromByteString,
    toByteString,
    fromWord8s,
    toWord8s,
    fromBase16,
    toBase16,
    fromBase32,
    toBase32,
    fromBase64,
    toBase64,
    fromByteArray,
    toByteArray,
    fromBase64UrlUnpadded,
    toBase64UrlUnpadded,
    chunkFromByteString,
    byteStringToChunk,
    chunkToByteString,
    fromChunks,
    chunks,
    byteStringChunks,
    toArray,
    fromArray,
    toLazyByteString,
    fromLazyByteString,
    flatten,
    at,
    index16be,
    index16le,
    index32be,
    index32le,
    index64be,
    index64le,
    take,
    drop,
    indexOf,
    size,
    empty,
    encodeNat16be,
    decodeNat16be,
    encodeNat32be,
    decodeNat32be,
    encodeNat64be,
    decodeNat64be,
    encodeNat16le,
    decodeNat16le,
    encodeNat32le,
    decodeNat32le,
    encodeNat64le,
    decodeNat64le,
    decodeUtf8,
    encodeUtf8,
    zlibCompress,
    zlibDecompress,
    gzipCompress,
    gzipDecompress,
    zstdCompress,
    zstdDecompress,
    hash64AddBytes,
  )
where

import Basement.Block.Mutable (Block (Block))
import Codec.Compression.GZip qualified as GZip
import Codec.Compression.Zlib qualified as Zlib
import Codec.Compression.Zstd qualified as Zstd
import Control.DeepSeq (NFData (..))
import Control.Exception (throw)
import Control.Monad.Primitive (unsafeIOToPrim, unsafePrimToIO)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftR)
import Data.ByteArray qualified as BA
import Data.ByteArray.Encoding qualified as BE
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as LB
import Data.ByteString.Lazy.Search qualified as SS
import Data.Char
import Data.Digest.Murmur64 (Hash64, hash64AddInt)
import Data.Primitive.ByteArray
  ( ByteArray (ByteArray),
    MutableByteArray,
    byteArrayFromListN,
    compareByteArrays,
    copyByteArray,
    copyByteArrayToPtr,
    emptyByteArray,
    indexByteArray,
    newByteArray,
    runByteArray,
    sizeofByteArray,
    unsafeFreezeByteArray,
    writeByteArray,
  )
import Data.Primitive.Ptr (Ptr, copyPtrToMutableByteArray)
import Data.Semigroup (Semigroup (..))
import Data.Text qualified as Text
import Foreign.Ptr (plusPtr)
import GHC.ByteOrder (ByteOrder (..), targetByteOrder)
import Unison.Prelude hiding (ByteString, empty)
import Unison.Util.Rope qualified as R
import Prelude hiding (drop, take)


withByteArrayST ::
  BA.ByteArrayAccess a => a -> (Ptr Word8 -> ST s ()) -> ST s ()
withByteArrayST a k =
  unsafeIOToPrim $ BA.withByteArray a (unsafePrimToIO . k)

data Chunk =
  Chunk { _off :: {-# UNPACK #-} !Int,
          chunkSize :: {-# UNPACK #-} !Int,
          _arr :: {-# UNPACK #-} !ByteArray
        }

emptyChunk :: Chunk
emptyChunk = Chunk 0 0 emptyByteArray

instance Eq Chunk where
  Chunk ol ll al == Chunk or lr ar =
    ll == lr && compareByteArrays al ol ar or ll == EQ
  {-# INLINE (==) #-}

instance Ord Chunk where
  Chunk ol ll al `compare` Chunk or lr ar =
    compareByteArrays al ol ar or (min ll lr) <> compare ll lr
  {-# INLINE compare #-}

concatChunks :: [Chunk] -> Chunk
concatChunks cs =
  createChunk len \m ->
    let go !_ [] = pure ()
        go !mo (Chunk o l a : cs) =
          copyByteArray m mo a o l *> go (mo+l) cs
    in go 0 cs
  where
    len = foldl' (\acc (Chunk _ l _) -> acc + l) 0 cs
{-# INLINE concatChunks #-}

foldl'Chunk :: (r -> Word8 -> r) -> r -> Chunk -> r
foldl'Chunk f z (Chunk o l a) = go z o
  where
    n = o+l
    go !acc i
      | i < n = go (f acc $ indexByteArray a i) (i+1)
      | otherwise = acc

instance Semigroup Chunk where
  cl@(Chunk ol ll al) <> cr@(Chunk or lr ar)
    | ll == 0 = cr
    | lr == 0 = cl
    | otherwise =
        createChunk (ll + lr) \m ->
          copyByteArray m 0 al ol ll *> copyByteArray m ll ar or lr

  sconcat cs = concatChunks (toList cs)

  stimes i c@(Chunk o l a)
    | j < 1 = emptyChunk
    | j == 1 = c
    | fromIntegral l * j > m = error "stimes @Chunk: size too large"
    | k <- fromIntegral i = createChunk (l*k) \m ->
        let go 0 = pure ()
            go (subtract 1 -> n) =
              copyByteArray m (n*l) a o l *> go n
        in go k
    where
      j :: Integer
      j = fromIntegral i
      m :: Integer
      m = fromIntegral (maxBound :: Int)

instance Monoid Chunk where
  mempty = emptyChunk
  mconcat = concatChunks

-- Bytes type represented as a rope of ByteStrings
newtype Bytes = Bytes {underlying :: R.Rope Chunk}
  deriving stock (Eq, Ord)
  deriving newtype (Semigroup, Monoid)

instance R.Sized Chunk where size = chunkSize

instance R.Drop Chunk where
  drop n c@(Chunk o l a)
    | n == 0 = c
    | n >= l = emptyChunk
    | otherwise = Chunk (o+n) (l-n) a

instance R.Take Chunk where
  take 0 _ = emptyChunk
  take n c@(Chunk o l a)
    | n < l = Chunk o n a
    | otherwise = c

instance R.Index Chunk Word8 where
  unsafeIndex n (Chunk o _ a) = indexByteArray a (n + o)

instance R.Reverse Chunk where
  reverse (Chunk o l a) = createChunk l \m ->
    let e = o + l - 1
        go i
          | i < l = writeByteArray m i $ indexByteArray @Word8 a (e - i)
          | otherwise = pure ()
    in go 0

instance NFData Bytes where rnf _ = ()

createByteArray ::
  Int -> (forall s. MutableByteArray s -> ST s ()) -> ByteArray
createByteArray sz f =
  runByteArray $
    newByteArray sz >>= \ma -> ma <$ f ma
{-# INLINE createByteArray #-}

createChunk ::
  Int -> (forall s. MutableByteArray s -> ST s ()) -> Chunk
createChunk sz f = Chunk 0 sz $ createByteArray sz f
{-# INLINE createChunk #-}

gCreateChunk ::
  Int -> (forall s. MutableByteArray s -> ST s r) -> (Chunk, r)
gCreateChunk sz f = runST do
  ma <- newByteArray sz
  r <- f ma
  (,r) . Chunk 0 sz <$> unsafeFreezeByteArray ma
{-# INLINE gCreateChunk #-}

whenLittleEndian :: (a -> a) -> a -> a
whenLittleEndian
  | LittleEndian <- targetByteOrder = \f x -> f x
  | otherwise = \_ x -> x
{-# INLINE whenLittleEndian #-}

whenBigEndian :: (a -> a) -> a -> a
whenBigEndian
  | BigEndian <- targetByteOrder = \f x -> f x
  | otherwise = \_ x -> x
{-# INLINE whenBigEndian #-}

-- Given an offset, size and bytes, extracts a byte array that satisfies
-- the following properties
--
--   1. The array starts at the given offset in the bytes.
--   2. The array contains _at least_ the specified number of bytes from
--      the original bytes.
--
-- The purpose of this is to avoid complicated logic for dealing with
-- reading out of a bytes. `indexByteArray` not only requires a single
-- array (obviously), but one that is 'aligned,' because indexing is
-- element-wise. The above properties ensure both.
extractChunkArr :: Int -> Int -> Bytes -> ByteArray
extractChunkArr ix ln (Bytes bs) = fixAlign ln $ R.extractChunk ix ln bs

fixAlign :: Int -> Chunk -> ByteArray
fixAlign ln (Chunk o _ ba)
  | o == 0 = ba
  | otherwise = createByteArray ln (\m -> copyByteArray m 0 ba o ln)

null :: Bytes -> Bool
null = R.null . underlying

empty :: Bytes
empty = mempty

isAsciiChunk :: Chunk -> Bool
isAsciiChunk (Chunk o l a) = test o
  where
    n = o+l
    test i
      | i >= n = True
      | indexByteArray @Word8 a i <= 0x7F = test (i+1)
      | otherwise = False

isAscii :: Bytes -> Bool
isAscii b = all isAsciiChunk (chunks b)

fromByteString :: B.ByteString -> Bytes
fromByteString b = snoc empty (byteStringToChunk b)

toByteString :: Bytes -> B.ByteString
toByteString b = B.concat (map chunkToByteString (chunks b))

toArray :: (BA.ByteArray b) => Bytes -> b
toArray (Bytes r) =
  BA.allocAndFreeze (R.size r) \(p :: Ptr Word8) ->
    let f po (Chunk o l a)
          | (p :: Ptr Word8) <- p `plusPtr` po =
              copyByteArrayToPtr p a o l
    in R.traverseWithPos_ f r
{-# INLINE toArray #-}

fromArray :: (BA.ByteArrayAccess b) => b -> Bytes
fromArray b = snoc empty (arrayToChunk b)

fromByteArray :: Int -> Int -> ByteArray -> Bytes
fromByteArray o l ba = snoc empty (Chunk o l ba)

toByteArray :: Bytes -> ByteArray
toByteArray (Bytes r)
  | R.One (Chunk 0 l a) <- r,
    l == sizeofByteArray a = a
  | otherwise =
      createByteArray (R.size r) \m ->
        R.traverseWithPos_ (f m) r
  where
    f m p (Chunk o l a) = copyByteArray m p a o l

byteStringToChunk, chunkFromByteString :: B.ByteString -> Chunk
byteStringToChunk bs
  | sz == 0 = emptyChunk
  | otherwise = createChunk sz \m ->
      withByteArrayST bs \p ->
        copyPtrToMutableByteArray m 0 p sz
  where
    sz = B.length bs

chunkFromByteString = byteStringToChunk

chunkToByteString :: Chunk -> B.ByteString
chunkToByteString (Chunk o l a)
  | l == 0 = B.empty
  | otherwise =
      BA.allocAndFreeze l \(p :: Ptr Word8) ->
        copyByteArrayToPtr p a o l

zlibCompress :: Bytes -> Bytes
zlibCompress = fromLazyByteString . Zlib.compress . toLazyByteString

gzipCompress :: Bytes -> Bytes
gzipCompress = fromLazyByteString . GZip.compress . toLazyByteString

zstdCompress :: Int -> Bytes -> Bytes
zstdCompress level = fromByteString . Zstd.compress level . toByteString

gzipDecompress :: Bytes -> Bytes
gzipDecompress = fromLazyByteString . GZip.decompress . toLazyByteString

zlibDecompress :: Bytes -> Bytes
zlibDecompress = fromLazyByteString . Zlib.decompress . toLazyByteString

{- HLINT ignore "Use newtype instead of data" -}
data ZstdDecompressException = ZstdDecompressException String deriving (Show, Exception)

zstdDecompress :: Bytes -> Bytes
zstdDecompress = fromByteString . getOrThrow . Zstd.decompress . toByteString
  where
    getOrThrow (Zstd.Decompress bs) = bs
    getOrThrow Zstd.Skip = B.empty
    getOrThrow (Zstd.Error err) = throw $ ZstdDecompressException err

toLazyByteString :: Bytes -> LB.ByteString
toLazyByteString b = LB.fromChunks $ map chunkToByteString $ chunks b

fromLazyByteString :: LB.ByteString -> Bytes
fromLazyByteString b = fromChunks (byteStringToChunk <$> LB.toChunks b)

size :: Bytes -> Int
size = R.size . underlying

chunks :: Bytes -> [Chunk]
chunks (Bytes bs) = toList bs

byteStringChunks :: Bytes -> [B.ByteString]
byteStringChunks bs = chunkToByteString <$> chunks bs

fromChunks :: [Chunk] -> Bytes
fromChunks = foldl' snoc empty

cons :: Chunk -> Bytes -> Bytes
cons b (Bytes bs) = Bytes (R.cons b bs)

snoc :: Bytes -> Chunk -> Bytes
snoc (Bytes bs) b = Bytes (R.snoc bs b)

flatten :: Bytes -> Bytes
flatten bs = Bytes . R.one . Chunk 0 sz $ toByteArray bs
  where
    sz = size bs

take :: Int -> Bytes -> Bytes
take n (Bytes bs) = Bytes (R.take n bs)

drop :: Int -> Bytes -> Bytes
drop n (Bytes bs) = Bytes (R.drop n bs)

indexOf :: Bytes -> Bytes -> Maybe Word64
indexOf needle haystack =
  case SS.indices needle' haystack' of
    [] -> Nothing
    (i : _) -> Just (fromIntegral i)
  where
    needle' = toByteString needle
    haystack' = toLazyByteString haystack

at, index :: Int -> Bytes -> Maybe Word8
at n (Bytes bs) = R.index n bs
index = at

-- Indexes into a bytes to retrieve a big-endian Word16. The position is
-- in bytes.
index16be :: Int -> Bytes -> Maybe Word16
index16be i bs
  | i + 1 >= size bs = Nothing
  | ba <- extractChunkArr i 2 bs =
      Just . whenLittleEndian byteSwap16 $ indexByteArray ba 0

-- Indexes into a bytes to retrieve a little-endian Word16. The position
-- is in bytes.
index16le :: Int -> Bytes -> Maybe Word16
index16le i bs
  | i + 1 >= size bs = Nothing
  | ba <- extractChunkArr i 2 bs =
      Just . whenBigEndian byteSwap16 $ indexByteArray ba 0

-- Indexes into a bytes to retrieve a big-endian Word32. The position is
-- in bytes.
index32be :: Int -> Bytes -> Maybe Word32
index32be i bs
  | i + 3 >= size bs = Nothing
  | ba <- extractChunkArr i 4 bs =
      Just . whenLittleEndian byteSwap32 $ indexByteArray ba 0

-- Indexes into a bytes to retrieve a little-endian Word32. The position
-- is in bytes.
index32le :: Int -> Bytes -> Maybe Word32
index32le i bs
  | i + 3 >= size bs = Nothing
  | ba <- extractChunkArr i 4 bs =
      Just . whenBigEndian byteSwap32 $ indexByteArray ba 0

-- Indexes into a bytes to retrieve a big-endian Word64. The position is
-- in bytes.
index64be :: Int -> Bytes -> Maybe Word64
index64be i bs
  | i + 7 >= size bs = Nothing
  | ba <- extractChunkArr i 8 bs =
      Just . whenLittleEndian byteSwap64 $ indexByteArray ba 0

-- Indexes into a bytes to retrieve a little-endian Word64. The position
-- is in bytes.
index64le :: Int -> Bytes -> Maybe Word64
index64le i bs
  | i + 7 >= size bs = Nothing
  | ba <- extractChunkArr i 8 bs =
      Just . whenBigEndian byteSwap64 $ indexByteArray ba 0

dropBlock :: Int -> Bytes -> Maybe (Chunk, Bytes)
dropBlock nBytes (Bytes chunks)
  | R.size chunks < nBytes = Nothing
  | otherwise = case R.uncons chunks of
      Nothing -> Nothing -- should be impossible
      Just (c, cs)
        | R.size c == nBytes ->
            Just (c, Bytes cs)
        | R.size c > nBytes ->
            Just (R.take nBytes c, Bytes $ R.cons (R.drop nBytes c) cs)
        | Chunk o l a <- c ->
            Just $ gCreateChunk nBytes \m ->
              copyByteArray m 0 a o l *> crawl m l cs
  where
    crawl :: MutableByteArray s -> Int -> R.Rope Chunk -> ST s Bytes
    crawl m pos chunks = case R.uncons chunks of
      Just (c@(Chunk o l a), cs)
        | l + pos == nBytes ->
            Bytes cs <$ copyByteArray m pos a o l
        | l + pos > nBytes, ln <- nBytes - pos, c <- R.drop ln c ->
            Bytes (R.cons c cs) <$ copyByteArray m pos a o ln
      -- these cases should be impossible due to length check
      _ -> pure $ Bytes chunks

decodeNat64be :: Bytes -> Maybe (Word64, Bytes)
decodeNat64be bs = case dropBlock 8 bs of
  Just (head, rest) -> Just (w, rest)
    where
      ba = fixAlign 8 head
      w = whenLittleEndian byteSwap64 $ indexByteArray ba 0
  Nothing -> Nothing

decodeNat64le :: Bytes -> Maybe (Word64, Bytes)
decodeNat64le bs = case dropBlock 8 bs of
  Just (head, rest) -> Just (w, rest)
    where
      ba = fixAlign 8 head
      w = whenBigEndian byteSwap64 $ indexByteArray ba 0
  Nothing -> Nothing

decodeNat32be :: Bytes -> Maybe (Word64, Bytes)
decodeNat32be bs = case dropBlock 4 bs of
  Just (head, rest) -> Just (fromIntegral w, rest)
    where
      ba = fixAlign 4 head
      w = whenLittleEndian byteSwap32 $ indexByteArray ba 0
  Nothing -> Nothing

decodeNat32le :: Bytes -> Maybe (Word64, Bytes)
decodeNat32le bs = case dropBlock 4 bs of
  Just (head, rest) -> Just (fromIntegral w, rest)
    where
      ba = fixAlign 4 head
      w = whenBigEndian byteSwap32 $ indexByteArray ba 0
  Nothing -> Nothing

decodeNat16be :: Bytes -> Maybe (Word64, Bytes)
decodeNat16be bs = case dropBlock 2 bs of
  Just (head, rest) -> Just (fromIntegral w, rest)
    where
      ba = fixAlign 2 head
      w = whenLittleEndian byteSwap16 $ indexByteArray ba 0
  Nothing -> Nothing

decodeNat16le :: Bytes -> Maybe (Word64, Bytes)
decodeNat16le bs = case dropBlock 2 bs of
  Just (head, rest) -> Just (fromIntegral w, rest)
    where
      ba = fixAlign 2 head
      w = whenBigEndian byteSwap16 $ indexByteArray ba 0
  Nothing -> Nothing

fillBE :: Word64 -> Int -> Int -> Word8
fillBE n k 0 = fromIntegral (shiftR n (k * 8))
fillBE n k i = fromIntegral (shiftR n ((k - i) * 8))
{-# INLINE fillBE #-}

encodeNat64be :: Word64 -> Bytes
encodeNat64be n =
  Bytes . R.one $ createChunk 8 \m ->
    writeByteArray m 0 . whenLittleEndian byteSwap64 $ fromIntegral n

encodeNat32be :: Word64 -> Bytes
encodeNat32be n =
  Bytes . R.one $ createChunk 4 \m ->
    writeByteArray m 0 . whenLittleEndian byteSwap32 $ fromIntegral n

encodeNat16be :: Word64 -> Bytes
encodeNat16be n =
  Bytes . R.one $ createChunk 2 \m ->
    writeByteArray m 0 . whenLittleEndian byteSwap16 $ fromIntegral n

fillLE :: Word64 -> Int -> Word8
fillLE n i = fromIntegral (shiftR n (i * 8))
{-# INLINE fillLE #-}

encodeNat64le :: Word64 -> Bytes
encodeNat64le n =
  Bytes . R.one $ createChunk 8 \m ->
    writeByteArray m 0 . whenBigEndian byteSwap64 $ fromIntegral n

encodeNat32le :: Word64 -> Bytes
encodeNat32le n =
  Bytes . R.one $ createChunk 4 \m ->
    writeByteArray m 0 . whenBigEndian byteSwap32 $ fromIntegral n

encodeNat16le :: Word64 -> Bytes
encodeNat16le n =
  Bytes . R.one $ createChunk 2 \m ->
    writeByteArray m 0 . whenBigEndian byteSwap16 $ fromIntegral n

toBase16 :: Bytes -> Bytes
toBase16 bs = foldl' step empty (chunks bs)
  where
    step bs b =
      snoc
        bs
        ( arrayToChunk @BA.Bytes $
            BE.convertToBase BE.Base16 (chunkToArray @BA.Bytes b)
        )

chunkToArray, arrayFromChunk :: (BA.ByteArray b) => Chunk -> b
chunkToArray (Chunk o l a) =
  BA.allocAndFreeze l $ \(ptr :: Ptr Word8) ->
    copyByteArrayToPtr ptr a o l
arrayFromChunk = chunkToArray

chunkToByteArray :: Chunk -> ByteArray
chunkToByteArray (Chunk o l a)
  | o == 0, l == sizeofByteArray a = a
  | otherwise =
      createByteArray l \m -> copyByteArray m 0 a o l

arrayToChunk, chunkFromArray :: (BA.ByteArrayAccess b) => b -> Chunk
arrayToChunk bs = case BA.convert bs :: Block Word8 of
  Block bs -> Chunk 0 n (ByteArray bs)
  where
    n = BA.length bs
{-# INLINE arrayToChunk #-}
chunkFromArray = arrayToChunk

fromBase16 :: Bytes -> Either Text.Text Bytes
fromBase16 = fromBase BE.Base16

toBase32, toBase64, toBase64UrlUnpadded :: Bytes -> Bytes
toBase32 = toBase BE.Base32
toBase64 = toBase BE.Base64
toBase64UrlUnpadded = toBase BE.Base64URLUnpadded

fromBase32, fromBase64, fromBase64UrlUnpadded :: Bytes -> Either Text.Text Bytes
fromBase32 = fromBase BE.Base32
fromBase64 = fromBase BE.Base64
fromBase64UrlUnpadded = fromBase BE.Base64URLUnpadded

fromBase :: BE.Base -> Bytes -> Either Text.Text Bytes
fromBase e bs = case BE.convertFromBase e (toArray @BA.Bytes bs) of
  Left e -> Left (Text.pack e)
  Right b -> Right $ snoc empty (chunkFromArray (b :: BA.Bytes))

toBase :: BE.Base -> Bytes -> Bytes
toBase e bs = snoc empty (arrayToChunk arr)
  where
    arr :: BA.Bytes
    arr = BE.convertToBase e (toArray @BA.Bytes bs)

toWord8s :: Bytes -> [Word8]
toWord8s bs = chunks bs >>= toList
  where
    toList (Chunk o l a) = unf o
      where
        n = o+l
        unf i | i < n = indexByteArray a i : unf (i+1)
              | otherwise = []

fromWord8s :: [Word8] -> Bytes
fromWord8s bs = snoc empty . Chunk 0 sz $ byteArrayFromListN sz bs
  where
    !sz = length bs

-- Adds the bytes of the value to a hash. This does not depend on the
-- chunking or splitting of the bytes values, just on the bytes.
hash64AddBytes :: Bytes -> Hash64 -> Hash64
hash64AddBytes bs h = foldl' addChunk h $ underlying bs
  where
    addChunk = foldl'Chunk (\h b -> hash64AddInt (fromIntegral b) h)

instance Show Bytes where
  show bs = toWord8s (toBase16 bs) >>= \w -> [chr (fromIntegral w)]
