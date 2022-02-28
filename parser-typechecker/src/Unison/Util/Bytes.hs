{-# Language ViewPatterns #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Unison.Util.Bytes (
  Bytes(..), Chunk, 
  fromByteString, toByteString, 
  fromWord8s, toWord8s, 
  fromBase16, toBase16, 
  fromBase32, toBase32, 
  fromBase64, toBase64, 
  fromBase64UrlUnpadded, toBase64UrlUnpadded,

  chunkFromByteString, byteStringToChunk, chunkToByteString,
  fromChunks, chunks, byteStringChunks,
  toArray, fromArray, toLazyByteString,
  flatten,

  at, take, drop, size, empty, 

  encodeNat16be, decodeNat16be,
  encodeNat32be, decodeNat32be,
  encodeNat64be, decodeNat64be,
  encodeNat16le, decodeNat16le,
  encodeNat32le, decodeNat32le,
  encodeNat64le, decodeNat64le,
  decodeUtf8, encodeUtf8,

  zlibCompress, zlibDecompress,
  gzipCompress, gzipDecompress
) where

import Control.DeepSeq (NFData(..))
import Control.Monad.Primitive (unsafeIOToPrim)
import Data.Bits (shiftR, shiftL, (.|.))
import Data.Char
import Unison.Prelude hiding (ByteString, empty)
import Prelude hiding (take, drop)
import qualified Data.ByteString as B
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BE
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as Text
import qualified Unison.Util.Rope as R
import qualified Data.Vector.Primitive as V
import qualified Data.Vector.Primitive.Mutable as MV
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as MSV
import qualified Data.Vector.Storable.ByteString as BSV
import Data.Primitive.ByteArray (copyByteArrayToPtr)
import Data.Primitive.Ptr (copyPtrToMutableByteArray)
import Foreign.Storable (pokeByteOff)
import Foreign.ForeignPtr (withForeignPtr)
import qualified Codec.Compression.Zlib as Zlib
import qualified Codec.Compression.GZip as GZip
import Unsafe.Coerce (unsafeCoerce)

type Chunk = V.Vector Word8

-- Bytes type represented as a rope of ByteStrings
newtype Bytes = Bytes { underlying :: R.Rope Chunk } deriving (Semigroup,Monoid,Eq,Ord)

instance R.Sized Chunk where size = V.length
instance R.Drop Chunk where drop = V.drop
instance R.Take Chunk where take = V.take
instance R.Index Chunk Word8 where unsafeIndex n bs = bs `V.unsafeIndex` n
instance R.Reverse Chunk where reverse = V.reverse
instance NFData Bytes where rnf _ = ()

null :: Bytes -> Bool
null = R.null . underlying

empty :: Bytes
empty = mempty

isAscii :: Bytes -> Bool
isAscii b = all (V.all (<= 0x7F)) (chunks b)

fromByteString :: B.ByteString -> Bytes
fromByteString b = snoc empty (byteStringToChunk b)

toByteString :: Bytes -> B.ByteString
toByteString b = B.concat (map chunkToByteString (chunks b))

toArray :: BA.ByteArray b => Bytes -> b
toArray b = chunkToArray $ V.concat (chunks b)

fromArray :: BA.ByteArrayAccess b => b -> Bytes
fromArray b = snoc empty (arrayToChunk b)

byteStringToChunk, chunkFromByteString :: B.ByteString -> Chunk
byteStringToChunk = fromStorable . BSV.byteStringToVector
chunkFromByteString = byteStringToChunk

chunkToByteString :: Chunk -> B.ByteString
chunkToByteString = BSV.vectorToByteString . toStorable

fromStorable :: SV.Vector Word8 -> V.Vector Word8
fromStorable sv
  = V.create $ do
      MSV.MVector l fp <- SV.unsafeThaw sv
      v@(MV.MVector _ _ ba) <- MV.unsafeNew l
      unsafeIOToPrim . withForeignPtr fp $ \p ->
        -- Note: unsafeCoerce is for s -> RealWorld in byte array type
        copyPtrToMutableByteArray (unsafeCoerce ba) 0 p l
      pure v

toStorable :: V.Vector Word8 -> SV.Vector Word8
toStorable (V.Vector o l ba) = SV.create $ do
  v@(MSV.MVector _ fp) <- MSV.unsafeNew l
  unsafeIOToPrim . withForeignPtr fp $ \p ->
    copyByteArrayToPtr p ba o l
  pure v

zlibCompress :: Bytes -> Bytes
zlibCompress = fromLazyByteString . Zlib.compress . toLazyByteString

gzipCompress :: Bytes -> Bytes
gzipCompress = fromLazyByteString . GZip.compress . toLazyByteString

gzipDecompress :: Bytes -> Bytes
gzipDecompress = fromLazyByteString . GZip.decompress . toLazyByteString

zlibDecompress :: Bytes -> Bytes
zlibDecompress = fromLazyByteString . Zlib.decompress . toLazyByteString

toLazyByteString :: Bytes -> LB.ByteString
toLazyByteString b = LB.fromChunks $ map chunkToByteString $ chunks b

fromLazyByteString :: LB.ByteString -> Bytes
fromLazyByteString b = fromChunks (byteStringToChunk <$> LB.toChunks b)

size :: Bytes -> Int
size = R.size . underlying

chunkSize :: Chunk -> Int
chunkSize = V.length

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
flatten b = snoc mempty (V.concat (chunks b))

take :: Int -> Bytes -> Bytes
take n (Bytes bs) = Bytes (R.take n bs)

drop :: Int -> Bytes -> Bytes
drop n (Bytes bs) = Bytes (R.drop n bs)

at, index :: Int -> Bytes -> Maybe Word8
at n (Bytes bs) = R.index n bs
index = at 

dropBlock :: Int -> Bytes -> Maybe (Chunk, Bytes)
dropBlock nBytes (Bytes chunks) = go mempty chunks
  where
  go acc chunks 
    | V.length acc == nBytes = Just (acc, Bytes chunks)
    | V.length acc >= nBytes, (hd,hd2) <- V.splitAt nBytes acc = Just (hd, Bytes (hd2 `R.cons` chunks))
    | Just (head, tail) <- R.uncons chunks = go (acc <> head) tail
    | otherwise = Nothing 

decodeNat64be :: Bytes -> Maybe (Word64, Bytes)
decodeNat64be bs = case dropBlock 8 bs of
  Just (head, rest) ->
    let
      b8 = V.unsafeIndex head 0
      b7 = V.unsafeIndex head 1
      b6 = V.unsafeIndex head 2
      b5 = V.unsafeIndex head 3
      b4 = V.unsafeIndex head 4
      b3 = V.unsafeIndex head 5
      b2 = V.unsafeIndex head 6
      b1 = V.unsafeIndex head 7
      b = shiftL (fromIntegral b8) 56
       .|.shiftL (fromIntegral b7) 48
       .|.shiftL (fromIntegral b6) 40
       .|.shiftL (fromIntegral b5) 32
       .|.shiftL (fromIntegral b4) 24
       .|.shiftL (fromIntegral b3) 16
       .|.shiftL (fromIntegral b2) 8
       .|.fromIntegral b1
    in
      Just(b, rest)
  Nothing -> Nothing

decodeNat64le :: Bytes -> Maybe (Word64, Bytes)
decodeNat64le bs = case dropBlock 8 bs of
  Just (head, rest) ->
    let
      b1 = V.unsafeIndex head 0
      b2 = V.unsafeIndex head 1
      b3 = V.unsafeIndex head 2
      b4 = V.unsafeIndex head 3
      b5 = V.unsafeIndex head 4
      b6 = V.unsafeIndex head 5
      b7 = V.unsafeIndex head 6
      b8 = V.unsafeIndex head 7
      b =  shiftL (fromIntegral b8) 56
       .|. shiftL (fromIntegral b7) 48
       .|. shiftL (fromIntegral b6) 40
       .|. shiftL (fromIntegral b5) 32
       .|. shiftL (fromIntegral b4) 24
       .|. shiftL (fromIntegral b3) 16
       .|. shiftL (fromIntegral b2) 8
       .|. fromIntegral b1
    in
      Just(b, rest)
  Nothing -> Nothing

decodeNat32be :: Bytes -> Maybe (Word64, Bytes)
decodeNat32be bs = case dropBlock 4 bs of
  Just (head, rest) ->
    let
      b4 = V.unsafeIndex head 0
      b3 = V.unsafeIndex head 1
      b2 = V.unsafeIndex head 2
      b1 = V.unsafeIndex head 3
      b =  shiftL (fromIntegral b4) 24
       .|. shiftL (fromIntegral b3) 16
       .|. shiftL (fromIntegral b2) 8
       .|. fromIntegral b1
    in
      Just(b, rest)
  Nothing -> Nothing

decodeNat32le :: Bytes -> Maybe (Word64, Bytes)
decodeNat32le bs = case dropBlock 4 bs of
  Just (head, rest) ->
    let
      b1 = V.unsafeIndex head 0
      b2 = V.unsafeIndex head 1
      b3 = V.unsafeIndex head 2
      b4 = V.unsafeIndex head 3
      b =  shiftL (fromIntegral b4) 24
       .|. shiftL (fromIntegral b3) 16
       .|. shiftL (fromIntegral b2) 8
       .|. fromIntegral b1
    in
      Just(b, rest)
  Nothing -> Nothing

decodeNat16be :: Bytes -> Maybe (Word64, Bytes)
decodeNat16be bs = case dropBlock 2 bs of
  Just (head, rest) ->
    let
      b2 = V.unsafeIndex head 0
      b1 = V.unsafeIndex head 1
      b =  shiftL (fromIntegral b2) 8
       .|. fromIntegral b1
    in
      Just(b, rest)
  Nothing -> Nothing

decodeNat16le :: Bytes -> Maybe (Word64, Bytes)
decodeNat16le bs = case dropBlock 2 bs of
  Just (head, rest) ->
    let
      b1 = V.unsafeIndex head 0
      b2 = V.unsafeIndex head 1
      b =  shiftL (fromIntegral b2) 8
       .|. fromIntegral b1
    in
      Just(b, rest)
  Nothing -> Nothing


fillBE :: Word64 -> Int -> Int -> Word8
fillBE n k 0 = fromIntegral (shiftR n (k*8))
fillBE n k i = fromIntegral (shiftR n ((k-i) * 8))
{-# inline fillBE #-}

encodeNat64be :: Word64 -> Bytes
encodeNat64be n = Bytes (R.one (V.generate 8 (fillBE n 7)))

encodeNat32be :: Word64 -> Bytes
encodeNat32be n = Bytes (R.one (V.generate 4 (fillBE n 3)))

encodeNat16be :: Word64 -> Bytes
encodeNat16be n = Bytes (R.one (V.generate 2 (fillBE n 1)))

fillLE :: Word64 -> Int -> Word8
fillLE n i = fromIntegral (shiftR n (i*8))
{-# inline fillLE #-}

encodeNat64le :: Word64 -> Bytes
encodeNat64le n = Bytes (R.one (V.generate 8 (fillLE n)))

encodeNat32le :: Word64 -> Bytes
encodeNat32le n = Bytes (R.one (V.generate 4 (fillLE n)))

encodeNat16le :: Word64 -> Bytes
encodeNat16le n = Bytes (R.one (V.generate 2 (fillLE n)))

toBase16 :: Bytes -> Bytes
toBase16 bs = foldl' step empty (chunks bs) where
  step bs b = snoc bs (arrayToChunk @BA.Bytes $
    BE.convertToBase BE.Base16 (chunkToArray @BA.Bytes b))

chunkToArray, arrayFromChunk :: BA.ByteArray b => Chunk -> b
chunkToArray bs = BA.allocAndFreeze (V.length bs) $ \ptr ->
  let
    go !ind =
      if ind < V.length bs
      then pokeByteOff ptr ind (V.unsafeIndex bs ind) >> go (ind+1)
      else pure ()
  in go 0

arrayFromChunk = chunkToArray

arrayToChunk, chunkFromArray :: BA.ByteArrayAccess b => b -> Chunk
arrayToChunk bs = V.generate (BA.length bs) (BA.index bs)
chunkFromArray = arrayToChunk

fromBase16 :: Bytes -> Either Text.Text Bytes
fromBase16 bs = case traverse convert (chunks bs) of
  Left e -> Left (Text.pack e)
  Right bs -> Right (fromChunks bs)
  where
    convert b = BE.convertFromBase BE.Base16 (chunkToArray @BA.Bytes b)
            <&> arrayToChunk @BA.Bytes

toBase32, toBase64, toBase64UrlUnpadded :: Bytes -> Bytes
toBase32 = toBase BE.Base32
toBase64 = toBase BE.Base64
toBase64UrlUnpadded = toBase BE.Base64URLUnpadded

fromBase32, fromBase64, fromBase64UrlUnpadded :: Bytes -> Either Text.Text Bytes
fromBase32 = fromBase BE.Base32
fromBase64 = fromBase BE.Base64
fromBase64UrlUnpadded = fromBase BE.Base64URLUnpadded

fromBase :: BE.Base -> Bytes -> Either Text.Text Bytes
fromBase e (Bytes bs) = case BE.convertFromBase e (chunkToArray @BA.Bytes $ R.flatten bs) of
  Left e -> Left (Text.pack e)
  Right b -> Right $ snoc empty (chunkFromArray (b :: BA.Bytes))

toBase :: BE.Base -> Bytes -> Bytes
toBase e (Bytes bs) = snoc empty (arrayToChunk arr)
  where
  arr :: BA.Bytes
  arr = BE.convertToBase e (chunkToArray @BA.Bytes $ R.flatten bs)

toWord8s :: Bytes -> [Word8]
toWord8s bs = chunks bs >>= V.toList

fromWord8s :: [Word8] -> Bytes
fromWord8s bs = snoc empty (V.fromList bs)

instance Show Bytes where
  show bs = toWord8s (toBase16 bs) >>= \w -> [chr (fromIntegral w)]
