{-# Language ViewPatterns #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.Util.Bytes where

import Data.Bits (shiftR, shiftL, (.|.))
import Data.Char
import Unison.Prelude hiding (ByteString, empty)
import qualified Data.ByteString as B
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BE
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as Text
import qualified Unison.Util.Rope as R
import qualified Data.Vector.Primitive as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.ByteString as BSV
import Foreign.Storable (pokeByteOff)
import qualified Codec.Compression.Zlib as Zlib
import qualified Codec.Compression.GZip as GZip

type ByteString = V.Vector Word8

-- Bytes type represented as a rope of ByteStrings
type Bytes = R.Rope ByteString

instance R.Sized ByteString where size = V.length
instance R.Drop ByteString where drop = V.drop
instance R.Take ByteString where take = V.take
instance R.Index ByteString Word8 where index n bs = bs V.!? n
instance R.Reverse ByteString where reverse = V.reverse

null :: Bytes -> Bool
null = R.null

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

byteStringToChunk :: B.ByteString -> ByteString
byteStringToChunk = fromStorable . BSV.byteStringToVector

chunkToByteString :: ByteString -> B.ByteString
chunkToByteString = BSV.vectorToByteString . toStorable

fromStorable :: (V.Prim a, SV.Storable a) => SV.Vector a -> V.Vector a
fromStorable v =
  -- is there a more efficient implementation of this
  V.generate (SV.length v) (SV.unsafeIndex v)
{-# inline fromStorable #-}

toStorable :: (V.Prim a, SV.Storable a) => V.Vector a -> SV.Vector a
toStorable v =
  -- is there a more efficient implementation of this
  SV.generate (V.length v) (V.unsafeIndex v)
{-# inline toStorable #-}

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
size = R.size

chunkSize :: ByteString -> Int
chunkSize = V.length

chunks :: Bytes -> [ByteString]
chunks = toList

byteStringChunks :: Bytes -> [B.ByteString]
byteStringChunks bs = chunkToByteString <$> chunks bs 

fromChunks :: [ByteString] -> Bytes
fromChunks = foldl' snoc empty

cons :: ByteString -> Bytes -> Bytes
cons = R.cons

snoc :: Bytes -> ByteString -> Bytes
snoc = R.snoc

flatten :: Bytes -> Bytes
flatten b = snoc mempty (V.concat (chunks b))

take :: Int -> Bytes -> Bytes
take = R.take

drop :: Int -> Bytes -> Bytes
drop = R.drop

at, index :: Int -> Bytes -> Maybe Word8
at = R.index
index = R.index

dropBlock :: Int -> Bytes -> Maybe (ByteString, Bytes)
dropBlock nBytes chunks = go mempty chunks
  where
  go acc chunks 
    | V.length acc == nBytes = Just (acc, chunks)
    | V.length acc >= nBytes, (hd,hd2) <- V.splitAt nBytes acc = Just (hd, hd2 `R.cons` chunks)
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
encodeNat64be n = R.one (V.generate 8 (fillBE n 7))

encodeNat32be :: Word64 -> Bytes
encodeNat32be n = R.one (V.generate 4 (fillBE n 3))

encodeNat16be :: Word64 -> Bytes
encodeNat16be n = R.one (V.generate 2 (fillBE n 1))

fillLE :: Word64 -> Int -> Word8
fillLE n i = fromIntegral (shiftR n (i*8))
{-# inline fillLE #-}

encodeNat64le :: Word64 -> Bytes
encodeNat64le n = R.one (V.generate 8 (fillLE n))

encodeNat32le :: Word64 -> Bytes
encodeNat32le n = R.one (V.generate 4 (fillLE n))

encodeNat16le :: Word64 -> Bytes
encodeNat16le n = R.one (V.generate 2 (fillLE n))

toBase16 :: Bytes -> Bytes
toBase16 bs = foldl' step empty (chunks bs) where
  step bs b = snoc bs (arrayToChunk @BA.Bytes $
    BE.convertToBase BE.Base16 (chunkToArray @BA.Bytes b))

chunkToArray, arrayFromChunk :: BA.ByteArray b => ByteString -> b
chunkToArray bs = BA.allocAndFreeze (V.length bs) $ \ptr ->
  let
    go !ind =
      if ind < V.length bs
      then pokeByteOff ptr ind (V.unsafeIndex bs ind) >> go (ind+1)
      else pure ()
  in go 0

arrayFromChunk = chunkToArray

arrayToChunk, chunkFromArray :: BA.ByteArrayAccess b => b -> ByteString
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
fromBase e bs = case BE.convertFromBase e (chunkToArray @BA.Bytes $ R.flatten bs) of
  Left e -> Left (Text.pack e)
  Right b -> Right $ snoc empty (chunkFromArray (b :: BA.Bytes))

toBase :: BE.Base -> Bytes -> Bytes
toBase e bs = snoc empty (arrayToChunk arr)
  where
  arr :: BA.Bytes
  arr = BE.convertToBase e (chunkToArray @BA.Bytes $ R.flatten bs)

toWord8s :: Bytes -> [Word8]
toWord8s bs = chunks bs >>= V.toList

fromWord8s :: [Word8] -> Bytes
fromWord8s bs = snoc empty (V.fromList bs)

instance Show Bytes where
  show bs = toWord8s (toBase16 bs) >>= \w -> [chr (fromIntegral w)]