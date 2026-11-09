-- Optimized in every build, --fast too: minifying is slow without it
{-# OPTIONS_GHC -O2 #-}

-- | Little-endian reads from a strict ByteString, and the builders the
-- writers use.
module Minify.Bytes
  ( u8,
    u16,
    u32,
    u64,
    i32,
    i64,
    putU16,
    putU32,
    putU64,
    uleb,
    sleb,
    getUleb,
    getSleb,
    cstringAt,
  )
where

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Unsafe as BU
import Data.Int
import Data.Word

u8 :: B.ByteString -> Int -> Word8
u8 = BU.unsafeIndex
{-# INLINE u8 #-}

u16 :: B.ByteString -> Int -> Word16
u16 b i = fromIntegral (u8 b i) .|. (fromIntegral (u8 b (i + 1)) `shiftL` 8)
{-# INLINE u16 #-}

u32 :: B.ByteString -> Int -> Word32
u32 b i =
  fromIntegral (u8 b i)
    .|. (fromIntegral (u8 b (i + 1)) `shiftL` 8)
    .|. (fromIntegral (u8 b (i + 2)) `shiftL` 16)
    .|. (fromIntegral (u8 b (i + 3)) `shiftL` 24)
{-# INLINE u32 #-}

u64 :: B.ByteString -> Int -> Word64
u64 b i = fromIntegral (u32 b i) .|. (fromIntegral (u32 b (i + 4)) `shiftL` 32)
{-# INLINE u64 #-}

i32 :: B.ByteString -> Int -> Int32
i32 b i = fromIntegral (u32 b i)
{-# INLINE i32 #-}

i64 :: B.ByteString -> Int -> Int64
i64 b i = fromIntegral (u64 b i)
{-# INLINE i64 #-}

putU16 :: Word16 -> BB.Builder
putU16 = BB.word16LE

putU32 :: Word32 -> BB.Builder
putU32 = BB.word32LE

putU64 :: Word64 -> BB.Builder
putU64 = BB.word64LE

uleb :: Word64 -> BB.Builder
uleb n
  | n < 0x80 = BB.word8 (fromIntegral n)
  | otherwise = BB.word8 (fromIntegral (n .&. 0x7f) .|. 0x80) <> uleb (n `shiftR` 7)

sleb :: Int64 -> BB.Builder
sleb n =
  let byte = fromIntegral (n .&. 0x7f) :: Word8
      rest = n `shiftR` 7
      done = (rest == 0 && byte .&. 0x40 == 0) || (rest == -1 && byte .&. 0x40 /= 0)
   in if done then BB.word8 byte else BB.word8 (byte .|. 0x80) <> sleb rest

-- | Reads a ULEB128 at an offset: the value and the offset after it.
getUleb :: B.ByteString -> Int -> (Word64, Int)
getUleb b = go 0 0
  where
    go !acc !sh i =
      let w = u8 b i
          acc' = acc .|. (fromIntegral (w .&. 0x7f) `shiftL` sh)
       in if w .&. 0x80 == 0 then (acc', i + 1) else go acc' (sh + 7) (i + 1)

getSleb :: B.ByteString -> Int -> (Int64, Int)
getSleb b = go 0 0
  where
    go !acc !sh i =
      let w = u8 b i
          acc' = acc .|. (fromIntegral (w .&. 0x7f) `shiftL` sh)
          sh' = sh + 7
       in if w .&. 0x80 == 0
            then (if w .&. 0x40 /= 0 && sh' < 64 then acc' .|. (complement 0 `shiftL` sh') else acc', i + 1)
            else go acc' sh' (i + 1)

cstringAt :: B.ByteString -> Int -> B.ByteString
cstringAt b i = B.takeWhile (/= 0) (B.drop i b)
