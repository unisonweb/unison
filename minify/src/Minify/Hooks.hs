-- Optimized in every build, --fast too: minifying is slow without it
{-# OPTIONS_GHC -O2 #-}

-- | What an architecture's decoder needs from the analysis and reports back
-- to it, so that "Minify.Analysis" knows nothing about instructions and the
-- architecture modules know nothing about definitions. One record is built
-- per definition being decoded.
module Minify.Hooks
  ( Hooks (..),
    byteAt,
    bytesAt,
    word32At,
    word64At,
  )
where

import qualified Data.ByteString as B
import Data.Word
import Minify.Bytes
import Minify.Ref

data Hooks = Hooks
  { -- | the whole file
    hkBytes :: B.ByteString,
    -- | the file offset of an address, when the file backs it
    hkOffsetOf :: Word64 -> Maybe Int,
    -- | the definition whose code is decoded
    hkDefStart :: !Word64,
    hkDefEnd :: !Word64,
    hkDefName :: String,
    -- | marks the bytes from an address as decoded; False when they were
    -- already decoded or belong to an info table (decoding stops there)
    hkVisit :: Word64 -> Int -> IO Bool,
    -- | whether the bytes at an address belong to an info table
    hkIsTable :: Word64 -> IO Bool,
    -- | marks bytes as data inside code (a jump table), never to be decoded
    hkMarkData :: Word64 -> Int -> IO (),
    -- | a reference: field address, kind, target, anchor (for the anchored kinds)
    hkRef :: Word64 -> RefKind -> Word64 -> Word64 -> IO (),
    -- | a code label to decode from: address, whether an info table precedes it
    hkLabel :: Word64 -> Bool -> IO (),
    -- | an address in a data section that code refers to (a static closure,
    -- a string, a jump table)
    hkData :: Word64 -> IO (),
    -- | a jump table: its address, entry size and whether entries are
    -- displacements from it (else absolute addresses). Its entries are read
    -- once decoding has settled, so that every table's address is known.
    hkJumpTable :: Word64 -> Int -> Bool -> IO (),
    -- | whether an address lies inside an instruction already decoded (not
    -- at its start): no jump table entry points there
    hkInterior :: Word64 -> IO Bool,
    -- | the nearest jump table known to start after an address (tables of
    -- one procedure are adjacent, and their entries look alike)
    hkNextTable :: Word64 -> IO (Maybe Word64),
    -- | an address fixed code refers to: what it lies in stays put
    hkPin :: String -> Word64 -> IO (),
    -- | the 4 KB page an adrp names, when its uses cannot be followed
    hkPinPage :: Word64 -> IO (),
    hkAnomaly :: String -> IO (),
    -- | the first pass only collects labels; its anomalies are not reported
    hkFirstPass :: IO Bool,
    hkInText :: Word64 -> Bool,
    -- | whether an info table ends at an address (a label with a table)
    hkHasTable :: Word64 -> Bool,
    -- | inside a definition that is Haskell code or data
    hkIsHaskellAt :: Word64 -> Bool,
    -- | inside a section the file backs
    hkInImage :: Word64 -> Bool,
    -- | the executable is position independent: no instruction carries an absolute address
    hkPie :: !Bool,
    -- | whether a 32-bit field at an address may hold an absolute address:
    -- always, unless the link kept its relocations, which then decide
    hkAbsField :: Word64 -> Bool
  }

byteAt :: Hooks -> Word64 -> Word8
byteAt hk a = case hkOffsetOf hk a of
  Just o | o < B.length (hkBytes hk) -> u8 (hkBytes hk) o
  _ -> 0

-- | Up to n bytes at an address (fewer at the end of the file's backing).
bytesAt :: Hooks -> Word64 -> Int -> B.ByteString
bytesAt hk a n = case hkOffsetOf hk a of
  Just o -> B.take n (B.drop o (hkBytes hk))
  Nothing -> B.empty

word32At :: Hooks -> Word64 -> Word32
word32At hk a = case hkOffsetOf hk a of
  Just o | o + 4 <= B.length (hkBytes hk) -> u32 (hkBytes hk) o
  _ -> 0

word64At :: Hooks -> Word64 -> Word64
word64At hk a = case hkOffsetOf hk a of
  Just o | o + 8 <= B.length (hkBytes hk) -> u64 (hkBytes hk) o
  _ -> 0
