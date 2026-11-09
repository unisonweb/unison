-- | A bundle's program inside a Mach-O executable (macOS), in the section
-- @__UNISON,__uc@ (see "Unison.Runtime.Bundle").
--
-- A Mach-O executable is a header, a list of load commands, and the bytes
-- of its segments, in order. Each segment's load command says where its
-- bytes are in the file (@fileoff@, @filesize@) and in memory (@vmaddr@,
-- @vmsize@), and lists its sections. The last segment is always
-- @__LINKEDIT@: the symbol table, the dynamic loader's information and the
-- code signature, which other load commands point into by file offset.
--
-- unisonrt is linked with @-sectcreate __UNISON __uc@, so that the segment
-- @__UNISON@, holding that one section, is the segment right before
-- @__LINKEDIT@, in the file and in memory. That makes the section the one
-- place that can grow without moving any code or data: growing it moves
-- only @__LINKEDIT@, which no code refers to by address, and the load
-- commands say where @__LINKEDIT@ and everything in it now is. Since the
-- program is then part of the image, the code signature covers it, once
-- the executable is signed again.
--
-- Field offsets are from @<mach-o/loader.h>@.
module Unison.Runtime.Bundle.MachO
  ( isMachO,
    programSection,
    setProgramSection,
  )
where

import Control.Monad (unless, when)
import Data.Bits (shiftL, shiftR, (.&.))
import Data.ByteString qualified as B
import Data.List (find, sortOn)
import Data.Word (Word32, Word64)

-- | Whether these are the bytes of a 64-bit little-endian Mach-O file
-- (arm64 and x86-64 macOS)
isMachO :: B.ByteString -> Bool
isMachO bytes = B.length bytes >= headerSize && u32 bytes 0 == mhMagic64

-- | The contents of the section @__UNISON,__uc@, if the file has it
programSection :: B.ByteString -> Maybe B.ByteString
programSection bytes = do
  (_, sect) <- findSection bytes
  let off = fromIntegral (u32 bytes (sect + sectionOffset))
      size = fromIntegral (u64 bytes (sect + sectionSize))
  pure (B.take size (B.drop off bytes))

-- | The file with the section @__UNISON,__uc@ holding these contents
-- instead. Its code signature no longer matches; the caller signs it
-- again. The steps:
--
-- 1. Find the segment @__UNISON@ and its section, and check that the
--    segment is the one right before @__LINKEDIT@, in the file and in
--    memory.
-- 2. The segment's new size: from its start to the end of the new
--    contents, in whole pages, since segments are mapped by page.
-- 3. @__LINKEDIT@ moves to just after it, in the file and in memory, by
--    the change in that size.
-- 4. The load commands are patched: the segment's sizes and the section's,
--    @__LINKEDIT@'s place, and every file offset another load command holds
--    into @__LINKEDIT@ ('linkEditOffsetFields').
-- 5. The new file is the bytes before the segment (with the patched load
--    commands, which are among them), the segment's new bytes, and
--    @__LINKEDIT@'s bytes, unchanged.
setProgramSection :: B.ByteString -> B.ByteString -> Either String B.ByteString
setProgramSection bytes contents = do
  -- 1.
  unless (isMachO bytes) (Left "not a 64-bit Mach-O executable")
  (uni, sect) <- maybe (Left "no __UNISON,__uc section: this unisonrt was linked without one") Right (findSection bytes)
  linkEdit <- maybe (Left "no __LINKEDIT segment") Right (findSegment "__LINKEDIT" bytes)
  let inFile = sortOn segFileOff [s | s <- segments bytes, segFileSize s > 0]
  when (map segName (drop (length inFile - 2) inFile) /= ["__UNISON", "__LINKEDIT"]) $
    Left "the __UNISON segment is not the one before __LINKEDIT in the file"
  when (any (\s -> segName s /= "__LINKEDIT" && segVmAddr s > segVmAddr uni) (segments bytes)) $
    Left "the __UNISON segment is not the one before __LINKEDIT in memory"
  let -- 2. the section's offset within its segment, then the segment's size
      sectStart = fromIntegral (u32 bytes (sect + sectionOffset)) - segFileOff uni
      newSize = roundUpToPage (sectStart + fromIntegral (B.length contents))
      -- 3.
      newLinkEditOff = segFileOff uni + newSize
      newLinkEditAddr = segVmAddr uni + newSize
      moved = toInteger newLinkEditOff - toInteger (segFileOff linkEdit)
      -- 4.
      patches =
        [ put64 (segCmd uni + segmentVmSize) newSize,
          put64 (segCmd uni + segmentFileSize) newSize,
          put64 (sect + sectionSize) (fromIntegral (B.length contents)),
          put64 (segCmd linkEdit + segmentVmAddr) newLinkEditAddr,
          put64 (segCmd linkEdit + segmentFileOff) newLinkEditOff
        ]
          ++ [ put32 field (fromIntegral (toInteger old + moved))
             | (at, cmd) <- loadCommands bytes,
               field <- (at +) <$> linkEditOffsetFields cmd,
               -- zero means the command has no such table
               let old = u32 bytes field,
               old /= 0
             ]
      before = foldl (\bs patch -> patch bs) (B.take (fromIntegral (segFileOff uni)) bytes) patches
      -- 5.
      segment =
        B.take (fromIntegral sectStart) (B.drop (fromIntegral (segFileOff uni)) bytes)
          <> contents
          <> B.replicate (fromIntegral newSize - fromIntegral sectStart - B.length contents) 0
  pure (before <> segment <> B.drop (fromIntegral (segFileOff linkEdit)) bytes)

-- | The offsets, within a load command of this type, of its fields that
-- hold a file offset into @__LINKEDIT@
linkEditOffsetFields :: Word32 -> [Int]
linkEditOffsetFields cmd
  -- symoff, stroff
  | cmd == lcSymtab = [8, 16]
  -- tocoff, modtaboff, extrefsymoff, indirectsymoff, extreloff, locreloff
  | cmd == lcDysymtab = [32, 40, 48, 56, 64, 72]
  -- rebase_off, bind_off, weak_bind_off, lazy_bind_off, export_off
  | cmd == lcDyldInfo || cmd == lcDyldInfoOnly = [8, 16, 24, 32, 40]
  -- dataoff, of a linkedit_data_command
  | cmd `elem` linkEditDataCommands = [8]
  | otherwise = []

-- * The format

data Segment = Segment
  { segName :: B.ByteString,
    -- | where its load command is in the file
    segCmd :: Int,
    segVmAddr :: Word64,
    segFileOff :: Word64,
    segFileSize :: Word64,
    -- | its sections: name, and where the section's header is in the file
    segSections :: [(B.ByteString, Int)]
  }

-- | The segment @__UNISON@ and where its section @__uc@'s header is
findSection :: B.ByteString -> Maybe (Segment, Int)
findSection bytes = do
  uni <- findSegment "__UNISON" bytes
  sect <- lookup "__uc" (segSections uni)
  pure (uni, sect)

findSegment :: B.ByteString -> B.ByteString -> Maybe Segment
findSegment name bytes = find ((== name) . segName) (segments bytes)

segments :: B.ByteString -> [Segment]
segments bytes = [segmentAt bytes at | isMachO bytes, (at, cmd) <- loadCommands bytes, cmd == lcSegment64]

segmentAt :: B.ByteString -> Int -> Segment
segmentAt bytes at =
  Segment
    { segName = name (at + 8),
      segCmd = at,
      segVmAddr = u64 bytes (at + segmentVmAddr),
      segFileOff = u64 bytes (at + segmentFileOff),
      segFileSize = u64 bytes (at + segmentFileSize),
      segSections =
        [ (name sect, sect)
        | k <- [0 .. fromIntegral (u32 bytes (at + segmentNSects)) - 1],
          let sect = at + segmentCommandSize + sectionHeaderSize * k
        ]
    }
  where
    name field = B.takeWhile (/= 0) (B.take 16 (B.drop field bytes))

-- | Each load command: where it is in the file, and its type. They follow
-- the header, as many as the header's @ncmds@, each @cmdsize@ long.
loadCommands :: B.ByteString -> [(Int, Word32)]
loadCommands bytes = go headerSize (u32 bytes 16)
  where
    go :: Int -> Word32 -> [(Int, Word32)]
    go _ 0 = []
    go at n = (at, u32 bytes at) : go (at + fromIntegral (u32 bytes (at + 4))) (n - 1)

-- | Segments are mapped by page; 16 KB covers arm64 and x86-64
pageSize :: Word64
pageSize = 0x4000

roundUpToPage :: Word64 -> Word64
roundUpToPage n = (n + pageSize - 1) .&. negate pageSize

mhMagic64 :: Word32
mhMagic64 = 0xfeedfacf

headerSize :: Int
headerSize = 32

lcSegment64, lcSymtab, lcDysymtab, lcDyldInfo, lcDyldInfoOnly :: Word32
lcSegment64 = 0x19
lcSymtab = 0x2
lcDysymtab = 0xb
lcDyldInfo = 0x22
lcDyldInfoOnly = 0x80000022

-- | The load commands that are a @linkedit_data_command@: code signature,
-- segment split info, function starts, data in code, dylib code-signing
-- DRs, linker optimization hints, exports trie, chained fixups
linkEditDataCommands :: [Word32]
linkEditDataCommands = [0x1d, 0x1e, 0x26, 0x29, 0x2b, 0x2e, 0x80000033, 0x80000034]

-- | Offsets within a @segment_command_64@, and its size before its sections
segmentVmAddr, segmentVmSize, segmentFileOff, segmentFileSize, segmentNSects, segmentCommandSize :: Int
segmentVmAddr = 24
segmentVmSize = 32
segmentFileOff = 40
segmentFileSize = 48
segmentNSects = 64
segmentCommandSize = 72

-- | Offsets within a @section_64@, and its size
sectionSize, sectionOffset, sectionHeaderSize :: Int
sectionSize = 40
sectionOffset = 48
sectionHeaderSize = 80

-- * Little-endian fields

u32 :: B.ByteString -> Int -> Word32
u32 bytes at = fromIntegral (littleEndian (B.take 4 (B.drop at bytes)))

u64 :: B.ByteString -> Int -> Word64
u64 bytes at = littleEndian (B.take 8 (B.drop at bytes))

littleEndian :: B.ByteString -> Word64
littleEndian = B.foldr' (\b acc -> acc `shiftL` 8 + fromIntegral b) 0

put32 :: Int -> Word32 -> B.ByteString -> B.ByteString
put32 at v bs = B.take at bs <> B.pack [fromIntegral (v `shiftR` (8 * k)) | k <- [0 .. 3]] <> B.drop (at + 4) bs

put64 :: Int -> Word64 -> B.ByteString -> B.ByteString
put64 at v bs = B.take at bs <> B.pack [fromIntegral (v `shiftR` (8 * k)) | k <- [0 .. 7]] <> B.drop (at + 8) bs
