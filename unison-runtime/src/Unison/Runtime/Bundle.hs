{-# LANGUAGE CPP #-}

-- | Bundles: a self-contained executable that is unisonrt and a compiled
-- program (unisonrt/README.md, "Bundles").
--
-- On macOS the program is the contents of the section @__UNISON,__uc@, in
-- a segment of its own just before @__LINKEDIT@. unisonrt is linked with
-- the section holding 16 zero bytes (unisonrt/bundle-section); a bundle's
-- holds 'bundleMagic', the program's length (8 bytes, little-endian) and
-- the program. Being part of the image, the program is covered by the code
-- signature, and the program reads it from memory.
--
-- Elsewhere the program follows the executable, with a 16-byte trailer:
-- the program's offset (8 bytes, little-endian), then 'bundleMagic'.
--
-- unisonrt's main.c tells a bundle apart the same way, to pass every
-- argument to the program; the section is found by unisonrt_bundle_section
-- (cbits/bundle.c).
module Unison.Runtime.Bundle
  ( readBundle,
    writeBundle,
    signExecutable,
    makeExecutable,
  )
where

import Data.Bits (shiftL, shiftR)
import Data.ByteString qualified as B
import Data.Word (Word64)
#if !defined(mingw32_HOST_OS)
import System.Posix.Files (setFileMode)
#endif
#if defined(darwin_HOST_OS)
import Control.Monad (unless, when)
import Data.Bits ((.&.))
import Data.ByteString.Unsafe qualified as BU
import Data.List (find, sortOn)
import Data.Word (Word32, Word8)
import Foreign.C.Types (CSize (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
#else
import System.IO (IOMode (ReadMode), SeekMode (AbsoluteSeek), hFileSize, hSeek, withBinaryFile)
#endif

bundleMagic :: B.ByteString
bundleMagic = "UNISONRT"

-- | The program this executable carries, if it is a bundle.
readBundle :: FilePath -> IO (Maybe B.ByteString)

-- | The bytes of a bundle of the given program, from the bytes of this
-- executable (a bundle or not). On macOS the result has to be signed
-- again, which 'writeBundle' does.
bundleBytes :: B.ByteString -> B.ByteString -> Either String B.ByteString

-- | Writes a bundle of the program, from the file of a unisonrt (a
-- bundle or not), as an executable that anyone can run.
writeBundle :: FilePath -> B.ByteString -> FilePath -> IO (Either String ())
writeBundle runtime program out = do
  this <- B.readFile runtime
  case bundleBytes this program of
    Left err -> pure (Left err)
    Right bytes -> do
      B.writeFile out bytes
      makeExecutable out
      signExecutable out

-- | Lets everyone run it, as the linker leaves an executable
makeExecutable :: FilePath -> IO ()
#if defined(mingw32_HOST_OS)
makeExecutable _ = pure ()
#else
makeExecutable out = setFileMode out 0o755
#endif

littleEndian :: Word64 -> B.ByteString
littleEndian n = B.pack [fromIntegral (n `shiftR` (8 * k)) | k <- [0 .. 7]]

fromLittleEndian :: B.ByteString -> Word64
fromLittleEndian = B.foldr' (\b acc -> acc `shiftL` 8 + fromIntegral b) 0

#if defined(darwin_HOST_OS)

foreign import ccall unsafe "unisonrt_bundle_section"
  c_bundleSection :: Ptr CSize -> IO (Ptr Word8)

readBundle _ = alloca \sizePtr -> do
  p <- c_bundleSection sizePtr
  size <- fromIntegral <$> peek sizePtr
  if p == nullPtr || size < 16
    then pure Nothing
    else do
      -- the image's own memory: copied, so that nothing points into it
      section <- B.copy <$> BU.unsafePackCStringLen (castPtr p, size)
      let (magic, rest) = B.splitAt 8 section
          len = fromIntegral (fromLittleEndian (B.take 8 rest))
      pure $
        if magic == bundleMagic && len <= B.length rest - 8
          then Just (B.take len (B.drop 8 rest))
          else Nothing

-- | Re-signs a rewritten executable, ad hoc: macOS on arm64 runs nothing
-- whose signature doesn't match it. It can be signed again with a real
-- identity afterwards. Elsewhere, nothing to do.
signExecutable :: FilePath -> IO (Either String ())
signExecutable out = do
  (code, _, err) <- readProcessWithExitCode "codesign" ["--sign", "-", "--force", out] ""
  pure $ case code of
    ExitSuccess -> Right ()
    ExitFailure _ -> Left ("codesign failed: " ++ err)

-- The Mach-O layout this needs: 64-bit, the section's segment right before
-- __LINKEDIT, which is last in the file and in memory. Growing the section
-- moves __LINKEDIT, in the file and in memory, by whole pages; nothing
-- refers to __LINKEDIT by address, and nothing before it moves.

data Segment = Segment
  { segName :: B.ByteString,
    -- | where its load command is
    segCmd :: Int,
    segVmAddr, segVmSize, segFileOff, segFileSize :: Word64,
    segSections :: [(B.ByteString, Int)]
  }

pageSize :: Word64
pageSize = 0x4000

bundleBytes this program = do
  unless (u32 0 == 0xfeedfacf) (Left "not a 64-bit Mach-O executable")
  let segments = [segment at | (at, cmd) <- commands, cmd == lcSegment64]
  uni <- maybe (Left "no __UNISON segment: this unisonrt was linked without one") Right (find ((== "__UNISON") . segName) segments)
  le <- maybe (Left "no __LINKEDIT segment") Right (find ((== "__LINKEDIT") . segName) segments)
  sect <- maybe (Left "no __UNISON,__uc section") Right (lookup "__uc" (segSections uni))
  let ordered = sortOn segFileOff [s | s <- segments, segFileSize s > 0]
  when (map segName (drop (length ordered - 2) ordered) /= ["__UNISON", "__LINKEDIT"]) $
    Left "the __UNISON segment is not the one before __LINKEDIT"
  when (any (\s -> segName s /= "__LINKEDIT" && segVmAddr s > segVmAddr uni) segments) $
    Left "the __UNISON segment is not the last before __LINKEDIT in memory"
  let content = bundleMagic <> littleEndian (fromIntegral (B.length program)) <> program
      sectOff = fromIntegral (u32 (sect + 48)) - segFileOff uni
      newSize = roundUp (sectOff + fromIntegral (B.length content))
      newLeOff = segFileOff uni + newSize
      newLeAddr = segVmAddr uni + newSize
      delta = fromIntegral newLeOff - fromIntegral (segFileOff le) :: Integer
      shift at = put32 at (fromIntegral (toInteger (u32 at) + delta))
      shiftNonZero at = [shift at | u32 at /= 0]
      patches =
        [ put64 (segCmd uni + 32) newSize,
          put64 (segCmd uni + 48) newSize,
          put64 (sect + 40) (fromIntegral (B.length content)),
          put64 (segCmd le + 24) newLeAddr,
          put64 (segCmd le + 40) newLeOff
        ]
          ++ concat [linkEditOffsets at cmd | (at, cmd) <- commands]
      linkEditOffsets at cmd
        | cmd == 0x2 = shiftNonZero (at + 8) ++ shiftNonZero (at + 16)
        | cmd == 0xb = concatMap (shiftNonZero . (at +)) [32, 40, 48, 56, 64, 72]
        | cmd == 0x22 || cmd == 0x80000022 = concatMap (shiftNonZero . (at +)) [8, 16, 24, 32, 40]
        | cmd `elem` linkEditData = shiftNonZero (at + 8)
        | otherwise = []
      header = foldl (\bs f -> f bs) (B.take (fromIntegral (segFileOff uni)) this) patches
      segmentBytes =
        B.take (fromIntegral sectOff) (B.drop (fromIntegral (segFileOff uni)) this)
          <> content
          <> B.replicate (fromIntegral newSize - fromIntegral sectOff - B.length content) 0
  pure (header <> segmentBytes <> B.drop (fromIntegral (segFileOff le)) this)
  where
    u32 :: Int -> Word32
    u32 at = fromIntegral (fromLittleEndian (B.take 4 (B.drop at this)))
    u64 :: Int -> Word64
    u64 at = fromLittleEndian (B.take 8 (B.drop at this))
    lcSegment64 = 0x19
    -- the linkedit_data_command load commands
    linkEditData = [0x1d, 0x1e, 0x26, 0x29, 0x2b, 0x2e, 0x80000033, 0x80000034]
    commands = go 32 (u32 16 :: Word32)
      where
        go _ 0 = []
        go at n = (at, u32 at) : go (at + fromIntegral (u32 (at + 4))) (n - 1)
    segment at =
      Segment
        { segName = B.takeWhile (/= 0) (B.take 16 (B.drop (at + 8) this)),
          segCmd = at,
          segVmAddr = u64 (at + 24),
          segVmSize = u64 (at + 32),
          segFileOff = u64 (at + 40),
          segFileSize = u64 (at + 48),
          segSections =
            [ (B.takeWhile (/= 0) (B.take 16 (B.drop s this)), s)
            | k <- [0 .. fromIntegral (u32 (at + 64)) - 1],
              let s = at + 72 + 80 * k
            ]
        }
    roundUp n = (n + pageSize - 1) .&. negate pageSize
    put32 at (v :: Word32) bs = B.take at bs <> B.take 4 (littleEndian (fromIntegral v)) <> B.drop (at + 4) bs
    put64 at (v :: Word64) bs = B.take at bs <> littleEndian v <> B.drop (at + 8) bs

#else

trailerSize :: Int
trailerSize = 16

readBundle self = withBinaryFile self ReadMode \h -> do
  size <- hFileSize h
  if size < fromIntegral trailerSize
    then pure Nothing
    else do
      hSeek h AbsoluteSeek (size - fromIntegral trailerSize)
      trailer <- B.hGet h trailerSize
      let (off, magic) = B.splitAt 8 trailer
          start = toInteger (fromLittleEndian off)
      if magic /= bundleMagic || start > size - fromIntegral trailerSize
        then pure Nothing
        else do
          hSeek h AbsoluteSeek start
          Just <$> B.hGet h (fromIntegral (size - fromIntegral trailerSize - start))

signExecutable :: FilePath -> IO (Either String ())
signExecutable _ = pure (Right ())

bundleBytes this program = Right (runtime <> program <> littleEndian (fromIntegral (B.length runtime)) <> bundleMagic)
  where
    -- this executable, without a program of its own if it is a bundle
    runtime
      | B.length this >= 16,
        (off, magic) <- B.splitAt 8 (B.drop (B.length this - 16) this),
        magic == bundleMagic,
        start <- fromIntegral (fromLittleEndian off),
        start <= B.length this - 16 =
          B.take start this
      | otherwise = this

#endif
