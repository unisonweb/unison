{-# LANGUAGE CPP #-}

-- | Bundles: a self-contained executable that is unisonrt and a compiled
-- program (unisonrt/README.md, "Bundles"). ucm's `bundle` command and
-- `unisonrt bundle` write them; unisonrt, finding a program in its own
-- executable, runs it.
--
-- The program goes where the executable's format allows:
--
-- * Mach-O (macOS): in the section @__UNISON,__uc@, inside the image, so
--   that the code signature covers it ("Unison.Runtime.Bundle.MachO"). The
--   section holds 'bundleMagic', the program's length (8 bytes,
--   little-endian) and the program; in a plain unisonrt it holds 16 zero
--   bytes (unisonrt/bundle-section).
-- * Anything else (ELF, PE): after the end of the executable, followed by a
--   16-byte trailer, the program's offset (8 bytes, little-endian) and
--   'bundleMagic'. The loaders ignore bytes after the end of the file's
--   contents.
--
-- unisonrt's main.c tells a bundle apart the same way, to pass every
-- argument to the program.
module Unison.Runtime.Bundle
  ( readBundle,
    bundledProgram,
    writeBundle,
    signExecutable,
    makeExecutable,
  )
where

import Data.Bits (shiftL, shiftR)
import Data.ByteString qualified as B
import Data.Word (Word64)
import Unison.Runtime.Bundle.MachO qualified as MachO
#if defined(darwin_HOST_OS)
import Data.ByteString.Unsafe qualified as BU
import Data.Word (Word8)
import Foreign.C.Types (CSize (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
#else
import System.IO (IOMode (ReadMode), SeekMode (AbsoluteSeek), hFileSize, hSeek, withBinaryFile)
#endif
#if defined(mingw32_HOST_OS)
import System.FilePath (takeExtension, (<.>))
#else
import System.Posix.Files (setFileMode)
#endif

bundleMagic :: B.ByteString
bundleMagic = "UNISONRT"

-- | The size of the trailer, and of the head of the Mach-O section
trailerSize :: Int
trailerSize = 16

-- | The program in the bytes of an executable, if it is a bundle
bundledProgram :: B.ByteString -> Maybe B.ByteString
bundledProgram bytes
  | MachO.isMachO bytes = MachO.programSection bytes >>= fromSection
  | otherwise = snd <$> fromTrailer bytes

-- | The bytes of a bundle of the program, from the bytes of a unisonrt (a
-- bundle or not). A Mach-O result has to be signed again
-- ('signExecutable').
bundleBytes :: B.ByteString -> B.ByteString -> Either String B.ByteString
bundleBytes runtime program
  | MachO.isMachO runtime = MachO.setProgramSection runtime (toSection program)
  | otherwise = Right (plain <> program <> toLittleEndian (fromIntegral (B.length plain)) <> bundleMagic)
  where
    -- without the program of its own, if it is a bundle
    plain = maybe runtime ((`B.take` runtime) . fst) (fromTrailer runtime)

-- | The Mach-O section's contents for a program
toSection :: B.ByteString -> B.ByteString
toSection program = bundleMagic <> toLittleEndian (fromIntegral (B.length program)) <> program

-- | A bundle's program, from the Mach-O section's contents
fromSection :: B.ByteString -> Maybe B.ByteString
fromSection section
  | magic == bundleMagic, len <= B.length rest - 8 = Just (B.take len (B.drop 8 rest))
  | otherwise = Nothing
  where
    (magic, rest) = B.splitAt 8 section
    len = fromIntegral (fromLittleEndian (B.take 8 rest))

-- | Where the program starts and the program, from the trailer at the end
-- of the bytes
fromTrailer :: B.ByteString -> Maybe (Int, B.ByteString)
fromTrailer bytes
  | B.length bytes >= trailerSize,
    (off, magic) <- B.splitAt 8 (B.drop (B.length bytes - trailerSize) bytes),
    magic == bundleMagic,
    start <- fromIntegral (fromLittleEndian off),
    start <= B.length bytes - trailerSize =
      Just (start, B.take (B.length bytes - trailerSize - start) (B.drop start bytes))
  | otherwise = Nothing

-- | Writes a bundle of the program, from the file of a unisonrt (a bundle
-- or not), as an executable that anyone can run, and says where it wrote
-- it: on Windows a name without an extension gets @.exe@.
writeBundle :: FilePath -> B.ByteString -> FilePath -> IO (Either String FilePath)
writeBundle runtime program out0 = do
  bytes <- B.readFile runtime
  case bundleBytes bytes program of
    Left err -> pure (Left err)
    Right bundle -> do
      B.writeFile out bundle
      makeExecutable out
      fmap (const out) <$> signExecutable out
  where
#if defined(mingw32_HOST_OS)
    out = if null (takeExtension out0) then out0 <.> "exe" else out0
#else
    out = out0
#endif

-- | Lets everyone run it, as the linker leaves an executable
makeExecutable :: FilePath -> IO ()
#if defined(mingw32_HOST_OS)
makeExecutable _ = pure ()
#else
makeExecutable out = setFileMode out 0o755
#endif

-- | Signs a rewritten executable again, ad hoc: macOS on arm64 runs
-- nothing whose signature doesn't match it. It can be signed again with a
-- real identity afterwards. Elsewhere there is no signature.
signExecutable :: FilePath -> IO (Either String ())
#if defined(darwin_HOST_OS)
signExecutable out = do
  (code, _, err) <- readProcessWithExitCode "codesign" ["--sign", "-", "--force", out] ""
  pure $ case code of
    ExitSuccess -> Right ()
    ExitFailure _ -> Left ("codesign failed: " ++ err)
#else
signExecutable _ = pure (Right ())
#endif

-- | The program this running executable carries, if it is a bundle. On
-- macOS the section is in memory already (cbits/bundle.c); elsewhere only
-- the trailer and the program are read from the file at this path.
readBundle :: FilePath -> IO (Maybe B.ByteString)
#if defined(darwin_HOST_OS)
readBundle _ = alloca \sizePtr -> do
  p <- c_bundleSection sizePtr
  size <- fromIntegral <$> peek sizePtr
  if p == nullPtr || size < trailerSize
    then pure Nothing
    else -- copied, so that nothing points into the image
      fromSection . B.copy <$> BU.unsafePackCStringLen (castPtr p, size)

foreign import ccall unsafe "unisonrt_bundle_section"
  c_bundleSection :: Ptr CSize -> IO (Ptr Word8)
#else
readBundle self = withBinaryFile self ReadMode \h -> do
  size <- hFileSize h
  if size < fromIntegral trailerSize
    then pure Nothing
    else do
      hSeek h AbsoluteSeek (size - fromIntegral trailerSize)
      (off, magic) <- B.splitAt 8 <$> B.hGet h trailerSize
      let start = toInteger (fromLittleEndian off)
      if magic /= bundleMagic || start > size - fromIntegral trailerSize
        then pure Nothing
        else do
          hSeek h AbsoluteSeek start
          Just <$> B.hGet h (fromIntegral (size - fromIntegral trailerSize - start))
#endif

toLittleEndian :: Word64 -> B.ByteString
toLittleEndian n = B.pack [fromIntegral (n `shiftR` (8 * k)) | k <- [0 .. 7]]

fromLittleEndian :: B.ByteString -> Word64
fromLittleEndian = B.foldr' (\b acc -> acc `shiftL` 8 + fromIntegral b) 0
