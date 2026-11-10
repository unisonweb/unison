-- Optimized in every build, --fast too: minifying is slow without it
{-# OPTIONS_GHC -O2 #-}

-- | Producing the output file: copying and relocating the kept definitions
-- into the planned layout, then handing the format's writer the file to
-- finish (headers, link-edit data, symbol tables). See design.md, "Layout
-- and rewriting".
module Minify.Rewrite
  ( rewrite,
    Stats (..),
    Choices (..),
  )
where

import Control.Monad
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Data.IORef
import Data.Int
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word
import Foreign.Marshal.Utils (copyBytes, fillBytes)
import Foreign.Ptr
import Foreign.Storable
import Minify.Analysis
import qualified Minify.Arm64 as Arm64
import Minify.Bytes
import qualified Minify.ElfWrite as ElfWrite
import Minify.Format
import Minify.Layout
import qualified Minify.MachOWrite as MachOWrite
import Minify.Ref
import Minify.Relocate
import qualified Minify.X86 as X86

rewrite :: Analysis -> Plan -> Choices -> IO (B.ByteString, Stats)
rewrite an pl choices = do
  let im = anImage an
  -- The format's writer prepares what follows the pieces and how to patch
  -- the headers, before the file is laid out.
  (trailer, finish, stats) <- case imRaw im of
    RawMachOImage mo rebases -> MachOWrite.parts an pl choices mo rebases
    RawElfImage e -> ElfWrite.parts an pl choices e
  let fileSize = fromIntegral (plLinkeditOff pl) + B.length trailer
  out <- BI.create fileSize $ \ptr -> do
    fillBytes ptr 0 fileSize
    fillBody an pl choices ptr
    BU.unsafeUseAsCString trailer $ \t -> copyBytes (ptr `plusPtr` fromIntegral (plLinkeditOff pl)) (castPtr t) (B.length trailer)
    finish ptr
  pure (out, stats {stFileBytes = fileSize})

-- | Fills the pieces: the original bytes of every piece's address range,
-- then the moved definitions at their new places, the rewritten
-- references, the stubs and the islands.
fillBody :: Analysis -> Plan -> Choices -> Ptr Word8 -> IO ()
fillBody an pl Choices {rwZeroFolded = zeroFolded} ptr = do
  let im = anImage an
      orig = imBytes im
      defs = imDefs im
      sects = imSections im
      arch = imArch im
      barf = definedSymbol im (if imFormat im == MachOFormat then "_barf" else "barf")
  errors <- newIORef (0 :: Int)
  BU.unsafeUseAsCString orig $ \src0 -> do
    let src = castPtr src0 :: Ptr Word8
    -- every piece's address range, as it was
    forM_ (plPieces pl) $ \p ->
      forM_ (pieceSpans im p) $ \(a, n, off) ->
        copyBytes (ptr `plusPtr` fromIntegral (pcFileOff p + (a - pcVmStart p))) (src `plusPtr` fromIntegral off) (fromIntegral n)
    -- Within the compacted sections, only kept definitions (at their new
    -- places), the islands and the stubs may hold bytes; everything else
    -- is zeroed, so neither removed definitions nor the old copies of
    -- moved ones survive in the file.
    let live =
          sortOn fst $
            [(plPlacedStart pl VU.! i, plPlacedStart pl VU.! i + (dEnd d - dStart d)) | i <- [0 .. V.length defs - 1], anKept an VU.! i, plPlacedStart pl VU.! i /= 0, not (zeroFolded && anRep an VU.! i /= i), let d = defs V.! i]
        -- the patch areas are zeroed too; the islands and stubs are written over them below
        zeroGaps _ [] = pure ()
        zeroGaps cursor ((a, b) : rest) = do
          when (a > cursor) (zeroRange cursor a)
          zeroGaps (max cursor b) rest
        zeroRange a b = forM_ (plPieces pl) $ \p -> do
          let lo = max a (pcVmStart p)
              hi = min b (pcFileEnd p)
          when (hi > lo) (fillBytes (ptr `plusPtr` fromIntegral (pcFileOff p + (lo - pcVmStart p))) 0 (fromIntegral (hi - lo)))
    forM_ (V.toList sects) $ \sc -> when (isCompactedSection sc) $ do
      let lo = secAddr sc
          hi = secAddr sc + secSize sc
          inside = [(max lo a, min hi b) | (a, b) <- live, b > lo, a < hi]
      zeroGaps lo inside
      case inside of
        [] -> zeroRange lo hi
        _ -> let lastEnd = maximum (map snd inside) in when (lastEnd < hi) (zeroRange lastEnd hi)
    -- kept definitions that moved (a folded one is its representative's bytes)
    forM_ [0 .. V.length defs - 1] $ \i -> when (anKept an VU.! i && plPlacedStart pl VU.! i /= 0 && not (zeroFolded && anRep an VU.! i /= i)) $ do
      let d = defs V.! i
          new = plPlacedStart pl VU.! i
      when (new /= dStart d) $
        case (fileOffsetOf im (dStart d), newFileOffsetOf pl new) of
          (Just o, Just o') -> copyBytes (ptr `plusPtr` fromIntegral o') (src `plusPtr` fromIntegral o) (fromIntegral (dEnd d - dStart d))
          _ -> fail ("no file offset for " ++ hsDisplay d)
    -- references, in every kept definition (a fixed one has only pointer
    -- slots, table fields and jump-table entries, which are rewritten in place)
    forM_ [0 .. V.length defs - 1] $ \i -> when (anKept an VU.! i && plPlacedStart pl VU.! i /= 0) $ do
      let d = defs V.! i
          delta = plPlacedStart pl VU.! i - dStart d
      VU.forM_ (anRefs an V.! i) $ \(at, k, target, anchor) -> do
        let kind = toEnum (fromIntegral k) :: RefKind
            at' = at + delta
            -- constructor names dropped: every con_desc names the empty string
            relocated = if kind == RConDesc && not (anKeepConDesc an) then Right (plEmptyString pl) else relocate an pl target
        case relocated of
          Left e -> do
            modifyIORef' errors (+ 1)
            n <- readIORef errors
            when (n <= 20) (putStrLn ("  " ++ e ++ " (from " ++ hsDisplay d ++ ")"))
          Right t' -> case newFileOffsetOf pl at' of
            Nothing -> fail ("no file offset for a reference in " ++ hsDisplay d)
            Just o -> patch (\t -> M.lookup t (plIslands pl)) ptr (fromIntegral o) at' t' (anchor + delta) kind
    -- loose references: slots outside every definition, in place; a slot
    -- whose target was removed is a GOT entry nothing kept loads, and is zeroed
    VU.forM_ (anLoose an) $ \(at, k, target, anchor) -> do
      let kind = toEnum (fromIntegral k) :: RefKind
      case newFileOffsetOf pl at of
        Nothing -> pure () -- a slot the file does not back (zero fill)
        Just o -> case relocate an pl target of
          Right t' -> patch (\t -> M.lookup t (plIslands pl)) ptr (fromIntegral o) at t' anchor kind
          Left _ -> pokeByteOff ptr (fromIntegral o) (0 :: Word64)
    -- abort stubs for cut definitions
    forM_ (M.toList (plStubs pl)) $ \(i, st) -> do
      let d = defs V.! i
          isData = sbClosure st /= Nothing
      tableOff <- maybe (fail "stub table has no file offset") pure (newFileOffsetOf pl (sbTable st))
      let tOff = fromIntegral tableOff :: Int
          code = sbCode st
          codeOff = tOff + fromIntegral (code - sbTable st)
      if isData
        then do
          -- a FUN_STATIC of one pointer argument, no pointers, no SRT
          pokeByteOff ptr tOff (5 :: Word32)
          pokeByteOff ptr (tOff + 4) (1 :: Word32)
          pokeByteOff ptr (tOff + 8) (0 :: Word64)
          pokeByteOff ptr (tOff + 16) (14 :: Word32)
          pokeByteOff ptr (tOff + 20) (0 :: Word32)
        else do
          -- the cut definition's own table, with the SRT cleared and the
          -- relative fields re-encoded for the stub's place
          case fileOffsetOf im (dStart d) of
            Just o | dHead d > 0 -> copyBytes (ptr `plusPtr` tOff) (src `plusPtr` fromIntegral o) (dHead d)
            _ -> fail ("no info table to copy for the stub of " ++ hsDisplay d)
          pokeByteOff ptr (codeOff - 4) (0 :: Word32)
          let ty = u32 orig (fromIntegral (maybe 0 id (fileOffsetOf im (dSym d))) - 8)
          when (ty >= 1 && ty <= 7) $ pokeByteOff ptr (codeOff - 24) (fromIntegral (fromIntegral (sbMsg st) - fromIntegral code :: Int64) :: Word64)
          when (ty == 31) $ fail ("the stub of " ++ hsDisplay d ++ " would need its large bitmap")
          when (dHead d == 40) $ do
            let ft = u32 orig (fromIntegral (maybe 0 id (fileOffsetOf im (dSym d))) - 24)
            when (ft == 1) $ fail ("the stub of " ++ hsDisplay d ++ " would need its argument bitmap")
            case [t | (at, k, t, _) <- VU.toList (anRefs an V.! i), at == dSym d - 40, toEnum (fromIntegral k) == RSlowApply] of
              t : _ -> pokeByteOff ptr (codeOff - 40) (fromIntegral (fromIntegral t - fromIntegral code :: Int64) :: Word64)
              [] -> fail ("no slow-apply offset recorded for " ++ hsDisplay d)
      case arch of
        Arm64 -> do
          ws <- either fail pure (Arm64.stubCode code (sbMsg st) barf)
          forM_ (zip [0 ..] ws) $ \(k, w) -> pokeByteOff ptr (codeOff + 4 * k) (w :: Word32)
        X86_64 ->
          forM_ (zip [0 ..] (X86.stubCode code (sbMsg st) barf)) $ \(k, b) -> pokeByteOff ptr (codeOff + k) (b :: Word8)
      mOff <- maybe (fail "stub message has no file offset") pure (newFileOffsetOf pl (sbMsg st))
      BU.unsafeUseAsCString (sbMsgText st) $ \m -> copyBytes (ptr `plusPtr` fromIntegral mOff) (castPtr m) (B.length (sbMsgText st))
      forM_ (sbClosure st) $ \c -> do
        cOff <- maybe (fail "closure stub has no file offset") pure (newFileOffsetOf pl c)
        pokeByteOff ptr (fromIntegral cOff) code
        pokeByteOff ptr (fromIntegral cOff + 8) (0 :: Word64)
    -- branch islands
    forM_ (M.toList (plIslands pl)) $ \(t, a) -> case newFileOffsetOf pl a of
      Nothing -> fail "island pool has no file offset"
      Just o -> do
        ws <- either fail pure (Arm64.islandCode a t)
        forM_ (zip [0 ..] ws) $ \(k, w) -> pokeByteOff ptr (fromIntegral o + 4 * k) (w :: Word32)
  n <- readIORef errors
  when (n > 0) (fail (show n ++ " references point into removed definitions"))

-- | The file-backed spans of a piece in the original file: (address, length, original offset).
pieceSpans :: Image -> Piece -> [(Word64, Word64, Word64)]
pieceSpans im p =
  [ (a, b - a, off)
  | (sa, sz, soff) <- VU.toList (imSectMap im),
    let a = max sa (pcVmStart p),
    let b = min (sa + sz) (pcFileEnd p),
    b > a,
    let off = soff + (a - sa)
  ]
    ++ headerSpan
  where
    -- the header area before the first section of the segment at file offset 0
    headerSpan = [(pcVmStart p, hs, 0) | pcFileOff p == 0, let hs = minimum (map (\(sa, _, _) -> sa) (VU.toList (imSectMap im))) - pcVmStart p, hs > 0]

-- | Re-encodes one reference at its new place for its new target. A branch
-- that cannot reach its target goes through its island. The anchor is the
-- new address the anchored kinds measure from.
patch :: (Word64 -> Maybe Word64) -> Ptr Word8 -> Int -> Word64 -> Word64 -> Word64 -> RefKind -> IO ()
patch island ptr off at' t' anchor' kind = case kind of
  RRel32 -> poke32 off (fromIntegral (fromIntegral t' - fromIntegral anchor' :: Int64))
  RRel64 -> poke64 off (fromIntegral (fromIntegral t' - fromIntegral anchor' :: Int64))
  RAbs32
    | t' >= 0x80000000 -> fail ("absolute 32-bit reference to " ++ hex t' ++ " does not fit")
    | otherwise -> poke32 off (fromIntegral t')
  RSrt -> poke32 off (fromIntegral (fromIntegral t' - fromIntegral (at' + 4) :: Int64))
  RConDesc -> poke64 off (fromIntegral (fromIntegral t' - fromIntegral (at' + 24) :: Int64))
  RSlowApply -> poke64 off (fromIntegral (fromIntegral t' - fromIntegral (at' + 40) :: Int64))
  RFunBitmap -> poke64 off (fromIntegral (fromIntegral t' - fromIntegral (at' + 32) :: Int64))
  RLargeBitmap -> poke64 off (fromIntegral (fromIntegral t' - fromIntegral (at' + 16) :: Int64))
  RPointer -> poke64 off t'
  _ -> do
    let count = fieldLen kind `div` 4
    ws <- forM [0 .. count - 1] $ \k -> peek32 (off + 4 * k)
    case Arm64.patchInsn kind island ws at' t' of
      Right ws' -> forM_ (zip [0 ..] ws') $ \(k, w) -> poke32 (off + 4 * k) w
      Left e -> fail (e ++ " at " ++ hex at' ++ " -> " ++ hex t')
  where
    peek32 o = peekByteOff ptr o :: IO Word32
    poke32 o (v :: Word32) = pokeByteOff ptr o v
    poke64 o (v :: Word64) = pokeByteOff ptr o v

_unusedRewrite :: Word8 -> Bool
_unusedRewrite w = w .&. 1 == 1
