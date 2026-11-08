-- | Producing the output file: copying and relocating the kept definitions,
-- rebuilding the link-edit data and the load commands. See design.md,
-- "Layout and rewriting".
module Minify.Rewrite
  ( rewrite,
    Stats (..),
  )
where

import Control.Monad
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BU
import Data.IORef
import Data.Int
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word
import Foreign.Marshal.Utils (copyBytes, fillBytes)
import Foreign.Ptr
import Foreign.Storable
import Minify.Analysis
import Minify.Arm64
import Minify.Layout
import Minify.MachO

data Stats = Stats
  { stSymbolsKept :: Int,
    stSymbolsDropped :: Int,
    stRebases :: Int,
    stBinds :: Int,
    stExports :: Int,
    stLinkeditBytes :: Int,
    stFileBytes :: Int
  }
  deriving (Show)

alignUp :: Word64 -> Word64 -> Word64
alignUp a n = (a + n - 1) .&. complement (n - 1)

toStrict :: BB.Builder -> B.ByteString
toStrict = BL.toStrict . BB.toLazyByteString

rewrite :: Analysis -> Plan -> IO (B.ByteString, Stats)
rewrite an pl = do
  let im = anImage an
      mo = imMachO im
      orig = moBytes mo
      defs = imDefs im
      sects = imSections im
      relocate :: Word64 -> Either String Word64
      relocate a = case newAddrOf an pl a of
        Just a' -> Right a'
        Nothing -> case defAt im a of
          Just d | Just st <- M.lookup (dIndex d) (plStubs pl) -> Right $ case sbClosure st of
            -- a pointer to a cut closure keeps its tag bits
            Just c -> c + (a .&. 7)
            Nothing -> sbCode st
          _ -> Left ("reference into a removed definition at " ++ hex a ++ maybe "" ((" in " ++) . hsDisplay) (defAt im a))
      barf = case [nValue nl | (nl, n) <- imSymbols im, n == "_barf", nType nl .&. 0x0e == 0x0e] of
        v : _ -> Just v
        [] -> Nothing
      textBase = head ([segVmAddr s | s <- imSegments im, segName s == "__TEXT"] ++ [0])
      -- segment numbering in the output: __PAGEZERO, the pieces, __LINKEDIT
      hasPageZero = any ((== "__PAGEZERO") . segName) (imSegments im)
      pieceIndex p = (if hasPageZero then 1 else 0) + length (takeWhile (\q -> pcVmStart q /= pcVmStart p) (plOrder pl))
      pieceOf a = case [p | p <- plPieces pl, a >= pcVmStart p, a < pcVmEnd p] of
        p : _ -> Just p
        [] -> Nothing
      segLoc :: Word64 -> Maybe (Int, Word64)
      segLoc a = (\p -> (pieceIndex p, a - pcVmStart p)) <$> pieceOf a

  -- Symbols.
  let syms = imSymbols im
      dys = case [d | LCDysymtab (Dysymtab d) <- moCommands mo] of
        d : _ -> d
        [] -> replicate 18 0
      dysAt k = fromIntegral (dys !! k) :: Int
      (ilocal, nlocal, iext, next, iundef, nundef) = (dysAt 0, dysAt 1, dysAt 2, dysAt 3, dysAt 4, dysAt 5)
      compactedSect i = i >= 1 && i <= V.length sects && isCompactedSection (sects V.! (i - 1))
      keepSym (nl, _) =
        if nType nl .&. 0x0e == 0x0e && compactedSect (fromIntegral (nSect nl))
          then relocate (nValue nl)
          else Right (nValue nl)
      decided = [(nl, name, either (const Nothing) Just (keepSym (nl, name))) | (nl, name) <- syms]
      -- sections are renumbered in the output: a symbol's n_sect follows its address
      newSections = concatMap pcSections (plOrder pl)
      sectIndexOf v = case [k | (k, sc) <- zip [1 :: Int ..] newSections, v >= secAddr sc, v < secAddr sc + max 1 (secSize sc)] of
        k : _ -> fromIntegral k
        [] -> 0
      renumber nl v = if nType nl .&. 0x0e == 0x0e then nl {nValue = v, nSect = sectIndexOf v} else nl {nValue = v}
      keptSyms = [(renumber nl v, name) | (nl, name, Just v) <- decided]
      indexMap = M.fromList [(oldIx, newIx) | (newIx, (oldIx, _)) <- zip [0 :: Int ..] [(i, ()) | (i, (_, _, Just _)) <- zip [0 :: Int ..] decided]]
      countIn lo n = length [() | (i, (_, _, Just _)) <- zip [0 :: Int ..] decided, i >= lo, i < lo + n]
      nlocal' = countIn ilocal nlocal
      next' = countIn iext next
      nundef' = countIn iundef nundef
      strtab = toStrict (BB.word8 0 <> mconcat [BB.byteString n <> BB.word8 0 | (_, n) <- keptSyms])
      strOffsets = scanl (\o (_, n) -> o + B.length n + 1) 1 keptSyms
      symtabBytes =
        toStrict $
          mconcat
            [ putU32 (fromIntegral so) <> BB.word8 (nType nl) <> BB.word8 (nSect nl) <> BB.word16LE (nDesc nl) <> putU64 (nValue nl)
              | ((nl, _), so) <- zip keptSyms strOffsets
            ]
      indirectOff = fromIntegral (dys !! 12)
      nindirect = fromIntegral (dys !! 13)
      indirectBytes =
        toStrict $
          mconcat
            [ putU32 v'
              | k <- [0 .. nindirect - 1],
                let v = u32 orig (indirectOff + 4 * k),
                let v' = if v .&. 0xc0000000 /= 0 then v else fromIntegral (M.findWithDefault (error ("indirect symbol " ++ show v ++ " was removed")) (fromIntegral v) indexMap)
            ]

  -- Rebases: slots inside removed definitions go; the rest move with their definition.
  let rebases =
        [ Rebase si so (rbType r)
          | r <- imRebaseRecords im,
            let a = segVmAddr (imSegments im !! rbSeg r) + rbOff r,
            Just a' <- [newAddrOf an pl a],
            Just (si, so) <- [segLoc a']
        ]
      stubRebases = [Rebase si so 1 | st <- M.elems (plStubs pl), Just c <- [sbClosure st], Just (si, so) <- [segLoc c]]
      rebaseBytes = toStrict (encodeRebases (rebases ++ stubRebases))
  -- Binds: every kind becomes an ordinary bind at its (unchanged) address.
  let dyld = head ([d | LCDyldInfo d <- moCommands mo] ++ [DyldInfo lcDyldInfoOnly 0 0 0 0 0 0 0 0 0 0])
      slice off sz = B.take (fromIntegral sz) (B.drop (fromIntegral off) orig)
      allBinds = concat [decodeBinds (slice o s) | (o, s) <- [(bindOff dyld, bindSize dyld), (lazyBindOff dyld, lazyBindSize dyld), (weakBindOff dyld, weakBindSize dyld)], s > 0]
      binds =
        [ b {bdSeg = si, bdOff = so}
          | b <- allBinds,
            let a = segVmAddr (imSegments im !! bdSeg b) + bdOff b,
            Just (si, so) <- [segLoc a]
        ]
      bindBytes = toStrict (encodeBinds binds)
  -- Exports.
  let exportsOld = decodeExports (slice (exportOff dyld) (exportSize dyld))
      exports =
        flip mapMaybe exportsOld $ \e ->
          if exFlags e .&. 0x08 /= 0
            then Just e
            else (\a' -> e {exAddr = a' - textBase}) <$> newAddrOf an pl (textBase + exAddr e)
      exportBytes = toStrict (encodeExports exports)
  -- Function starts.
  let fsOld = concat [decodeFunctionStarts textBase (slice o s) | LCLinkeditData c o s <- moCommands mo, c == lcFunctionStarts]
      fsNew = mapMaybe (newAddrOf an pl) fsOld
      fsBytes = toStrict (encodeFunctionStarts textBase fsNew)

  -- The link-edit blob.
  let pad8 b = b <> B.replicate (fromIntegral (alignUp (fromIntegral (B.length b)) 8) - B.length b) 0
      parts = map pad8 [rebaseBytes, bindBytes, exportBytes, fsBytes, symtabBytes, indirectBytes]
      offsets = scanl (\o b -> o + B.length b) (fromIntegral (plLinkeditOff pl)) parts
      offAt k = offsets !! k
      (rebaseO, bindO, exportO, fsO, symO, indO, strO) = (offAt 0, offAt 1, offAt 2, offAt 3, offAt 4, offAt 5, offAt 6)
      linkedit = B.concat (parts ++ [strtab])
      linkeditLen = B.length linkedit
      fileSize = fromIntegral (plLinkeditOff pl) + linkeditLen

  -- Load commands.
  let entryAddr = head ([textBase + off | LCMain off _ <- moCommands mo] ++ [0])
  entryNew <- either fail pure (relocate entryAddr)
  entryOff <- maybe (fail "entry point has no file offset") pure (newFileOffsetOf pl entryNew)
  let segCmd name vmaddr vmsize fileoff filesize maxp initp flags secs =
        let body =
              BB.byteString (padName name) <> putU64 vmaddr <> putU64 vmsize <> putU64 fileoff <> putU64 filesize
                <> putU32 maxp <> putU32 initp <> putU32 (fromIntegral (length secs)) <> putU32 flags
                <> mconcat (map secCmd secs)
         in putU32 lcSegment64 <> putU32 (fromIntegral (72 + 80 * length secs)) <> body
      secCmd s =
        BB.byteString (padName (secName s)) <> BB.byteString (padName (secSegName s)) <> putU64 (secAddr s) <> putU64 (secSize s)
          <> putU32 (secOffset s) <> putU32 (secAlign s) <> putU32 (secRelOff s) <> putU32 (secNReloc s) <> putU32 (secFlags s)
          <> putU32 (secReserved1 s) <> putU32 (secReserved2 s) <> putU32 (secReserved3 s)
      padName n = B.take 16 (n <> B.replicate 16 0)
      pageZero = [segCmd (segName s) (segVmAddr s) (segVmSize s) 0 0 (segMaxProt s) (segInitProt s) (segFlags s) [] | s <- imSegments im, segName s == "__PAGEZERO"]
      pieceCmds = [segCmd (pcName p) (pcVmStart p) (pcVmEnd p - pcVmStart p) (pcFileOff p) (pcFileEnd p - pcVmStart p) (pcMaxProt p) (pcInitProt p) (pcFlags p) (pcSections p) | p <- plOrder pl]
      linkeditSeg = head [s | s <- imSegments im, segName s == "__LINKEDIT"]
      linkeditCmd = segCmd "__LINKEDIT" (plLinkeditVm pl) (alignUp (fromIntegral linkeditLen) pageSize) (plLinkeditOff pl) (fromIntegral linkeditLen) (segMaxProt linkeditSeg) (segInitProt linkeditSeg) (segFlags linkeditSeg) []
      others = flip mapMaybe (moCommands mo) $ \case
        LCSegment _ -> Nothing
        LCSymtab _ -> Just (putU32 lcSymtab <> putU32 24 <> putU32 (fromIntegral symO) <> putU32 (fromIntegral (length keptSyms)) <> putU32 (fromIntegral strO) <> putU32 (fromIntegral (B.length strtab)))
        LCDysymtab _ ->
          let ws = [0, nlocal', nlocal', next', nlocal' + next', nundef', 0, 0, 0, 0, 0, 0, indO, nindirect, 0, 0, 0, 0]
           in Just (putU32 lcDysymtab <> putU32 80 <> mconcat (map (putU32 . fromIntegral) ws))
        LCDyldInfo d ->
          Just
            ( putU32 (diCmd d) <> putU32 48
                <> putU32 (fromIntegral rebaseO) <> putU32 (fromIntegral (B.length rebaseBytes))
                <> putU32 (fromIntegral bindO) <> putU32 (fromIntegral (B.length bindBytes))
                <> putU32 0 <> putU32 0
                <> putU32 0 <> putU32 0
                <> putU32 (fromIntegral exportO) <> putU32 (fromIntegral (B.length exportBytes))
            )
        LCLinkeditData c _ _
          | c == lcFunctionStarts -> Just (putU32 c <> putU32 16 <> putU32 (fromIntegral fsO) <> putU32 (fromIntegral (B.length fsBytes)))
          | c == lcDataInCode -> Just (putU32 c <> putU32 16 <> putU32 (fromIntegral fsO) <> putU32 0)
          | c == lcCodeSignature -> Nothing
          | otherwise -> Just (putU32 c <> putU32 16 <> putU32 0 <> putU32 0)
        LCMain _ stack -> Just (putU32 lcMain <> putU32 24 <> putU64 entryOff <> putU64 stack)
        LCOther _ raw -> Just (BB.byteString raw)
      cmds = pageZero ++ pieceCmds ++ [linkeditCmd] ++ others
      cmdBytes = toStrict (mconcat cmds)
      header =
        toStrict $
          putU32 0xfeedfacf <> putU32 (moCpuType mo) <> putU32 (moCpuSubtype mo) <> putU32 (moFileType mo)
            <> putU32 (fromIntegral (length cmds)) <> putU32 (fromIntegral (B.length cmdBytes)) <> putU32 (moFlags mo) <> putU32 (moReserved mo)
      firstSectOff = minimum (fromIntegral (plLinkeditOff pl) : [fromIntegral (secOffset s) | p <- plPieces pl, s <- pcSections p, secOffset s /= 0])
  when (32 + B.length cmdBytes > firstSectOff) $
    fail ("the load commands need " ++ show (32 + B.length cmdBytes) ++ " bytes but the first section starts at " ++ show firstSectOff)

  -- The bytes.
  errors <- newIORef (0 :: Int)
  out <- BI.create fileSize $ \ptr -> do
    fillBytes ptr 0 fileSize
    BU.unsafeUseAsCString orig $ \src0 -> do
      let src = castPtr src0 :: Ptr Word8
      -- every piece's address range, as it was
      forM_ (plPieces pl) $ \p ->
        forM_ (pieceSpans im p) $ \(a, n, off) ->
          copyBytes (ptr `plusPtr` fromIntegral (pcFileOff p + (a - pcVmStart p))) (src `plusPtr` fromIntegral off) (fromIntegral n)
      -- kept definitions that moved
      forM_ [0 .. V.length defs - 1] $ \i -> when (anKept an VU.! i) $ do
        let d = defs V.! i
            new = plNewStart pl VU.! i
        when (new /= dStart d) $
          case (fileOffsetOf im (dStart d), newFileOffsetOf pl new) of
            (Just o, Just o') -> copyBytes (ptr `plusPtr` fromIntegral o') (src `plusPtr` fromIntegral o) (fromIntegral (dEnd d - dStart d))
            _ -> fail ("no file offset for " ++ hsDisplay d)
      -- references
      forM_ [0 .. V.length defs - 1] $ \i -> when (anKept an VU.! i && dKind (defs V.! i) == Haskell) $ do
        let d = defs V.! i
            delta = plNewStart pl VU.! i - dStart d
        VU.forM_ (anRefs an V.! i) $ \(at, k, target) -> do
          let kind = toEnum (fromIntegral k) :: RefKind
              at' = at + delta
          case relocate target of
            Left e -> do
              modifyIORef' errors (+ 1)
              n <- readIORef errors
              when (n <= 20) (putStrLn ("  " ++ e ++ " (from " ++ hsDisplay d ++ ")"))
            Right t' -> case newFileOffsetOf pl at' of
              Nothing -> fail ("no file offset for a reference in " ++ hsDisplay d)
              Just o -> patch (\t -> M.lookup t (plIslands pl)) ptr (fromIntegral o) at' t' kind
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
              case [t | (at, k, t) <- VU.toList (anRefs an V.! i), at == dSym d - 40, toEnum (fromIntegral k) == RSlowApply] of
                t : _ -> pokeByteOff ptr (codeOff - 40) (fromIntegral (fromIntegral t - fromIntegral code :: Int64) :: Word64)
                [] -> fail ("no slow-apply offset recorded for " ++ hsDisplay d)
        -- adrp x0, msg; add x0, x0, lo; bl barf (or brk); brk #0
        w1 <- either fail pure (adrpInsn 0 code (sbMsg st))
        pokeByteOff ptr codeOff w1
        pokeByteOff ptr (codeOff + 4) (addInsn 0 0 (sbMsg st))
        w3 <- case barf of
          Just b -> either fail pure (blInsn (code + 8) b)
          Nothing -> pure brkInsn
        pokeByteOff ptr (codeOff + 8) w3
        pokeByteOff ptr (codeOff + 12) brkInsn
        mOff <- maybe (fail "stub message has no file offset") pure (newFileOffsetOf pl (sbMsg st))
        BU.unsafeUseAsCString (sbMsgText st) $ \m -> copyBytes (ptr `plusPtr` fromIntegral mOff) (castPtr m) (B.length (sbMsgText st))
        forM_ (sbClosure st) $ \c -> do
          cOff <- maybe (fail "closure stub has no file offset") pure (newFileOffsetOf pl c)
          pokeByteOff ptr (fromIntegral cOff) code
          pokeByteOff ptr (fromIntegral cOff + 8) (0 :: Word64)
      -- branch islands: adrp x16, target; add x16, x16, lo; br x16
      forM_ (M.toList (plIslands pl)) $ \(t, a) -> case newFileOffsetOf pl a of
        Nothing -> fail "island pool has no file offset"
        Just o -> do
          w1 <- either fail pure (adrpInsn 16 a t)
          pokeByteOff ptr (fromIntegral o) w1
          pokeByteOff ptr (fromIntegral o + 4) (addInsn 16 16 t)
          pokeByteOff ptr (fromIntegral o + 8) (0xd61f0200 :: Word32)
    -- header and load commands
    BU.unsafeUseAsCString header $ \h -> copyBytes ptr (castPtr h) (B.length header)
    BU.unsafeUseAsCString cmdBytes $ \c -> copyBytes (ptr `plusPtr` 32) (castPtr c) (B.length cmdBytes)
    -- link-edit
    BU.unsafeUseAsCString linkedit $ \l -> copyBytes (ptr `plusPtr` fromIntegral (plLinkeditOff pl)) (castPtr l) linkeditLen
  n <- readIORef errors
  when (n > 0) (fail (show n ++ " references point into removed definitions"))
  pure
    ( out,
      Stats
        { stSymbolsKept = length keptSyms,
          stSymbolsDropped = length syms - length keptSyms,
          stRebases = length rebases,
          stBinds = length binds,
          stExports = length exports,
          stLinkeditBytes = linkeditLen,
          stFileBytes = fileSize
        }
    )
  where
    hex a = "0x" ++ go a
      where
        go 0 = ""
        go v = go (v `shiftR` 4) ++ [("0123456789abcdef" !!) (fromIntegral (v .&. 15))]

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
    -- the header area before the first section of __TEXT
    headerSpan = [(pcVmStart p, hs, 0) | pcFileOff p == 0, let hs = minimum (map (\(sa, _, _) -> sa) (VU.toList (imSectMap im))) - pcVmStart p, hs > 0]

-- | Re-encodes one reference at its new place for its new target. A branch
-- that cannot reach its target goes through its island.
patch :: (Word64 -> Maybe Word64) -> Ptr Word8 -> Int -> Word64 -> Word64 -> RefKind -> IO ()
patch island ptr off at' t' kind = case kind of
  RAdrpAdd -> do
    w1 <- peek32 off
    w2 <- peek32 (off + 4)
    poke32 off =<< orFail (encodeAdrp w1 at' t')
    poke32 (off + 4) (encodeAdd12 w2 t')
  RAdrpLdr -> do
    w1 <- peek32 off
    w2 <- peek32 (off + 4)
    poke32 off =<< orFail (encodeAdrp w1 at' t')
    poke32 (off + 4) =<< orFail (encodeLdr12 w2 t')
  RB -> imm26
  RBl -> imm26
  RBcond -> imm19
  RCbz -> imm19
  RTbz -> do
    w <- peek32 off
    poke32 off =<< orFail (encodeImm14 w at' t')
  RSrt -> poke32 off (fromIntegral (fromIntegral t' - fromIntegral (at' + 4) :: Int64))
  RConDesc -> poke64 off (fromIntegral (fromIntegral t' - fromIntegral (at' + 24) :: Int64))
  RSlowApply -> poke64 off (fromIntegral (fromIntegral t' - fromIntegral (at' + 40) :: Int64))
  RFunBitmap -> poke64 off (fromIntegral (fromIntegral t' - fromIntegral (at' + 32) :: Int64))
  RLargeBitmap -> poke64 off (fromIntegral (fromIntegral t' - fromIntegral (at' + 16) :: Int64))
  RPointer -> poke64 off t'
  where
    imm26 = do
      w <- peek32 off
      case encodeImm26 w at' t' of
        Right w' -> poke32 off w'
        Left e -> case island t' of
          Just a -> poke32 off =<< orFail (encodeImm26 w at' a)
          Nothing -> fail (e ++ " at " ++ show at' ++ " -> " ++ show t' ++ " and no island")
    imm19 = do
      w <- peek32 off
      poke32 off =<< orFail (encodeImm19 w at' t')
    peek32 o = peekByteOff ptr o :: IO Word32
    poke32 o (v :: Word32) = pokeByteOff ptr o v
    poke64 o (v :: Word64) = pokeByteOff ptr o v
    orFail = either (\e -> fail (e ++ " at " ++ show at' ++ " -> " ++ show t')) pure
