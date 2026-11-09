-- Optimized in every build, --fast too: minifying is slow without it
{-# OPTIONS_GHC -O2 #-}

-- | Finishing a Mach-O output: the symbol table, the dyld streams
-- (rebases, binds, exports), the function starts, and the load commands
-- that describe the pieces. See design.md, "Layout and rewriting".
module Minify.MachOWrite (parts) where

import Control.Monad
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Unsafe as BU
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Vector as V
import Data.Word
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr
import Minify.Analysis
import Minify.Bytes
import Minify.Format
import Minify.Layout
import Minify.MachO hiding (MSection (..), MSegment (..))
import Minify.Relocate

-- | The link-edit data to append after the pieces, the action that writes
-- the header and load commands, and the statistics.
parts :: Analysis -> Plan -> Choices -> MachO -> [Rebase] -> IO (B.ByteString, Ptr Word8 -> IO (), Stats)
parts an pl Choices {rwKeepHaskellExports = keepHaskellExports, rwStrip = strip} mo rebaseRecords = do
  let im = anImage an
      orig = imBytes im
      sects = imSections im
      segs = imSegments im
      textBase = head ([segVmAddr s | s <- segs, segRole s == SegText] ++ [0])
      -- segment numbering in the output: __PAGEZERO, the pieces, __LINKEDIT
      hasPageZero = any ((== SegPageZero) . segRole) segs
      pieceIndex p = (if hasPageZero then 1 else 0) + length (takeWhile (\q -> pcVmStart q /= pcVmStart p) (plOrder pl))
      pieceOf a = case [p | p <- plPieces pl, a >= pcVmStart p, a < pcVmEnd p] of
        p : _ -> Just p
        [] -> Nothing
      segLoc :: Word64 -> Maybe (Int, Word64)
      segLoc a = (\p -> (pieceIndex p, a - pcVmStart p)) <$> pieceOf a

  -- Symbols.
  (st, nls) <- maybe (fail "no symbol table") pure (machoSymbols mo)
  let syms = [(nl, symbolName mo st nl) | nl <- nls]
      dys = case [d | LCDysymtab (Dysymtab d) <- moCommands mo] of
        d : _ -> d
        [] -> replicate 18 0
      dysAt k = fromIntegral (dys !! k) :: Int
      (ilocal, nlocal, iext, next, iundef, nundef) = (dysAt 0, dysAt 1, dysAt 2, dysAt 3, dysAt 4, dysAt 5)
      compactedSect i = i >= 1 && i <= V.length sects && isCompactedSection (sects V.! (i - 1))
      keepSym (nl, _)
        | strip && nType nl .&. 0x0e /= 0 = Left "stripped"
        | nType nl .&. 0x0e == 0x0e && compactedSect (fromIntegral (nSect nl)) = relocate an pl (nValue nl)
        | otherwise = Right (nValue nl)
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
        | r <- rebaseRecords,
          let a = segVmAddr (segs !! rbSeg r) + rbOff r,
          Just a' <- [newAddrOf an pl a],
          Just (si, so) <- [segLoc a']
        ]
      stubRebases = [Rebase si so 1 | stb <- M.elems (plStubs pl), Just c <- [sbClosure stb], Just (si, so) <- [segLoc c]]
      rebaseBytes = toStrict (encodeRebases (rebases ++ stubRebases))
  -- Binds: every kind becomes an ordinary bind. A slot can sit inside a
  -- Haskell static closure (a @foreign import "&sym"@ is a closure whose
  -- payload dyld binds), so slots move with their definitions like rebases.
  let dyld = head ([d | LCDyldInfo d <- moCommands mo] ++ [DyldInfo lcDyldInfoOnly 0 0 0 0 0 0 0 0 0 0])
      slice off sz = B.take (fromIntegral sz) (B.drop (fromIntegral off) orig)
      allBinds = concat [decodeBinds (slice o s) | (o, s) <- [(bindOff dyld, bindSize dyld), (lazyBindOff dyld, lazyBindSize dyld), (weakBindOff dyld, weakBindSize dyld)], s > 0]
      binds =
        [ b {bdSeg = si, bdOff = so}
        | b <- allBinds,
          let a = segVmAddr (segs !! bdSeg b) + bdOff b,
          Just a' <- [newAddrOf an pl a],
          Just (si, so) <- [segLoc a']
        ]
      bindBytes = toStrict (encodeBinds binds)
  -- Exports.
  let exportsOld = decodeExports (slice (exportOff dyld) (exportSize dyld))
      haskellNamed e = case defAt im (textBase + exAddr e) of
        Just d -> dKind d == Haskell && dSym d == textBase + exAddr e
        Nothing -> False
      exports =
        flip mapMaybe exportsOld $ \e ->
          if
            | exFlags e .&. 0x08 /= 0 -> Just e
            | not keepHaskellExports && haskellNamed e -> Nothing
            | otherwise -> (\a' -> e {exAddr = a' - textBase}) <$> newAddrOf an pl (textBase + exAddr e)
      exportBytes = toStrict (encodeExports exports)
  -- Function starts.
  let fsOld = concat [decodeFunctionStarts textBase (slice o s) | LCLinkeditData c o s <- moCommands mo, c == lcFunctionStarts]
      fsNew = if strip then [] else mapMaybe (newAddrOf an pl) fsOld
      fsBytes = toStrict (encodeFunctionStarts textBase fsNew)

  -- The link-edit blob.
  let pad8 b = b <> B.replicate (fromIntegral (alignUp (fromIntegral (B.length b)) 8) - B.length b) 0
      blobs = map pad8 [rebaseBytes, bindBytes, exportBytes, fsBytes, symtabBytes, indirectBytes]
      offsets = scanl (\o b -> o + B.length b) (fromIntegral (plLinkeditOff pl)) blobs
      offAt k = offsets !! k
      (rebaseO, bindO, exportO, fsO, symO, indO, strO) = (offAt 0, offAt 1, offAt 2, offAt 3, offAt 4, offAt 5, offAt 6)
      linkedit = B.concat (blobs ++ [strtab])
      linkeditLen = B.length linkedit

  -- Load commands.
  let entryAddr = head ([textBase + off | LCMain off _ <- moCommands mo] ++ [0])
  entryNew <- either fail pure (relocate an pl entryAddr)
  entryOff <- maybe (fail "entry point has no file offset") pure (newFileOffsetOf pl entryNew)
  let segCmd name vmaddr vmsize fileoff filesize maxp initp flags secs =
        let body =
              BB.byteString (padName name)
                <> putU64 vmaddr
                <> putU64 vmsize
                <> putU64 fileoff
                <> putU64 filesize
                <> putU32 maxp
                <> putU32 initp
                <> putU32 (fromIntegral (length secs))
                <> putU32 flags
                <> mconcat (map secCmd secs)
         in putU32 lcSegment64 <> putU32 (fromIntegral (72 + 80 * length secs)) <> body
      secCmd s =
        let raw = secRaw s
         in BB.byteString (padName (secName s))
              <> BB.byteString (padName (secSegName s))
              <> putU64 (secAddr s)
              <> putU64 (secSize s)
              <> putU32 (fromIntegral (secOffset s))
              <> putU32 (msAlign raw)
              <> putU32 (msRelOff raw)
              <> putU32 (msNReloc raw)
              <> putU32 (msFlags raw)
              <> putU32 (msReserved1 raw)
              <> putU32 (msReserved2 raw)
              <> putU32 (msReserved3 raw)
      padName n = B.take 16 (n <> B.replicate 16 0)
      pageZero = [segCmd (segName s) (segVmAddr s) (segVmSize s) 0 0 (segMaxProt s) (segInitProt s) (segFlags s) [] | s <- segs, segRole s == SegPageZero]
      pieceCmds = [segCmd (pcName p) (pcVmStart p) (pcVmEnd p - pcVmStart p) (pcFileOff p) (pcFileEnd p - pcVmStart p) (segMaxProt sg) (segInitProt sg) (segFlags sg) (pcSections p) | p <- plOrder pl, let sg = pcSegment p]
      linkeditSeg = head [s | s <- segs, segRole s == SegLinkedit]
      pageSize = imPageSize im
      linkeditCmd = segCmd "__LINKEDIT" (plLinkeditVm pl) (alignUp (fromIntegral linkeditLen) pageSize) (plLinkeditOff pl) (fromIntegral linkeditLen) (segMaxProt linkeditSeg) (segInitProt linkeditSeg) (segFlags linkeditSeg) []
      others = flip mapMaybe (moCommands mo) $ \case
        LCSegment _ -> Nothing
        LCSymtab _ -> Just (putU32 lcSymtab <> putU32 24 <> putU32 (fromIntegral symO) <> putU32 (fromIntegral (length keptSyms)) <> putU32 (fromIntegral strO) <> putU32 (fromIntegral (B.length strtab)))
        LCDysymtab _ ->
          let ws = [0, nlocal', nlocal', next', nlocal' + next', nundef', 0, 0, 0, 0, 0, 0, indO, nindirect, 0, 0, 0, 0]
           in Just (putU32 lcDysymtab <> putU32 80 <> mconcat (map (putU32 . fromIntegral) ws))
        LCDyldInfo d ->
          Just
            ( putU32 (diCmd d)
                <> putU32 48
                <> putU32 (fromIntegral rebaseO)
                <> putU32 (fromIntegral (B.length rebaseBytes))
                <> putU32 (fromIntegral bindO)
                <> putU32 (fromIntegral (B.length bindBytes))
                <> putU32 0
                <> putU32 0
                <> putU32 0
                <> putU32 0
                <> putU32 (fromIntegral exportO)
                <> putU32 (fromIntegral (B.length exportBytes))
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
          putU32 0xfeedfacf
            <> putU32 (moCpuType mo)
            <> putU32 (moCpuSubtype mo)
            <> putU32 (moFileType mo)
            <> putU32 (fromIntegral (length cmds))
            <> putU32 (fromIntegral (B.length cmdBytes))
            <> putU32 (moFlags mo)
            <> putU32 (moReserved mo)
      firstSectOff = minimum (fromIntegral (plLinkeditOff pl) : [fromIntegral (secOffset s) | p <- plPieces pl, s <- pcSections p, secOffset s /= 0])
  when (32 + B.length cmdBytes > firstSectOff) $
    fail ("the load commands need " ++ show (32 + B.length cmdBytes) ++ " bytes but the first section starts at " ++ show firstSectOff)
  let finish ptr = do
        BU.unsafeUseAsCString header $ \h -> copyBytes ptr (castPtr h) (B.length header)
        BU.unsafeUseAsCString cmdBytes $ \c -> copyBytes (ptr `plusPtr` 32) (castPtr c) (B.length cmdBytes)
      stats =
        Stats
          { stSymbolsKept = length keptSyms,
            stSymbolsDropped = length syms - length keptSyms,
            stRebases = length rebases,
            stBinds = length binds,
            stExports = length exports,
            stLinkeditBytes = linkeditLen,
            stFileBytes = 0
          }
  pure (linkedit, finish, stats)

_unusedMachOWrite :: [Word64] -> [Word64]
_unusedMachOWrite = sortOn id
