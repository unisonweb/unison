-- | Finishing an ELF output: the program headers (in the patch area, since
-- the holes add some and the original table has no room), the section
-- headers with the compacted sections cut into pieces, the symbol table
-- relocated, and the dynamic symbols and relative relocations patched in
-- place. See design.md, "ELF".
module Minify.ElfWrite (parts) where

import Control.Monad
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Unsafe as BU
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Vector as V
import Data.Word
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr
import Foreign.Storable
import Minify.Analysis
import Minify.Bytes
import Minify.Elf
import Minify.Format
import Minify.Layout
import Minify.Relocate

-- | The bytes to append after the pieces (non-loaded sections, the symbol
-- table, the section headers), the action that writes the ELF header, the
-- program headers and the in-place fixes, and the statistics.
parts :: Analysis -> Plan -> Choices -> Elf -> IO (B.ByteString, Ptr Word8 -> IO (), Stats)
parts an pl Choices {rwStrip = strip} e = do
  let im = anImage an
      orig = imBytes im
      shdrs = zip [0 :: Int ..] (elfShdrs e)
      pageSize = imPageSize im
      -- the new sections, in file order: the pieces' sections, then the
      -- non-loaded ones, then the symbol and string tables
      pieceSects = [(s, if pcExtra p then secName s <> "." <> BC.pack (show k) else secName s) | (k, p) <- zip [0 :: Int ..] (plPieces pl), s <- pcSections p]
      pieceSectsNamed = renamePieces pieceSects
      -- a section cut into several pieces: .text, .text.1, .text.2 ...
      renamePieces xs =
        let counts = M.fromListWith (+) [(secName s, 1 :: Int) | (s, _) <- xs]
            go :: M.Map B.ByteString Int -> [(Section, B.ByteString)] -> [(Section, B.ByteString)]
            go _ [] = []
            go seen ((s, _) : rest) =
              let n = M.findWithDefault 0 (secName s) seen
                  nm = if n == 0 || M.findWithDefault 0 (secName s) counts == 1 then secName s else secName s <> "." <> BC.pack (show n)
               in (s, nm) : go (M.insertWith (+) (secName s) 1 seen) rest
         in go M.empty xs
      isAlloc s = shFlags s .&. shfAlloc /= 0
      symtabIx = elfSymtabIx e
      strtabIx = fmap (\i -> fromIntegral (shLink (elfShdrs e !! i))) symtabIx
      -- relocation sections a link with --emit-relocs kept describe the
      -- input's layout, not the output's, and are dropped
      unloaded =
        [ (i, s)
          | (i, s) <- shdrs,
            shType s /= shtNull,
            not (isAlloc s),
            shType s /= shtRela,
            Just i /= symtabIx,
            Just i /= strtabIx,
            i /= elfShstrndx e
        ]
  -- Symbols: relocated, with their section numbers mapped to the new
  -- sections. Symbols in removed definitions go; with --strip the table goes.
  (symsKept, nlocals, allSyms) <- case symtabIx of
    Nothing -> pure ([], 0, 0)
    Just ix -> do
      let syms = symbolsOf e ix
          relocSym (sym, name) = do
            let shndx = fromIntegral (stShndx sym) :: Int
            if shndx == 0 || shndx >= 0xff00
              then Just (sym, name)
              else case newAddrOrStub (stValue sym) of
                Nothing -> Nothing
                Just v -> Just (sym {stValue = v}, name)
          newAddrOrStub a = case relocate an pl a of
            Right a' -> Just a'
            Left _ -> Nothing
          kept = if strip then [] else mapMaybe relocSym syms
          locals = length (takeWhile (\(s, _) -> stInfo s `shiftR` 4 == 0) kept)
      pure (kept, locals, length syms)
  -- Section numbering in the output.
  let newList =
        [(Nothing, "", Nothing)] -- the null section
          ++ [(Just s, nm, Nothing) | (s, nm) <- pieceSectsNamed]
          ++ [(Nothing, sectionName e s, Just (i, s)) | (i, s) <- unloaded]
          ++ (if strip then [] else [(Nothing, ".symtab", Nothing), (Nothing, ".strtab", Nothing)])
          ++ [(Nothing, ".shstrtab", Nothing)]
      count = length newList
      shstrtab = toStrict (BB.word8 0 <> mconcat [BB.byteString nm <> BB.word8 0 | (_, nm, _) <- drop 1 newList])
      nameOffsets = M.fromList (zip (map (\(_, nm, _) -> nm) (drop 1 newList)) (scanl (\o nm -> o + B.length nm + 1) 1 (map (\(_, nm, _) -> nm) (drop 1 newList))))
      nameOff nm = fromIntegral (M.findWithDefault 0 nm nameOffsets) :: Word32
      -- old section index to new: a compacted section maps to its first piece
      oldToNew = M.fromList (reverse [(secOrig s, k) | (k, (Just s, _, _)) <- zip [0 :: Int ..] newList] ++ [(i, k) | (k, (_, _, Just (i, _))) <- zip [0 :: Int ..] newList])
      strtabNew = head ([k | (k, (_, nm, _)) <- zip [0 :: Int ..] newList, nm == ".strtab"] ++ [0])
      sectIndexOfAddr v = case [k | (k, (Just s, _, _)) <- zip [0 :: Int ..] newList, v >= secAddr s, v < secAddr s + max 1 (secSize s)] of
        k : _ -> fromIntegral k
        [] -> 0
      remapShndx old v
        | old == 0 || old >= 0xff00 = old
        | otherwise = case M.lookup (fromIntegral old) oldToNew of
            Just k | k > 0, (Just s, _, _) <- newList !! k, isCompactedSection s -> sectIndexOfAddr v
            Just k -> fromIntegral k
            Nothing -> 0
  -- The symbol table bytes.
  let strtab = toStrict (BB.word8 0 <> mconcat [BB.byteString n <> BB.word8 0 | (_, n) <- symsKept])
      strOffsets = scanl (\o (_, n) -> o + B.length n + 1) 1 symsKept
      symBytes =
        toStrict $
          mconcat
            [ putU32 (if B.null n then 0 else fromIntegral so) <> BB.word8 (stInfo s) <> BB.word8 (stOther s) <> putU16 (remapShndx (stShndx s) (stValue s)) <> putU64 (stValue s) <> putU64 (stSize s)
              | ((s, n), so) <- zip symsKept strOffsets
            ]
  -- The trailer: unloaded sections, symtab, strtab, shstrtab, section headers.
  let pad8 b = b <> B.replicate (fromIntegral (alignUp (fromIntegral (B.length b)) 8) - B.length b) 0
      unloadedBytes = [pad8 (B.take (fromIntegral (shSize s)) (B.drop (fromIntegral (shOffset s)) orig)) | (_, s) <- unloaded]
      trailerParts = unloadedBytes ++ (if strip then [] else [pad8 symBytes, pad8 strtab]) ++ [pad8 shstrtab]
      trailerOffsets = scanl (\o b -> o + fromIntegral (B.length b)) (plLinkeditOff pl) trailerParts
      unloadedOffs = M.fromList (zip (map fst unloaded) trailerOffsets)
      afterUnloaded = drop (length unloaded) trailerOffsets
      (symOff, strOff, shstrOff) =
        if strip
          then (0, 0, head afterUnloaded)
          else (afterUnloaded !! 0, afterUnloaded !! 1, afterUnloaded !! 2)
      shoff = last trailerOffsets
      shdrFor (k, (ms, nm, munl)) = case (ms, munl) of
        (Just s, _) ->
          let raw = secRaw s
           in shdr (nameOff nm) (esType raw) (esFlags raw) (secAddr s) (if secNoBits s then fromMaybe 0 (pieceOffsetOf (secAddr s)) else secOffset s) (secSize s) (remapLink (esLink raw)) (remapInfo (esType raw) (esInfo raw)) (esAlign raw) (esEntSize raw)
        (Nothing, Just (i, s)) -> shdr (nameOff nm) (shType s) (shFlags s) 0 (M.findWithDefault 0 i unloadedOffs) (shSize s) (remapLink (shLink s)) (shInfo s) (shAddralign s) (shEntsize s)
        (Nothing, Nothing)
          | nm == ".symtab" -> shdr (nameOff nm) shtSymtab 0 0 symOff (fromIntegral (B.length symBytes)) (fromIntegral strtabNew) (fromIntegral nlocals) 8 24
          | nm == ".strtab" -> shdr (nameOff nm) shtStrtab 0 0 strOff (fromIntegral (B.length strtab)) 0 0 1 0
          | nm == ".shstrtab" -> shdr (nameOff nm) shtStrtab 0 0 shstrOff (fromIntegral (B.length shstrtab)) 0 0 1 0
          | k == 0 -> shdr 0 0 0 0 0 0 0 0 0 0
          | otherwise -> shdr 0 0 0 0 0 0 0 0 0 0
      remapLink l = if l == 0 then 0 else fromIntegral (M.findWithDefault 0 (fromIntegral l) oldToNew)
      -- sh_info is a section index for relocation sections only
      remapInfo ty i = if ty == shtRela && i /= 0 then fromIntegral (M.findWithDefault 0 (fromIntegral i) oldToNew) else i
      shdr nm ty fl ad off sz ln inf al es = putU32 nm <> putU32 ty <> putU64 fl <> putU64 ad <> putU64 off <> putU64 sz <> putU32 ln <> putU32 inf <> putU64 al <> putU64 es
      shdrBytes = toStrict (mconcat (map shdrFor (zip [0 :: Int ..] newList)))
      trailer = B.concat (trailerParts ++ [shdrBytes])
      pieceOffsetOf a = case [p | p <- plPieces pl, a >= pcVmStart p, a <= pcVmEnd p] of
        p : _ -> Just (pcFileOff p + (min a (pcFileEnd p) - pcVmStart p))
        [] -> Nothing
  -- Program headers.
  let (phdrAddr, phdrRoom) = plPhdrArea pl
      prot p = (if p .&. 1 /= 0 then 4 else 0) .|. (if p .&. 2 /= 0 then 2 else 0) .|. (if p .&. 4 /= 0 then 1 else 0) :: Word32
      loadPhdrs =
        [ Phdr ptLoad (prot (segInitProt (pcSegment p))) (pcFileOff p) (pcVmStart p) (pcVmStart p) (pcFileEnd p - pcVmStart p) (pcVmEnd p - pcVmStart p) pageSize
          | p <- plPieces pl
        ]
      otherPhdrs =
        [ ph {phOffset = fromMaybe (phOffset ph) (newFileOffsetOf pl (phVaddr ph))}
          | ph <- elfPhdrs e,
            phType ph /= ptLoad,
            phType ph /= ptPhdr
        ]
      nph = length loadPhdrs + length otherPhdrs + 1
      phdrHeader = Phdr ptPhdr 4 0 phdrAddr phdrAddr (56 * fromIntegral nph) (56 * fromIntegral nph) 8
      phdrs = phdrHeader : [ph | ph <- otherPhdrs, phType ph == 3] ++ loadPhdrs ++ [ph | ph <- otherPhdrs, phType ph /= 3]
      phdrBytes = toStrict (mconcat [putU32 (phType p) <> putU32 (phFlags p) <> putU64 (phOffset p) <> putU64 (phVaddr p) <> putU64 (phPaddr p) <> putU64 (phFilesz p) <> putU64 (phMemsz p) <> putU64 (phAlign p) | p <- phdrs])
  when (fromIntegral (B.length phdrBytes) > phdrRoom) $
    fail ("the program headers need " ++ show (B.length phdrBytes) ++ " bytes but " ++ show phdrRoom ++ " were reserved")
  phdrFileOff <- maybe (fail "the program header area has no file offset") pure (newFileOffsetOf pl phdrAddr)
  -- fix the PT_PHDR entry's offset now that it is known
  let phdrBytes' = toStrict (mconcat [putU32 (phType p) <> putU32 (phFlags p) <> putU64 (if phType p == ptPhdr then phdrFileOff else phOffset p) <> putU64 (phVaddr p) <> putU64 (phPaddr p) <> putU64 (phFilesz p) <> putU64 (phMemsz p) <> putU64 (phAlign p) | p <- phdrs])
  entry <- either fail pure (relocate an pl (elfEntry e))
  -- Dynamic symbols: defined ones move with their definitions; one in a
  -- removed definition becomes undefined, so dlsym does not find it.
  let dynFixes = case elfDynsymIx e of
        Nothing -> []
        Just ix ->
          let s = elfShdrs e !! ix
           in [ (fileOff, newV, newShndx)
                | (k, (sym, _)) <- zip [0 :: Int ..] (symbolsOf e ix),
                  stShndx sym /= 0 && stShndx sym < 0xff00,
                  let oldOff = fromIntegral (shOffset s) + 24 * k,
                  Just fileOff <- [newFileOffsetOf pl (shAddr s + fromIntegral (24 * k))],
                  let (newV, newShndx) = case relocate an pl (stValue sym) of
                        Right v -> (v, remapShndx (stShndx sym) v)
                        Left _ -> (0, 0),
                  oldOff >= 0
              ]
      -- every relocation's slot moves with its definition (a bind can sit in
      -- a Haskell closure, the payload of a @foreign import "&f"@); a
      -- relative relocation's addend is relocated too; one whose slot was
      -- removed becomes a no-op relocation
      relTy = relativeType (elfMachine e)
      relaFixes =
        [ (fileOff, slot', addend')
          | sh <- elfShdrs e,
            shType sh == shtRela,
            shFlags sh .&. shfAlloc /= 0,
            k <- [0 .. fromIntegral (shSize sh `div` 24) - 1],
            let o = fromIntegral (shOffset sh) + 24 * k,
            let ty = fromIntegral (u64 orig (o + 8) .&. 0xffffffff) :: Word32,
            Just fileOff <- [newFileOffsetOf pl (shAddr sh + fromIntegral (24 * k))],
            let slot = u64 orig o,
            let addend = u64 orig (o + 16),
            let (slot', addend') = case newAddrOf an pl slot of
                  Just s'
                    | ty /= relTy -> (Just s', addend)
                    | Right a' <- relocate an pl addend -> (Just s', a')
                  _ -> (Nothing, 0)
        ]
  unless (null (elfRelr e)) $ fail "RELR relocations are not supported yet"
  let finish ptr = do
        -- the ELF header: new program and section header tables
        pokeByteOff ptr 24 entry
        pokeByteOff ptr 32 phdrFileOff
        pokeByteOff ptr 40 shoff
        pokeByteOff ptr 56 (fromIntegral nph :: Word16)
        pokeByteOff ptr 60 (fromIntegral count :: Word16)
        pokeByteOff ptr 62 (fromIntegral (count - 1) :: Word16)
        BU.unsafeUseAsCString phdrBytes' $ \p -> copyBytes (ptr `plusPtr` fromIntegral phdrFileOff) (castPtr p) (B.length phdrBytes')
        forM_ dynFixes $ \(off, v, shndx) -> do
          pokeByteOff ptr (fromIntegral off + 6) (shndx :: Word16)
          pokeByteOff ptr (fromIntegral off + 8) (v :: Word64)
        forM_ relaFixes $ \(off, mslot, addend) -> case mslot of
          Just slot -> do
            pokeByteOff ptr (fromIntegral off) slot
            pokeByteOff ptr (fromIntegral off + 16) addend
          Nothing -> do
            -- the slot is gone: a relative relocation of the scratch word, or a no-op
            pokeByteOff ptr (fromIntegral off) (fst (plDataArea pl))
            pokeByteOff ptr (fromIntegral off + 8) (fromIntegral relTy :: Word64)
            pokeByteOff ptr (fromIntegral off + 16) (0 :: Word64)
      stats =
        Stats
          { stSymbolsKept = length symsKept,
            stSymbolsDropped = allSyms - length symsKept,
            stRebases = length [() | (_, Just _, _) <- relaFixes],
            stBinds = length (imBinds im),
            stExports = 0,
            stLinkeditBytes = B.length trailer,
            stFileBytes = 0
          }
  pure (trailer, finish, stats)

_unusedElfWrite :: V.Vector Int -> [Int]
_unusedElfWrite = sortOn id . V.toList
