-- | Reading a 64-bit Mach-O executable: the header, the load commands, the
-- symbol table and the link-edit streams dyld reads (rebases, binds,
-- exports, function starts). Encoders for the streams the tool regenerates
-- live here too, next to their decoders.
module Minify.MachO
  ( MachO (..),
    LoadCommand (..),
    MSegment (..),
    MSection (..),
    Symtab (..),
    Dysymtab (..),
    DyldInfo (..),
    Nlist (..),
    parseMachO,
    loadMachO,
    machoSymbols,
    symbolName,
    sectionsInOrder,
    segments,
    cpuArm64,
    cpuX86_64,
    -- * Rebases, binds, exports, function starts
    Rebase (..),
    decodeRebases,
    encodeRebases,
    Bind (..),
    decodeBinds,
    encodeBinds,
    Export (..),
    decodeExports,
    encodeExports,
    decodeFunctionStarts,
    encodeFunctionStarts,
    -- * Byte helpers
    module Minify.Bytes,
    -- * Constants
    lcSegment64,
    lcSymtab,
    lcDysymtab,
    lcDyldInfoOnly,
    lcDyldInfo,
    lcFunctionStarts,
    lcDataInCode,
    lcCodeSignature,
    lcMain,
    lcUuid,
  )
where

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Int
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Word
import Minify.Bytes
import qualified Minify.Format as F

-- * Byte helpers: re-exported from "Minify.Bytes"

-- * Load commands

lcSegment64, lcSymtab, lcDysymtab, lcDyldInfo, lcDyldInfoOnly, lcFunctionStarts, lcDataInCode, lcCodeSignature, lcMain, lcUuid :: Word32
lcSegment64 = 0x19
lcSymtab = 0x2
lcDysymtab = 0xb
lcDyldInfo = 0x22
lcDyldInfoOnly = 0x80000022
lcFunctionStarts = 0x26
lcDataInCode = 0x29
lcCodeSignature = 0x1d
lcMain = 0x80000028
lcUuid = 0x1b

data MSection = MSection
  { secName :: !B.ByteString,
    secSegName :: !B.ByteString,
    secAddr :: !Word64,
    secSize :: !Word64,
    secOffset :: !Word32,
    secAlign :: !Word32,
    secRelOff :: !Word32,
    secNReloc :: !Word32,
    secFlags :: !Word32,
    secReserved1 :: !Word32,
    secReserved2 :: !Word32,
    secReserved3 :: !Word32
  }
  deriving (Show)

data MSegment = MSegment
  { segName :: !B.ByteString,
    segVmAddr :: !Word64,
    segVmSize :: !Word64,
    segFileOff :: !Word64,
    segFileSize :: !Word64,
    segMaxProt :: !Word32,
    segInitProt :: !Word32,
    segFlags :: !Word32,
    segSections :: [MSection]
  }
  deriving (Show)

data Symtab = Symtab {symOff :: !Word32, symNsyms :: !Word32, strOff :: !Word32, strSize :: !Word32}
  deriving (Show)

-- | The 18 words of LC_DYSYMTAB after cmd and cmdsize, in order.
newtype Dysymtab = Dysymtab [Word32] deriving (Show)

data DyldInfo = DyldInfo
  { diCmd :: !Word32,
    rebaseOff :: !Word32,
    rebaseSize :: !Word32,
    bindOff :: !Word32,
    bindSize :: !Word32,
    weakBindOff :: !Word32,
    weakBindSize :: !Word32,
    lazyBindOff :: !Word32,
    lazyBindSize :: !Word32,
    exportOff :: !Word32,
    exportSize :: !Word32
  }
  deriving (Show)

data LoadCommand
  = LCSegment MSegment
  | LCSymtab Symtab
  | LCDysymtab Dysymtab
  | LCDyldInfo DyldInfo
  | -- | cmd, dataoff, datasize (LC_FUNCTION_STARTS, LC_DATA_IN_CODE, LC_CODE_SIGNATURE, ...)
    LCLinkeditData !Word32 !Word32 !Word32
  | -- | entryoff, stacksize
    LCMain !Word64 !Word64
  | -- | any other command, as its complete bytes
    LCOther !Word32 !B.ByteString
  deriving (Show)

data MachO = MachO
  { moCpuType :: !Word32,
    moCpuSubtype :: !Word32,
    moFileType :: !Word32,
    moFlags :: !Word32,
    moReserved :: !Word32,
    moCommands :: [LoadCommand],
    moBytes :: !B.ByteString
  }

parseMachO :: B.ByteString -> Either String MachO
parseMachO b
  | B.length b < 32 = Left "file too short for a Mach-O header"
  | u32 b 0 /= 0xfeedfacf = Left "not a 64-bit little-endian Mach-O file"
  | otherwise =
      let ncmds = fromIntegral (u32 b 16)
          cmds = go 32 ncmds
       in Right
            MachO
              { moCpuType = u32 b 4,
                moCpuSubtype = u32 b 8,
                moFileType = u32 b 12,
                moFlags = u32 b 24,
                moReserved = u32 b 28,
                moCommands = cmds,
                moBytes = b
              }
  where
    go :: Int -> Int -> [LoadCommand]
    go _ 0 = []
    go off n =
      let cmd = u32 b off
          size = fromIntegral (u32 b (off + 4))
          raw = B.take size (B.drop off b)
          lc
            | cmd == lcSegment64 = LCSegment (parseSegment raw)
            | cmd == lcSymtab = LCSymtab (Symtab (u32 raw 8) (u32 raw 12) (u32 raw 16) (u32 raw 20))
            | cmd == lcDysymtab = LCDysymtab (Dysymtab [u32 raw (8 + 4 * i) | i <- [0 .. 17]])
            | cmd == lcDyldInfo || cmd == lcDyldInfoOnly =
                LCDyldInfo (DyldInfo cmd (u32 raw 8) (u32 raw 12) (u32 raw 16) (u32 raw 20) (u32 raw 24) (u32 raw 28) (u32 raw 32) (u32 raw 36) (u32 raw 40) (u32 raw 44))
            | cmd `elem` [lcFunctionStarts, lcDataInCode, lcCodeSignature, 0x2b, 0x2e, 0x33, 0x34, 0x80000034] =
                -- LC_DYLIB_CODE_SIGN_DRS, LC_LINKER_OPTIMIZATION_HINT, LC_DYLD_EXPORTS_TRIE, LC_DYLD_CHAINED_FIXUPS
                LCLinkeditData cmd (u32 raw 8) (u32 raw 12)
            | cmd == lcMain = LCMain (u64 raw 8) (u64 raw 16)
            | otherwise = LCOther cmd raw
       in lc : go (off + size) (n - 1)

parseSegment :: B.ByteString -> MSegment
parseSegment raw =
  MSegment
    { segName = cstringAt (B.take 16 (B.drop 8 raw)) 0,
      segVmAddr = u64 raw 24,
      segVmSize = u64 raw 32,
      segFileOff = u64 raw 40,
      segFileSize = u64 raw 48,
      segMaxProt = u32 raw 56,
      segInitProt = u32 raw 60,
      segFlags = u32 raw 68,
      segSections = [parseSection (B.drop (72 + 80 * i) raw) | i <- [0 .. fromIntegral (u32 raw 64) - 1]]
    }

parseSection :: B.ByteString -> MSection
parseSection s =
  MSection
    { secName = cstringAt (B.take 16 s) 0,
      secSegName = cstringAt (B.take 16 (B.drop 16 s)) 0,
      secAddr = u64 s 32,
      secSize = u64 s 40,
      secOffset = u32 s 48,
      secAlign = u32 s 52,
      secRelOff = u32 s 56,
      secNReloc = u32 s 60,
      secFlags = u32 s 64,
      secReserved1 = u32 s 68,
      secReserved2 = u32 s 72,
      secReserved3 = u32 s 76
    }

segments :: MachO -> [MSegment]
segments mo = [s | LCSegment s <- moCommands mo]

cpuArm64, cpuX86_64 :: Word32
cpuArm64 = 0x0100000c
cpuX86_64 = 0x01000007

-- | The format-neutral view of a Mach-O executable (see "Minify.Format").
loadMachO :: MachO -> Either String (F.Loaded, [Rebase])
loadMachO mo = do
  arch <-
    if
      | moCpuType mo == cpuArm64 -> Right F.Arm64
      | moCpuType mo == cpuX86_64 -> Right F.X86_64
      | otherwise -> Left ("unsupported cpu type " ++ show (moCpuType mo))
  (st, nls) <- maybe (Left "no symbol table: the file is stripped") Right (machoSymbols mo)
  let msegs = segments mo
      b = moBytes mo
      role sn n
        | sn == "__TEXT" && n == "__text" = F.Code
        | sn == "__TEXT" && n == "__cstring" = F.Strings
        | sn == "__TEXT" && n == "__const" = F.Const
        | sn == "__DATA" && n == "__data" = F.Data
        | otherwise = F.Other
      sects =
        [ F.Section
            { F.secName = secName s,
              F.secSegName = secSegName s,
              F.secAddr = secAddr s,
              F.secSize = secSize s,
              F.secOffset = fromIntegral (secOffset s),
              F.secNoBits = secFlags s .&. 0xff == 1,
              F.secRole = role (secSegName s) (secName s),
              F.secOrig = i,
              F.secRaw = F.MachOSect (secAlign s) (secRelOff s) (secNReloc s) (secFlags s) (secReserved1 s) (secReserved2 s) (secReserved3 s)
            }
          | (i, s) <- zip [0 ..] (sectionsInOrder mo)
        ]
      sectsOf sg = [sc | sc <- sects, F.secSegName sc == segName sg]
      segRole sg
        | segName sg == "__TEXT" = F.SegText
        | segName sg == "__LINKEDIT" = F.SegLinkedit
        | segName sg == "__PAGEZERO" = F.SegPageZero
        | "__DATA" `B.isPrefixOf` segName sg = F.SegData
        | otherwise = F.SegOther
      segs =
        [ F.Segment
            { F.segName = segName sg,
              F.segRole = segRole sg,
              F.segVmAddr = segVmAddr sg,
              F.segVmSize = segVmSize sg,
              F.segFileOff = segFileOff sg,
              F.segFileSize = segFileSize sg,
              F.segMaxProt = segMaxProt sg,
              F.segInitProt = segInitProt sg,
              F.segFlags = segFlags sg,
              F.segSections = sectsOf sg,
              F.segOrig = i
            }
          | (i, sg) <- zip [0 ..] msegs
        ]
      classify nl name
        | nType nl .&. 0x0e /= 0x0e = F.SymIgnore
        | nType nl .&. 0x01 /= 0 = F.SymExternal
        | "_dsp_" `B.isPrefixOf` name && "_dsp" `B.isSuffixOf` name = F.SymIgnore
        | F.ghcLocalLabel name = F.SymLocalHs
        | otherwise = F.SymLocalC
      syms =
        [ F.Symbol {F.syName = name, F.syValue = nValue nl, F.syDefined = nType nl .&. 0x0e == 0x0e, F.syClass = classify nl name, F.syOrig = i}
          | (i, nl) <- zip [0 ..] nls,
            let name = symbolName mo st nl
        ]
      dyld = case [d | LCDyldInfo d <- moCommands mo] of
        d : _ -> Just d
        [] -> Nothing
      slice off sz = B.take (fromIntegral sz) (B.drop (fromIntegral off) b)
      rebaseRecs = maybe [] (\d -> decodeRebases (slice (rebaseOff d) (rebaseSize d))) dyld
      segAddr i = segVmAddr (msegs !! i)
      slots = [segAddr (rbSeg r) + rbOff r | r <- rebaseRecs]
      binds = case dyld of
        Nothing -> []
        Just d ->
          [ (segAddr (bdSeg bd) + bdOff bd, bdSymbol bd)
            | (o, sz) <- [(bindOff d, bindSize d), (lazyBindOff d, lazyBindSize d), (weakBindOff d, weakBindSize d)],
              sz > 0,
              bd <- decodeBinds (slice o sz)
          ]
  pure
    ( F.Loaded
        { F.ldFormat = F.MachOFormat,
          F.ldArch = arch,
          F.ldSlots = Just slots,
          F.ldBinds = binds,
          F.ldAbsFields = Nothing,
          F.ldPageSize = if arch == F.Arm64 then 16384 else 4096,
          F.ldPie = True,
          F.ldSegments = segs,
          F.ldSections = sects,
          F.ldSymbols = syms,
          F.ldRaw = F.RawMachO
        },
      rebaseRecs
    )

-- | Sections in load-command order; a symbol's n_sect is a 1-based index
-- into this list.
sectionsInOrder :: MachO -> [MSection]
sectionsInOrder = concatMap segSections . segments

-- * Symbols

data Nlist = Nlist
  { nStrx :: !Word32,
    nType :: !Word8,
    nSect :: !Word8,
    nDesc :: !Word16,
    nValue :: !Word64
  }
  deriving (Show)

machoSymbols :: MachO -> Maybe (Symtab, [Nlist])
machoSymbols mo = case [s | LCSymtab s <- moCommands mo] of
  [st] ->
    let b = moBytes mo
        base = fromIntegral (symOff st)
        n = fromIntegral (symNsyms st)
     in Just (st, [Nlist (u32 b o) (u8 b (o + 4)) (u8 b (o + 5)) (u16 b (o + 6)) (u64 b (o + 8)) | i <- [0 .. n - 1], let o = base + 16 * i])
  _ -> Nothing

symbolName :: MachO -> Symtab -> Nlist -> B.ByteString
symbolName mo st nl = cstringAt (moBytes mo) (fromIntegral (strOff st) + fromIntegral (nStrx nl))

-- * Rebases

-- | A pointer slot dyld slides: segment index, offset in the segment, type (1 = pointer).
data Rebase = Rebase {rbSeg :: !Int, rbOff :: !Word64, rbType :: !Word8}
  deriving (Show, Eq)

decodeRebases :: B.ByteString -> [Rebase]
decodeRebases b = go 0 0 0 1
  where
    n = B.length b
    go :: Int -> Int -> Word64 -> Word8 -> [Rebase]
    go i seg off ty
      | i >= n = []
      | otherwise =
          let w = u8 b i
              op = w .&. 0xf0
              imm = w .&. 0x0f
           in case op of
                0x00 -> []
                0x10 -> go (i + 1) seg off imm
                0x20 -> let (v, i') = getUleb b (i + 1) in go i' (fromIntegral imm) v ty
                0x30 -> let (v, i') = getUleb b (i + 1) in go i' seg (off + v) ty
                0x40 -> go (i + 1) seg (off + fromIntegral imm * 8) ty
                0x50 -> doN (fromIntegral imm) 0 (i + 1) seg off ty
                0x60 -> let (c, i') = getUleb b (i + 1) in doN (fromIntegral c) 0 i' seg off ty
                0x70 -> let (v, i') = getUleb b (i + 1) in Rebase seg off ty : go i' seg (off + 8 + v) ty
                0x80 ->
                  let (c, i1) = getUleb b (i + 1)
                      (sk, i2) = getUleb b i1
                   in doN (fromIntegral c) sk i2 seg off ty
                _ -> error ("rebase opcode " ++ show w ++ " at " ++ show i)
    doN :: Int -> Word64 -> Int -> Int -> Word64 -> Word8 -> [Rebase]
    doN 0 _ i seg off ty = go i seg off ty
    doN c sk i seg off ty = Rebase seg off ty : doN (c - 1) sk i seg (off + 8 + sk) ty

-- | Encodes rebases grouped by segment, in the order given within a segment
-- (callers pass them sorted by offset).
encodeRebases :: [Rebase] -> BB.Builder
encodeRebases rbs = BB.word8 0x11 <> mconcat (map segRun (groupSeg (sortOn (\r -> (rbSeg r, rbOff r)) rbs))) <> BB.word8 0x00
  where
    groupSeg [] = []
    groupSeg (r : rs) = let (same, rest) = span ((== rbSeg r) . rbSeg) rs in (r : same) : groupSeg rest
    segRun [] = mempty
    segRun (r : rs) = BB.word8 (0x20 .|. fromIntegral (rbSeg r)) <> uleb (rbOff r) <> emit (rbOff r) (r : rs)
    -- emit a run starting with the cursor at the first slot
    emit _ [] = mempty
    emit cur (r : rs)
      | rbOff r /= cur = BB.word8 0x30 <> uleb (rbOff r - cur) <> emit (rbOff r) (r : rs)
      | otherwise =
          let (consecutive, rest) = countConsecutive (rbOff r) (r : rs)
              k = fromIntegral consecutive :: Word64
           in (if consecutive < 16 then BB.word8 (0x50 .|. fromIntegral consecutive) else BB.word8 0x60 <> uleb k)
                <> emit (rbOff r + 8 * k) rest
    countConsecutive :: Word64 -> [Rebase] -> (Int, [Rebase])
    countConsecutive _ [] = (0, [])
    countConsecutive at (r : rs)
      | rbOff r == at = let (c, rest) = countConsecutive (at + 8) rs in (c + 1, rest)
      | otherwise = (0, r : rs)

-- * Binds

data Bind = Bind
  { bdSeg :: !Int,
    bdOff :: !Word64,
    bdType :: !Word8,
    -- | positive: dylib ordinal; 0 self; -1 main executable; -2 flat lookup; -3 weak lookup
    bdLib :: !Int,
    bdFlags :: !Word8,
    bdSymbol :: !B.ByteString,
    bdAddend :: !Int64
  }
  deriving (Show, Eq)

-- | Decodes a bind or lazy-bind stream (DONE separates lazy records and ends
-- a regular stream; both are read to the end of the bytes).
decodeBinds :: B.ByteString -> [Bind]
decodeBinds b = go 0 (Bind 0 0 1 0 0 B.empty 0)
  where
    n = B.length b
    go i st
      | i >= n = []
      | otherwise =
          let w = u8 b i
              op = w .&. 0xf0
              imm = w .&. 0x0f
           in case op of
                0x00 -> go (i + 1) st
                0x10 -> go (i + 1) st {bdLib = fromIntegral imm}
                0x20 -> let (v, i') = getUleb b (i + 1) in go i' st {bdLib = fromIntegral v}
                0x30 -> go (i + 1) st {bdLib = if imm == 0 then 0 else fromIntegral imm - 16}
                0x40 -> let s = cstringAt b (i + 1) in go (i + 2 + B.length s) st {bdFlags = imm, bdSymbol = s}
                0x50 -> go (i + 1) st {bdType = imm}
                0x60 -> let (v, i') = getSleb b (i + 1) in go i' st {bdAddend = v}
                0x70 -> let (v, i') = getUleb b (i + 1) in go i' st {bdSeg = fromIntegral imm, bdOff = v}
                0x80 -> let (v, i') = getUleb b (i + 1) in go i' st {bdOff = bdOff st + v}
                0x90 -> st : go (i + 1) st {bdOff = bdOff st + 8}
                0xA0 -> let (v, i') = getUleb b (i + 1) in st : go i' st {bdOff = bdOff st + 8 + v}
                0xB0 -> st : go (i + 1) st {bdOff = bdOff st + 8 + fromIntegral imm * 8}
                0xC0 ->
                  let (c, i1) = getUleb b (i + 1)
                      (sk, i2) = getUleb b i1
                      slots = [st {bdOff = bdOff st + fromIntegral k * (8 + sk)} | k <- [0 .. c - 1]]
                   in slots ++ go i2 st {bdOff = bdOff st + c * (8 + sk)}
                _ -> error ("bind opcode " ++ show w ++ " at " ++ show i)

-- | Encodes binds as an ordinary (non-lazy) stream, one full record each.
encodeBinds :: [Bind] -> BB.Builder
encodeBinds bds = mconcat (map one (sortOn (\r -> (bdSeg r, bdOff r)) bds)) <> BB.word8 0x00
  where
    one Bind {..} =
      lib
        <> BB.word8 (0x40 .|. bdFlags)
        <> BB.byteString bdSymbol
        <> BB.word8 0
        <> BB.word8 (0x50 .|. bdType)
        <> BB.word8 0x60
        <> sleb bdAddend
        <> BB.word8 (0x70 .|. fromIntegral bdSeg)
        <> uleb bdOff
        <> BB.word8 0x90
      where
        lib
          | bdLib < 0 = BB.word8 (0x30 .|. fromIntegral (bdLib + 16))
          | bdLib == 0 = BB.word8 0x30
          | bdLib < 16 = BB.word8 (0x10 .|. fromIntegral bdLib)
          | otherwise = BB.word8 0x20 <> uleb (fromIntegral bdLib)

-- * Exports

-- | An exported symbol: name, flags, and the value (an offset from the
-- header for the ordinary kinds). Re-exports and stub-and-resolver entries
-- keep their extra data verbatim.
data Export = Export
  { exName :: !B.ByteString,
    exFlags :: !Word64,
    exAddr :: !Word64,
    -- | for a re-export: (ordinal, imported name); for stub-and-resolver: resolver offset in exAddr2
    exOther :: Maybe (Word64, B.ByteString)
  }
  deriving (Show)

decodeExports :: B.ByteString -> [Export]
decodeExports b
  | B.null b = []
  | otherwise = node 0 B.empty
  where
    node i prefix =
      let (tsize, i1) = getUleb b i
          here
            | tsize == 0 = []
            | otherwise =
                let (flags, i2) = getUleb b i1
                 in if flags .&. 0x08 /= 0 -- EXPORT_SYMBOL_FLAGS_REEXPORT
                      then
                        let (ordv, i3) = getUleb b i2
                            s = cstringAt b i3
                         in [Export prefix flags 0 (Just (ordv, s))]
                      else
                        let (addr, i3) = getUleb b i2
                         in if flags .&. 0x10 /= 0 -- STUB_AND_RESOLVER
                              then let (res, _) = getUleb b i3 in [Export prefix flags addr (Just (res, B.empty))]
                              else [Export prefix flags addr Nothing]
          childrenAt = i1 + fromIntegral tsize
          nchildren = fromIntegral (u8 b childrenAt) :: Int
          children = goChildren (childrenAt + 1) nchildren
          goChildren _ 0 = []
          goChildren j k =
            let s = cstringAt b j
                (off, j') = getUleb b (j + B.length s + 1)
             in node (fromIntegral off) (prefix <> s) ++ goChildren j' (k - 1)
       in here ++ children

-- | A radix tree over symbol names; children are keyed by the first byte of
-- their edge label, which is unique among a node's children.
data Trie = Trie (Maybe Export) (M.Map Word8 (B.ByteString, Trie))

-- | Builds the export trie. Node offsets are ULEB-encoded, so sizes are
-- iterated to a fixed point as ld64 does.
encodeExports :: [Export] -> BB.Builder
encodeExports exps = render
  where
    trie = foldl' (\t e -> insertT t (exName e) e) (Trie Nothing M.empty) exps
    insertT :: Trie -> B.ByteString -> Export -> Trie
    insertT (Trie t kids) name e
      | B.null name = Trie (Just e) kids
      | otherwise = case M.lookup (B.head name) kids of
          Nothing -> Trie t (M.insert (B.head name) (name, Trie (Just e) M.empty) kids)
          Just (k, old) ->
            let p = commonPrefix k name
                rest = B.drop (B.length p) name
             in if p == k
                  then Trie t (M.insert (B.head name) (k, insertT old rest e) kids)
                  else
                    let kRest = B.drop (B.length p) k
                        splitNode = Trie Nothing (M.singleton (B.head kRest) (kRest, old))
                     in Trie t (M.insert (B.head name) (p, insertT splitNode rest e) kids)
    commonPrefix a c = B.take (length (takeWhile id (B.zipWith (==) a c))) a
    -- Layout: nodes are numbered in preorder; a node's size depends on the
    -- ULEB offsets of its children, so offsets are recomputed until stable.
    nodesPre = preorder trie
    preorder t@(Trie _ kids) = t : concatMap (preorder . snd) (M.elems kids)
    nodeCount = length nodesPre
    childIdx :: M.Map Int [(B.ByteString, Int)]
    childIdx = snd (goIdx 0 trie)
      where
        goIdx i (Trie _ kids) =
          let (next, acc, pairs) = foldl' step (i + 1, M.empty, []) (M.elems kids)
              step (j, m, ps) (lbl, kid) = let (j', m') = goIdx j kid in (j', M.union m m', ps ++ [(lbl, j)])
           in (next, M.insert i pairs acc)
    terminals :: M.Map Int (Maybe Export)
    terminals = M.fromList (zip [0 ..] [t | Trie t _ <- nodesPre])
    sizeOf :: M.Map Int Word64 -> Int -> Int
    sizeOf offs i =
      let term = case M.findWithDefault Nothing i terminals of
            Nothing -> 1 -- uleb 0
            Just e ->
              let body = fromIntegral (BL.length (BB.toLazyByteString (termBody e)))
               in ulebLen body + fromIntegral body
          kids = M.findWithDefault [] i childIdx
          kidBytes = sum [B.length lbl + 1 + ulebLen (M.findWithDefault 0 j offs) | (lbl, j) <- kids]
       in term + 1 + kidBytes
    ulebLen :: Word64 -> Int
    ulebLen v = fromIntegral (BL.length (BB.toLazyByteString (uleb v)))
    termBody e = case exOther e of
      Just (ordv, s) | exFlags e .&. 0x08 /= 0 -> uleb (exFlags e) <> uleb ordv <> BB.byteString s <> BB.word8 0
      Just (res, _) -> uleb (exFlags e) <> uleb (exAddr e) <> uleb res
      Nothing -> uleb (exFlags e) <> uleb (exAddr e)
    offsetsFrom :: M.Map Int Word64 -> M.Map Int Word64
    offsetsFrom offs = M.fromList (zip [0 .. nodeCount - 1] (scanl (\o i -> o + fromIntegral (sizeOf offs i)) 0 [0 .. nodeCount - 1]))
    fixpoint offs = let offs' = offsetsFrom offs in if offs' == offs then offs else fixpoint offs'
    finalOffs = fixpoint (M.fromList [(i, 0) | i <- [0 .. nodeCount - 1]])
    render = mconcat [renderNode i | i <- [0 .. nodeCount - 1]]
    renderNode i =
      let term = case M.findWithDefault Nothing i terminals of
            Nothing -> BB.word8 0
            Just e ->
              let body = BB.toLazyByteString (termBody e)
               in uleb (fromIntegral (BL.length body)) <> BB.lazyByteString body
          kids = M.findWithDefault [] i childIdx
       in term <> BB.word8 (fromIntegral (length kids)) <> mconcat [BB.byteString lbl <> BB.word8 0 <> uleb (M.findWithDefault 0 j finalOffs) | (lbl, j) <- kids]

-- * Function starts

-- | Function start addresses, given the __TEXT segment's address.
decodeFunctionStarts :: Word64 -> B.ByteString -> [Word64]
decodeFunctionStarts base b = go 0 base
  where
    n = B.length b
    go i cur
      | i >= n = []
      | otherwise =
          let (d, i') = getUleb b i
           in if d == 0 then [] else let a = cur + d in a : go i' a

encodeFunctionStarts :: Word64 -> [Word64] -> BB.Builder
encodeFunctionStarts base addrs = go base (sortOn id addrs) <> BB.word8 0
  where
    go _ [] = mempty
    go cur (a : as) = uleb (a - cur) <> go a as

_unused :: BC.ByteString
_unused = BC.empty
