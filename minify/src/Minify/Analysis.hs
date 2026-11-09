-- Optimized in every build, --fast too: minifying is slow without it
{-# OPTIONS_GHC -O2 #-}

-- | Carving the file into definitions, finding every reference between
-- them, and deciding which ones are kept. See design.md.
module Minify.Analysis
  ( Kind (..),
    Def (..),
    Image (..),
    RawImage (..),
    RefKind (..),
    Analysis (..),
    Options (..),
    loadImage,
    analyze,
    defAt,
    fileOffsetOf,
    isCompactedSection,
    isCode,
    hsDisplay,
    tableSize,
    imFormat,
    imArch,
    imPie,
    imPageSize,
    imBinds,
  )
where

import Control.Monad
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Data.Int
import Data.List (isPrefixOf, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word
import qualified Minify.Arm64 as Arm64
import Minify.Bytes
import Minify.Elf (Elf, loadElf, parseElf)
import Minify.Format
import Minify.Hooks
import Minify.MachO (MachO, Rebase, loadMachO, parseMachO)
import Minify.Ref
import qualified Minify.X86 as X86
import Minify.ZEncoding

data Kind = Haskell | Fixed deriving (Eq, Show)

-- | A definition: the bytes from one symbol to the next, less the info
-- table that belongs to the next.
data Def = Def
  { dIndex :: !Int,
    dName :: !B.ByteString,
    -- | other symbols at the same address
    dAliases :: [B.ByteString],
    dHs :: Maybe HsName,
    -- | the symbol's address (the label; code starts here)
    dSym :: !Word64,
    -- | first byte (the info table, when there is one)
    dStart :: !Word64,
    dEnd :: !Word64,
    -- | index into 'imSections'
    dSect :: !Int,
    dKind :: !Kind,
    dHead :: !Int,
    -- | a local closure, static closure or string split off the definition named by its symbol, or named by a local label
    dLocal :: !Bool
  }
  deriving (Show)

-- | The format's own record, for the writer.
data RawImage = RawMachOImage MachO [Rebase] | RawElfImage Elf

data Image = Image
  { imLoaded :: Loaded,
    imBytes :: !B.ByteString,
    imSections :: V.Vector Section,
    -- | (address, size, file offset) of sections with file contents, by address
    imSectMap :: VU.Vector (Word64, Word64, Word64),
    imDefs :: V.Vector Def,
    imStarts :: VU.Vector Word64,
    imSymbols :: [Symbol],
    -- | pointer slots as (slot address, value stored there), by address: the
    -- loader's list when it has one, otherwise what the analysis found
    imSlots :: VU.Vector (Word64, Word64),
    imSegments :: [Segment],
    imRaw :: RawImage
  }

imFormat :: Image -> Format
imFormat = ldFormat . imLoaded

imArch :: Image -> Arch
imArch = ldArch . imLoaded

imPie :: Image -> Bool
imPie = ldPie . imLoaded

imPageSize :: Image -> Word64
imPageSize = ldPageSize . imLoaded

imBinds :: Image -> [(Word64, B.ByteString)]
imBinds = ldBinds . imLoaded

data Options = Options
  { optRemovePrefixes :: [String],
    optKeepDead :: Bool,
    -- | keep constructor name strings (con_desc) reachable; otherwise they are
    -- dropped and the fields point at one empty string
    optKeepConDesc :: Bool,
    -- | which sections' definitions are split at internal closures and strings (for debugging)
    optSplitText :: Bool,
    optSplitData :: Bool,
    optSplitStrings :: Bool,
    -- | debugging: which reference kinds may split data, and an address range the data splits are limited to
    optDataSplitKinds :: [RefKind],
    optSplitRange :: Maybe (Word64, Word64),
    -- | debugging: rewrite pointer slots in fixed data (else pin their targets); rewrite SRT fields in fixed code (else pin)
    optPatchFixedPointers :: Bool,
    optPatchFixedSrt :: Bool,
    -- | fold identical code and strings onto one copy
    optFold :: Bool,
    -- | debugging: only definitions starting in one of these ranges may be folded (empty: all)
    optFoldRange :: [(Word64, Word64)],
    -- | code labels known from elsewhere (verify passes the planned entry of
    -- every kept definition, since a label only a removed closure named
    -- would otherwise go undecoded in the output): address, has a table
    optExtraLabels :: [(Word64, Bool)],
    -- | debugging: every GOT entry roots what it holds, instead of being an edge from the code that loads it
    optGotRoots :: Bool,
    -- | debugging: definitions starting in these ranges are kept whatever reaches them
    optKeepRanges :: [(Word64, Word64)],
    -- | static closures known from elsewhere (verify passes every kept data definition's place)
    optExtraData :: [Word64],
    -- | debugging: definitions starting in these ranges stay where they are
    optPinRanges :: [(Word64, Word64)]
  }

data Analysis = Analysis
  { anImage :: Image,
    -- | references, per definition index: (field address, kind, target, anchor)
    anRefs :: V.Vector (VU.Vector (Word64, Word8, Word64, Word64)),
    anPinned :: VU.Vector Bool,
    anKept :: VU.Vector Bool,
    anCut :: VU.Vector Bool,
    -- | cut definitions that a kept one still refers to
    anCutReferenced :: VU.Vector Bool,
    anAnomalies :: [String],
    anRoots :: Int,
    anKeepConDesc :: Bool,
    anPinCounts :: M.Map String Int,
    -- | references from slots outside every definition: (slot, kind, target, anchor)
    anLoose :: VU.Vector (Word64, Word8, Word64, Word64),
    -- | the representative of each definition: itself, or the identical one it is folded onto
    anRep :: VU.Vector Int,
    -- | things counted along the way, for the report
    anCounts :: M.Map String Int,
    anNotes :: [String]
  }

hsDisplay :: Def -> String
hsDisplay d = maybe (BC.unpack (dName d)) displayName (dHs d) ++ (if dLocal d then " (local at " ++ hex (dStart d) ++ ")" else "")

-- * Loading

loadImage :: B.ByteString -> Either String Image
loadImage bytes = do
  (loaded, raw) <-
    if
      | B.length bytes >= 4 && u32 bytes 0 == 0xfeedfacf -> do
          mo <- parseMachO bytes
          (ld, rebases) <- loadMachO mo
          pure (ld, RawMachOImage mo rebases)
      | B.take 4 bytes == "\x7f\&ELF" -> do
          e <- parseElf bytes
          ld <- loadElf e
          pure (ld, RawElfImage e)
      | otherwise -> Left "not a Mach-O or ELF file"
  let sects = V.fromList (ldSections loaded)
      sectMap =
        VU.fromList
          ( sortOn
              (\(a, _, _) -> a)
              [(secAddr s, secSize s, secOffset s) | s <- V.toList sects, not (secNoBits s), secSize s > 0]
          )
      slots = case ldSlots loaded of
        Nothing -> VU.empty
        Just ss ->
          VU.fromList
            ( sortOn
                fst
                [ (a, v)
                | a <- S.toList (S.fromList ss),
                  Just off <- [fileOffsetOf' sectMap a],
                  let v = u64 bytes (fromIntegral off),
                  -- a null word is not a reference (this tool's own output
                  -- points the relocations of removed slots at one scratch word)
                  v /= 0
                ]
            )
      headOf a = maybe 0 (tableSize bytes . fromIntegral) (fileOffsetOf' sectMap a)
      typeAt a = maybe 0 (\o -> u32 bytes (fromIntegral o - 8)) (fileOffsetOf' sectMap a)
      byteAt a = maybe 0 (u8 bytes . fromIntegral) (fileOffsetOf' sectMap a)
      defs = carve headOf typeAt byteAt sects (ldSymbols loaded)
  when (V.null defs) (Left "no definitions found in the compactable sections")
  pure
    Image
      { imLoaded = loaded,
        imBytes = bytes,
        imSections = sects,
        imSectMap = sectMap,
        imDefs = defs,
        imStarts = VU.fromList (map dStart (V.toList defs)),
        imSymbols = ldSymbols loaded,
        imSlots = slots,
        imSegments = ldSegments loaded,
        imRaw = raw
      }

fileOffsetOf' :: VU.Vector (Word64, Word64, Word64) -> Word64 -> Maybe Word64
fileOffsetOf' sm a =
  let n = VU.length sm
      go lo hi
        | lo >= hi = lo
        | otherwise =
            let mid = (lo + hi) `div` 2
                (sa, _, _) = sm VU.! mid
             in if sa <= a then go (mid + 1) hi else go lo mid
      i = go 0 n - 1
   in if i < 0
        then Nothing
        else
          let (sa, sz, off) = sm VU.! i
           in if a < sa + sz then Just (off + (a - sa)) else Nothing

-- | The file offset of a virtual address, if it is backed by file bytes.
fileOffsetOf :: Image -> Word64 -> Maybe Word64
fileOffsetOf = fileOffsetOf' . imSectMap

-- | The definition containing an address.
defAt :: Image -> Word64 -> Maybe Def
defAt im a =
  let starts = imStarts im
      n = VU.length starts
      go lo hi
        | lo >= hi = lo
        | otherwise =
            let mid = (lo + hi) `div` 2
             in if starts VU.! mid <= a then go (mid + 1) hi else go lo mid
      i = go 0 n - 1
   in if i < 0
        then Nothing
        else let d = imDefs im V.! i in if a < dEnd d then Just d else Nothing

-- | Splits each compacted section into definitions at its symbols. The
-- first argument gives the size of the info table before a code label,
-- the second the closure type of that table.
carve :: (Word64 -> Int) -> (Word64 -> Word32) -> (Word64 -> Word8) -> V.Vector Section -> [Symbol] -> V.Vector Def
-- sections need not be in address order (the output's are not)
carve headOf typeAt byteAt sects syms = V.fromList (zipWith (\i d -> d {dIndex = i}) [0 ..] (sortOn dStart (concat perSection)))
  where
    -- symbols are filed by address, not by their section number, which a
    -- split section would make ambiguous
    perSection =
      [ carveSection si s [(syValue sy, syName sy, syClass sy) | sy <- syms, syDefined sy, syClass sy /= SymIgnore, syValue sy >= secAddr s, syValue sy < secAddr s + secSize s]
      | (si, s) <- zip [0 ..] (V.toList sects),
        isCompactedSection s
      ]
    carveSection si s named =
      let byAddr = M.toAscList (M.fromListWith (++) [(a, [(nm, cls)]) | (a, nm, cls) <- named])
          -- the name of a group: an external Haskell one if any, else any external, else the first
          pick nms = case [n | (n, SymExternal) <- nms, isJust (parseHsName n)] of
            n : _ -> (n, SymExternal, filter (/= n) (map fst nms))
            [] -> case [n | (n, SymExternal) <- nms] of
              n : _ -> (n, SymExternal, filter (/= n) (map fst nms))
              [] -> case nms of
                (n, c) : rest -> (n, c, map fst rest)
                [] -> error "empty symbol group"
          -- a local label with a return frame's table belongs to its procedure
          -- (conditional branches cross it); other local labels begin a
          -- definition that is Haskell's but has no module of its own
          starts0 =
            [ (a, nm, rest, hs, kind, hd, cls == SymLocalHs)
            | (a, nms) <- byAddr,
              let (nm, cls, rest) = pick nms,
              let hs = if cls == SymExternal then parseHsName nm else Nothing,
              let kind = if isJust hs || cls == SymLocalHs then Haskell else Fixed,
              let hd = if isCode s && infoLike nm then headOf a else 0,
              not (cls == SymLocalHs && isCode s && infoLike nm && (hd == 0 || isReturnTable (typeAt a)))
            ]
          -- The linker merges identical strings and shares suffixes: a
          -- symbol in a string section that is preceded by a byte other than
          -- NUL names the tail of the string before it, and the two must
          -- stay together. The merged definition is fixed if either was.
          starts
            | secRole s == Strings = mergeTails starts0
            | otherwise = starts0
          mergeTails (x@(a1, nm1, rest1, hs1, kind1, hd1, lcl1) : y@(a2, nm2, rest2, _, kind2, _, _) : more)
            | byteAt (a2 - 1) /= 0 =
                let kind' = if kind1 == Fixed || kind2 == Fixed then Fixed else Haskell
                 in mergeTails ((a1, nm1, rest1 ++ nm2 : rest2, hs1, kind', hd1, lcl1) : more)
            | otherwise = x : mergeTails (y : more)
          mergeTails xs = xs
          secStart = secAddr s
          secEnd = secAddr s + secSize s
          leadDef en = Def 0 (BC.pack ("<" ++ BC.unpack (secName s) ++ " lead>")) [] Nothing secStart secStart en si Fixed 0 False
          lead = case starts of
            (a, _, _, _, _, hd, _) : _ | fromIntegral a - fromIntegral hd > (fromIntegral secStart :: Int64) -> [leadDef (a - fromIntegral hd)]
            (_ : _) -> []
            -- a section (or a piece of one, in this tool's own output) with no symbol at all
            [] -> [leadDef secEnd | secEnd > secStart]
          mk (a, nm, rest, hs, kind, hd, lcl) next =
            let st = a - fromIntegral hd
                en = case next of
                  Just (a2, _, _, _, _, hd2, _) -> a2 - fromIntegral hd2
                  Nothing -> secEnd
             in Def 0 nm rest hs a st (max st en) si kind hd lcl
          defs = zipWith mk starts (map Just (drop 1 starts) ++ [Nothing])
       in lead ++ defs
    infoLike nm = any (`B.isSuffixOf` nm) ["_info", "_entry"]
    isReturnTable ty = ty >= 29 && ty <= 36

-- * Analysis

data Env = Env
  { eImage :: Image,
    eBytes :: B.ByteString,
    -- | the address range of the code: from the first code section to the
    -- end of the last (this tool's own output has several)
    eTextLo :: !Word64,
    eTextHi :: !Word64,
    -- | bytes per bitmap entry: 4 on AArch64, 1 on x86
    eGranule :: !Int,
    eVisited :: VUM.IOVector Bool,
    eTabled :: VUM.IOVector Bool,
    -- | words that belong to an info table or a jump table (known after the first pass)
    eIsTable :: VUM.IOVector Bool,
    -- | the first pass only discovers labels; its references and anomalies are discarded
    eFirstPass :: IORef Bool,
    -- | labels with info tables, collected by the first pass
    eTableLabels :: IORef [Word64],
    -- | address ranges fixed code or data points at: what they overlap stays put
    ePins :: IORef [(String, Word64, Word64)],
    -- | how many pins of each source: exact adrp, page adrp, branch, pointer, table field
    ePinCounts :: IORef (M.Map String Int),
    eRefs :: IORef (VUM.IOVector (Int, Word64, Word8, Word64, Word64)),
    -- | references from slots outside every definition (patched in place; roots)
    eLoose :: IORef [(Word64, Word8, Word64, Word64)],
    ePatchFixedSrt :: Bool,
    eRefCount :: IORef Int,
    eLabels :: IORef [(Word64, Bool)],
    -- | addresses in data that code or closures refer to, to be parsed as closures
    eDataLabels :: IORef [Word64],
    eClosuresDone :: IORef (S.Set Word64),
    -- | pointer slots found by parsing closures (when the loader has no list)
    eFoundSlots :: IORef [(Word64, Word64)],
    -- | jump tables found in read-only data: split points
    eJumpTables :: IORef [Word64],
    -- | jump tables waiting to have their entries read: definition, address, entry size, relative
    eTableQueue :: IORef [(Int, Word64, Int, Bool)],
    -- | first bytes of decoded instructions (with 'eVisited': the interior of one)
    eStarts :: VUM.IOVector Bool,
    -- | tables already queued in this pass
    eTablesSeen :: IORef (S.Set Word64),
    -- | every jump table's address, from both passes: a table's entries end where the next begins
    eTableStarts :: IORef (S.Set Word64),
    eCounts :: IORef (M.Map String Int),
    -- | observations worth printing that are not anomalies
    eNotes :: IORef [String],
    -- | the 32-bit absolute fields the relocations name (x86-64 with --emit-relocs)
    eAbsFields :: S.Set Word64,
    -- | labels the sweep of fixed code found, pushed again in every pass
    eSweepLabels :: IORef [(Word64, Bool)],
    eAnomalies :: IORef [String]
  }

analyze :: Options -> Image -> IO Analysis
analyze opts im0 = do
  let im = im0
      bytes = imBytes im
      codeSects = [sc | sc <- V.toList (imSections im), isCode sc, secSize sc > 0]
      textLo = if null codeSects then error "no code section" else minimum (map secAddr codeSects)
      textHi = maximum (map (\sc -> secAddr sc + secSize sc) codeSects)
      granule = if imArch im == Arm64 then 4 else 1
      nbits = fromIntegral (textHi - textLo) `div` granule + 1
      exactSlots = isJust (ldSlots (imLoaded im))
  visited <- VUM.replicate nbits False
  starts <- VUM.replicate nbits False
  tabled <- VUM.replicate nbits False
  isTable <- VUM.replicate nbits False
  firstPass <- newIORef True
  tableLabels <- newIORef []
  pins <- newIORef []
  pinCounts <- newIORef M.empty
  refs <- VUM.new (1 `shiftL` 20) >>= newIORef
  loose <- newIORef []
  refCount <- newIORef 0
  labels <- newIORef []
  dataLabels <- newIORef []
  closuresDone <- newIORef S.empty
  foundSlots <- newIORef []
  jumpTables <- newIORef []
  tableQueue <- newIORef []
  tablesSeen <- newIORef S.empty
  tableStarts <- newIORef S.empty
  counts <- newIORef M.empty
  notes <- newIORef []
  anomalies <- newIORef []
  sweepLabels <- newIORef []
  let absFields = maybe S.empty S.fromList (ldAbsFields (imLoaded im))
      env = Env im bytes textLo textHi granule visited tabled isTable firstPass tableLabels pins pinCounts refs loose (optPatchFixedSrt opts) refCount labels dataLabels closuresDone foundSlots jumpTables tableQueue starts tablesSeen tableStarts counts notes absFields sweepLabels anomalies
  -- Fixed code is swept for what it points at; the targets stay put.
  forM_ (V.toList (imDefs im)) $ \d ->
    when (dKind d == Fixed && isCode (imSections im V.! dSect d)) $ do
      let hk0 = mkHooks env d
          hk = hk0 {hkLabel = \a tbl -> modifyIORef' (eSweepLabels env) ((a, tbl) :)}
      case imArch im of
        Arm64 -> Arm64.sweepFixed hk (dStart d) (dEnd d)
        X86_64 -> X86.sweepFixed hk (dStart d) (dEnd d)
  -- Pointer slots are references wherever they are: a pointer in C data is
  -- an absolute address that can be rewritten like any other. A slot in
  -- no definition at all (the GOT, a C constant section) is a loose
  -- reference, patched in place and a root.
  let hasTable = looksLikeTable im
      -- a pointer into code names a closure's entry (with its table) or, in a
      -- file whose local symbols were stripped, a C function lying in a
      -- Haskell definition's region; the latter has nothing to parse
      pointerSlot slot value = case defAt im slot of
        Just d | dKind d == Haskell || optPatchFixedPointers opts -> do
          addRef env (dIndex d) slot RPointer value 0
          when (inText env value) (pushLabel env value (hasTable value))
        Just _ -> pinTargetAs env "pointer in fixed data" value
        Nothing | optPatchFixedPointers opts -> addLoose env slot RPointer value 0
        Nothing -> pinTargetAs env "pointer in fixed data" value
      -- Without a list from the loader, the pointers in fixed data are found
      -- by looking: a word that lands in a Haskell definition. In the GOT
      -- every word is an address, so those are rewritten; elsewhere an
      -- integer could look like one, so the target is pinned instead.
      scanFixedData gotOnly = do
        let isGot sc = secName sc `elem` [".got", ".got.plt"]
            wordsOf lo hi = [a | a <- [alignUp8 lo, alignUp8 lo + 8 .. hi - 8]]
            alignUp8 a = (a + 7) .&. complement 7
            consider got a = do
              let v = word64At' env a
              case defAt im (v .&. complement 7) of
                Just t | dKind t == Haskell -> do
                  if got
                    then do
                      addLoose env a RPointer v 0
                      if inText env v then pushLabel env v (hasTable v) else pushData env (v .&. complement 7)
                      bump env "GOT entries rewritten"
                    else do
                      pinTargetAs env "pointer in fixed data" v
                      pushData env (v .&. complement 7)
                _ -> pure ()
        forM_ (V.toList (imSections im)) $ \sc ->
          when (not (isCode sc) && not (secNoBits sc) && secSize sc > 0 && (isGot sc || (not gotOnly && not (isCompactedSection sc)))) $
            mapM_ (consider (isGot sc)) (wordsOf (secAddr sc) (secAddr sc + secSize sc))
        unless gotOnly $ forM_ (V.toList (imDefs im)) $ \d ->
          when (dKind d == Fixed && not (isCode (imSections im V.! dSect d))) $
            mapM_ (consider False) (wordsOf (dStart d) (dEnd d))
        -- C's unlabeled constants (a static array of pointers) lie in the
        -- region of whatever symbol precedes them, often a Haskell string:
        -- a word in such a region that looks like a pointer pins its target
        unless gotOnly $ forM_ (V.toList (imDefs im)) $ \d ->
          when (dKind d == Haskell && secRole (imSections im V.! dSect d) `elem` [Strings, Const]) $
            mapM_ (consider False) (wordsOf (dStart d) (dEnd d))
      -- the GOT's entries have no relocations of their own in an ELF
      -- executable (the linker made them), so they are looked at whether or
      -- not the loader lists the other slots
      seedPointers =
        if exactSlots
          then do
            VU.forM_ (imSlots im) (uncurry pointerSlot)
            when (imFormat im == ElfFormat) (scanFixedData True)
          else do
            scanFixedData False
            forM_ (V.toList (imDefs im)) $ \d ->
              when (dKind d == Haskell && secRole (imSections im V.! dSect d) == Data) (pushData env (dStart d))
  seedPointers
  -- Every Haskell code definition is decoded from its own label. The first
  -- passes find the labels and so the info tables, and the jump tables,
  -- which bound each other and so must all be known before any is read;
  -- they are repeated until no new table turns up. The last pass decodes
  -- with the tables known, so that a fall-through into one (after an
  -- exhaustive compare chain) stops instead of reading table words as code.
  let seed = do
        forM_ (V.toList (imDefs im)) $ \d ->
          when (dKind d == Haskell && isCode (imSections im V.! dSect d)) (pushLabel env (dSym d) (dHead d > 0))
        forM_ (optExtraLabels opts) $ \(a, tbl) -> pushLabel env a tbl
        forM_ (optExtraData opts) (pushData env)
        readIORef (eSweepLabels env) >>= mapM_ (uncurry (pushLabel env))
      reset = do
        writeIORef refCount 0
        writeIORef anomalies []
        writeIORef counts M.empty
        VUM.set visited False
        VUM.set starts False
        VUM.set tabled False
        -- the pointer references are re-added; the pins from the fixed sweep stay
        writeIORef (eLoose env) []
        writeIORef closuresDone S.empty
        writeIORef foundSlots []
        writeIORef jumpTables []
        writeIORef tableQueue []
        writeIORef tablesSeen S.empty
      -- every jump table is registered before any is read (x86)
      registerTables = when (imArch im == X86_64) $
        forM_ (V.toList (imDefs im)) $ \d ->
          when (dKind d == Haskell && isCode (imSections im V.! dSect d)) $ do
            let hk = mkHooks env d
            forM_ (X86.scanTables hk) $ \(t, size, relative) -> hkJumpTable hk t size relative
      discover = do
        before <- S.size <$> readIORef tableStarts
        registerTables
        seedPointers
        seed
        drain env
        after <- S.size <$> readIORef tableStarts
        when (after > before) (reset >> discover)
  discover
  tls <- readIORef tableLabels
  forM_ tls $ \l -> case fileOffsetOf im l of
    Just off -> do
      let hd = tableSize bytes (fromIntegral off)
      forM_ [0 .. hd `div` granule - 1] $ \k -> VUM.write isTable (textIndex env l - 1 - k) True
    Nothing -> pure ()
  writeIORef firstPass False
  reset
  registerTables
  seedPointers
  seed
  drain env
  -- References, by address.
  n <- readIORef refCount
  buf <- readIORef refs
  byAddr <- do
    v <- VU.freeze (VUM.slice 0 n buf)
    mv <- VU.thaw (VU.map (\(_, a, k, t, x) -> (a, k, t, x)) v)
    VA.sortBy (\(a, _, _, _) (b, _, _, _) -> compare a b) mv
    VU.unsafeFreeze mv
  anoms <- reverse <$> readIORef anomalies
  tls2 <- readIORef tableLabels
  found <- readIORef foundSlots
  jts <- readIORef jumpTables
  let slots = if exactSlots then imSlots im else VU.fromList (sortOn fst (S.toList (S.fromList found)))
      imS = im {imSlots = slots}
  -- Definitions are split where something inside them can live on its
  -- own: in code at a label with a function, thunk or constructor table (a
  -- local closure; return-point tables belong to their procedure), in data
  -- at a referenced closure start (a word that is itself a pointer slot),
  -- in strings at a constructor name or a jump table.
  let typeAt l = maybe 0 (\o -> u32 bytes (fromIntegral o - 8)) (fileOffsetOf im l)
      isSlot a = slotSearch 0 (VU.length slots)
        where
          slotSearch lo hi
            | lo >= hi = False
            | otherwise =
                let mid = (lo + hi) `div` 2
                    (sa, _) = slots VU.! mid
                 in if sa == a then True else if sa < a then slotSearch (mid + 1) hi else slotSearch lo mid
      sectOf d = imSections im V.! dSect d
      -- the range restricts only the section it lies in
      inRange t = case optSplitRange opts of
        Nothing -> True
        Just (lo, hi) -> case (defAt im t, defAt im lo) of
          (Just a, Just b) | dSect a == dSect b -> t >= lo && t < hi
          _ -> True
      textSplits =
        [ (l - fromIntegral hd, hd)
        | l <- tls2,
          Just d <- [defAt im l],
          dKind d == Haskell,
          l /= dSym d,
          inRange l,
          let ty = typeAt l,
          ty >= 1 && ty <= 22,
          Just o <- [fileOffsetOf im l],
          let hd = tableSize bytes (fromIntegral o),
          hd > 0,
          l - fromIntegral hd > dStart d
        ]
      refTargets = VU.toList byAddr
      dataSplits =
        [ (t', 0)
        | (_, k, t, _) <- refTargets,
          k `elem` map (fromIntegral . fromEnum) (optDataSplitKinds opts),
          let t' = t .&. complement 7,
          inRange t',
          Just d <- [defAt im t'],
          dKind d == Haskell,
          secRole (sectOf d) == Data,
          t' - dStart d >= 8,
          isSlot t'
        ]
      strSplits =
        [ (t, 0)
        | (_, k, t, _) <- refTargets,
          k == fromIntegral (fromEnum RConDesc),
          Just d <- [defAt im t],
          dKind d == Haskell,
          secRole (sectOf d) == Strings,
          t > dStart d
        ]
      tableSplits =
        [ (t, 0)
        | t <- jts,
          Just d <- [defAt im t],
          not (isCode (sectOf d)),
          t > dStart d
        ]
      splits = M.fromList ((if optSplitText opts then textSplits else []) ++ (if optSplitData opts then dataSplits else []) ++ (if optSplitStrings opts then strSplits else []) ++ tableSplits)
      refined = V.fromList (zipWith (\i d -> d {dIndex = i}) [0 ..] (concatMap (refine splits) (V.toList (imDefs im))))
      im' = imS {imDefs = refined, imStarts = VU.fromList (map dStart (V.toList refined))}
      ndefs' = V.length refined
      lowerAt a =
        let total = VU.length byAddr
            go lo hi
              | lo >= hi = lo
              | otherwise = let mid = (lo + hi) `div` 2; (x, _, _, _) = byAddr VU.! mid in if x < a then go (mid + 1) hi else go lo mid
         in go 0 total
      perDef = V.generate ndefs' $ \i ->
        let d = refined V.! i
            lo = lowerAt (dStart d)
            hi = lowerAt (dEnd d)
         in VU.slice lo (hi - lo) byAddr
  -- Pins: a definition overlapping a pinned range stays where it is.
  pinnedM <- VUM.replicate ndefs' False
  pinWhy <- newIORef M.empty
  ranges <- readIORef pins
  forM_ ranges $ \(why, lo, hi) -> do
    let firstPast = let go a b | a >= b = a | otherwise = let mid = (a + b) `div` 2 in if dEnd (refined V.! mid) <= lo then go (mid + 1) b else go a mid in go 0 ndefs'
        loop i
          | i >= ndefs' = pure ()
          | otherwise = do
              let d = refined V.! i
              when (dStart d < hi) $ do
                when (dKind d == Haskell) $ do
                  already <- VUM.read pinnedM i
                  unless already $ do
                    VUM.write pinnedM i True
                    modifyIORef' pinWhy (M.insertWith (+) (why ++ " in " ++ BC.unpack (secName (sectOf d))) (1 :: Int))
                loop (i + 1)
    loop firstPast
  forM_ [0 .. ndefs' - 1] $ \i -> do
    let a = dStart (refined V.! i)
    when (dKind (refined V.! i) == Haskell && any (\(lo, hi) -> a >= lo && a < hi) (optPinRanges opts)) (VUM.write pinnedM i True)
  pinnedV <- VU.freeze pinnedM
  pinCountsV <- readIORef pinWhy
  looseV <- readIORef loose
  countsV <- readIORef counts
  notesV <- reverse <$> readIORef notes
  -- Cut by prefix, then mark from the roots.
  -- Only code and closures are cut: a literal a kept definition still uses
  -- has nothing sensible to be replaced by, so it stays.
  let cut = VU.generate ndefs' $ \i ->
        let d = refined V.! i
            sect = sectOf d
         in dKind d == Haskell
              && (isCode sect || secRole sect == Data)
              && any (\p -> any (p `isPrefixOf`) (prefixForms d)) (optRemovePrefixes opts)
      -- A GOT entry is an edge, not a root: code that loads the slot reaches
      -- what it holds, and one that fixed code names (the sweep pinned it)
      -- roots its target. Any other loose slot (a table of closures in a
      -- constant section, indexed at run time) is a root.
      looseMap = M.fromList [(at, t) | (at, _, t, _) <- looseV]
      pinnedSlot at = any (\(_, lo, hi) -> at >= lo && at < hi) ranges
      gotRanges = [(secAddr sc, secAddr sc + secSize sc) | sc <- V.toList (imSections im), secName sc `elem` ["__got", ".got", ".got.plt"]]
      inGot at = any (\(lo, hi) -> at >= lo && at < hi) gotRanges
      looseTargets = [dIndex d | (at, _, t, _) <- looseV, optGotRoots opts || pinnedSlot at || not (inGot at), Just d <- [defAt im' t]]
      forced i = let a = dStart (refined V.! i) in any (\(lo, hi) -> a >= lo && a < hi) (optKeepRanges opts)
      roots = looseTargets ++ [i | i <- [0 .. ndefs' - 1], let d = refined V.! i, dKind d == Fixed || pinnedV VU.! i || forced i, not (cut VU.! i)]
      -- the definition a target lands in, through a loose slot if need be
      landing t = case defAt im' t of
        Just d -> Just d
        Nothing -> M.lookup t looseMap >>= defAt im'
      conDesc = fromIntegral (fromEnum RConDesc) :: Word8
      follows k = optKeepConDesc opts || k /= conDesc
  kept <- VUM.replicate ndefs' False
  cutRef <- VUM.replicate ndefs' False
  let mark [] = pure ()
      mark (i : rest) = do
        seen <- VUM.read kept i
        if seen
          then mark rest
          else do
            VUM.write kept i True
            let targets = [t | (_, k, t, _) <- VU.toList (perDef V.! i), follows k]
                nexts = flip mapMaybe targets $ \t -> case landing t of
                  Just d
                    | cut VU.! dIndex d -> Nothing
                    | otherwise -> Just (dIndex d)
                  Nothing -> Nothing
            forM_ targets $ \t -> case landing t of
              Just d | cut VU.! dIndex d -> VUM.write cutRef (dIndex d) True
              _ -> pure ()
            mark (nexts ++ rest)
  if optKeepDead opts
    then forM_ [0 .. ndefs' - 1] $ \i -> VUM.write kept i (not (cut VU.! i))
    else mark roots
  keptV <- VU.freeze kept
  cutRefV <- VU.freeze cutRef
  let repV = if optFold opts then fold im' bytes perDef keptV pinnedV cut (optFoldRange opts) else VU.generate ndefs' id
  pure
    Analysis
      { anImage = im',
        anRefs = perDef,
        anPinned = pinnedV,
        anKept = keptV,
        anCut = cut,
        anCutReferenced = cutRefV,
        anAnomalies = anoms,
        anRoots = length roots,
        anKeepConDesc = optKeepConDesc opts,
        anPinCounts = pinCountsV,
        anLoose = VU.fromList looseV,
        anRep = repV,
        anCounts = countsV,
        anNotes = notesV
      }

-- | Splits a definition at the given points (address to table size).
refine :: M.Map Word64 Int -> Def -> [Def]
refine splits d =
  let points = M.toAscList (fst (M.split (dEnd d) (snd (M.split (dStart d) splits))))
      ends = map fst (drop 1 points) ++ [dEnd d]
      firstEnd = maybe (dEnd d) fst (listToMaybe points)
      locals =
        [ d {dSym = p + fromIntegral hd, dStart = p, dEnd = e, dHead = hd, dLocal = True, dAliases = []}
        | ((p, hd), e) <- zip points ends
        ]
   in d {dEnd = firstEnd} : locals

-- | The forms a prefix is matched against: @Module.binding@ and
-- @package:Module.binding@.
prefixForms :: Def -> [String]
prefixForms d = case dHs d of
  Nothing -> []
  Just hs ->
    let m = zdecode (BC.unpack (hsModule hs)) ++ "." ++ zdecode (BC.unpack (hsBinding hs))
     in [m, packageBase (hsPackage hs) ++ ":" ++ m]

-- | The size of the info table that ends at a file offset, from the closure
-- type in it. Constructors carry a con_desc offset; functions carry their
-- type and arity, and the general ones also a slow-apply offset and a
-- bitmap; thunks and return frames carry only the standard table.
-- | Whether a label in code has an info table before it. The closure type
-- must be one a table can have, and the SRT field, unless the table is a
-- constructor's (where the field is the tag), must be zero or lead to a
-- place in the file: the bytes before a plain block of code (a jump
-- table's target) can pass the first test by chance, but not both.
looksLikeTable :: Image -> Word64 -> Bool
looksLikeTable im label = case fileOffsetOf im label of
  Nothing -> False
  Just o ->
    let off = fromIntegral o
        bytes = imBytes im
        ty = u32 bytes (off - 8)
        srt = i32 bytes (off - 4)
        target = fromIntegral (fromIntegral label + fromIntegral srt :: Int64)
     in tableSize bytes off > 0
          && ((ty >= 1 && ty <= 7) || srt == 0 || isJust (fileOffsetOf im target))

tableSize :: B.ByteString -> Int -> Int
tableSize bytes off
  | off < 40 = 0
  | otherwise =
      let ty = u32 bytes (off - 8)
       in if
            | ty >= 1 && ty <= 7 -> 24
            | ty >= 8 && ty <= 14 -> let ft = u32 bytes (off - 24) in if ft <= 1 then 40 else 24
            | ty >= 15 && ty <= 22 -> 16
            | ty == 28 -> 16 -- IND_STATIC: a top-level binding that is another closure
            | ty >= 29 && ty <= 36 -> 16
            | ty >= 55 && ty <= 57 -> 16
            | otherwise -> 0

inText :: Env -> Word64 -> Bool
inText env a = a >= eTextLo env && a < eTextHi env

textIndex :: Env -> Word64 -> Int
textIndex env a = fromIntegral (a - eTextLo env) `div` eGranule env

word64At' :: Env -> Word64 -> Word64
word64At' env a = case fileOffsetOf (eImage env) a of
  Just off | fromIntegral off + 8 <= B.length (eBytes env) -> u64 (eBytes env) (fromIntegral off)
  _ -> 0

anomaly :: Env -> String -> IO ()
anomaly env s = modifyIORef' (eAnomalies env) (s :)

bump :: Env -> String -> IO ()
bump env k = modifyIORef' (eCounts env) (M.insertWith (+) k 1)

addRef :: Env -> Int -> Word64 -> RefKind -> Word64 -> Word64 -> IO ()
addRef env di at kind target anchor = do
  n <- readIORef (eRefCount env)
  buf <- readIORef (eRefs env)
  buf' <-
    if n < VUM.length buf
      then pure buf
      else do
        b2 <- VUM.grow buf (VUM.length buf)
        writeIORef (eRefs env) b2
        pure b2
  VUM.write buf' n (di, at, fromIntegral (fromEnum kind), target, anchor)
  writeIORef (eRefCount env) (n + 1)

addLoose :: Env -> Word64 -> RefKind -> Word64 -> Word64 -> IO ()
addLoose env at kind target anchor = modifyIORef' (eLoose env) ((at, fromIntegral (fromEnum kind), target, anchor) :)

pushLabel :: Env -> Word64 -> Bool -> IO ()
pushLabel env a tbl = when (inText env a) $ modifyIORef' (eLabels env) ((a, tbl) :)

pushData :: Env -> Word64 -> IO ()
pushData env a = modifyIORef' (eDataLabels env) (a :)

pinRange :: Env -> String -> Word64 -> Word64 -> IO ()
pinRange env why lo hi = do
  modifyIORef' (ePins env) ((why, lo, hi) :)
  modifyIORef' (ePinCounts env) (M.insertWith (+) why 1)

-- | Pins one address; a static closure there is parsed too, since its
-- fields point at things that may move even though it stays.
pinTargetAs :: Env -> String -> Word64 -> IO ()
pinTargetAs env why a = do
  pinRange env why a (a + 1)
  case defAt (eImage env) (a .&. complement 7) of
    Just d | dKind d == Haskell && secRole (imSections (eImage env) V.! dSect d) == Data -> pushData env (a .&. complement 7)
    _ -> pure ()

-- | Pins the 4 KB page an adrp names.
pinPage :: Env -> Word64 -> IO ()
pinPage env page = pinRange env "page" page (page + 4096)

-- | What the architecture's decoder may do while decoding one definition.
mkHooks :: Env -> Def -> Hooks
mkHooks env d =
  Hooks
    { hkBytes = eBytes env,
      hkOffsetOf = fmap fromIntegral . fileOffsetOf (eImage env),
      hkDefStart = dStart d,
      hkDefEnd = dEnd d,
      hkDefName = BC.unpack (dName d),
      hkVisit = \a n -> do
        let ix = textIndex env a
        seen <- VUM.read (eVisited env) ix
        table <- VUM.read (eIsTable env) ix
        if seen || table
          then pure False
          else do
            let count = max 1 ((n + eGranule env - 1) `div` eGranule env)
                limit = VUM.length (eVisited env)
            forM_ [ix .. min (limit - 1) (ix + count - 1)] $ \k -> VUM.write (eVisited env) k True
            VUM.write (eStarts env) ix True
            pure True,
      hkInterior = \a ->
        if not (inText env a)
          then pure False
          else do
            let ix = textIndex env a
            v <- VUM.read (eVisited env) ix
            st <- VUM.read (eStarts env) ix
            pure (v && not st),
      hkIsTable = \a -> if inText env a then VUM.read (eIsTable env) (textIndex env a) else pure False,
      hkMarkData = \a n -> when (inText env a) $ do
        let ix = textIndex env a
            count = max 1 ((n + eGranule env - 1) `div` eGranule env)
            limit = VUM.length (eIsTable env)
        forM_ [ix .. min (limit - 1) (ix + count - 1)] $ \k -> VUM.write (eIsTable env) k True,
      hkRef = \at kind target anchor -> do
        addRef env (dIndex d) at kind target anchor
        when (kind == RAbs32) (bump env "absolute 32-bit references"),
      hkLabel = pushLabel env,
      -- a closure pointer from code carries its tag bits
      hkData = \a -> pushData env (a .&. complement 7),
      hkJumpTable = \t size relative -> do
        seen <- S.member t <$> readIORef (eTablesSeen env)
        unless seen $ do
          modifyIORef' (eTablesSeen env) (S.insert t)
          modifyIORef' (eJumpTables env) (t :)
          modifyIORef' (eTableStarts env) (S.insert t)
          modifyIORef' (eTableQueue env) ((dIndex d, t, size, relative) :)
          bump env "jump tables",
      hkNextTable = \t -> S.lookupGT t <$> readIORef (eTableStarts env),
      hkPin = pinTargetAs env,
      hkPinPage = pinPage env,
      hkAnomaly = anomaly env,
      hkFirstPass = readIORef (eFirstPass env),
      hkInText = inText env,
      hkHasTable = \a -> inText env a && looksLikeTable (eImage env) a,
      hkIsHaskellAt = \a -> maybe False ((== Haskell) . dKind) (defAt (eImage env) a),
      hkInImage = isJust . fileOffsetOf (eImage env),
      hkPie = imPie (eImage env),
      hkAbsField = case ldAbsFields (imLoaded (eImage env)) of
        Nothing -> const True
        Just _ -> \a -> S.member a (eAbsFields env)
    }

drain :: Env -> IO ()
drain env = do
  ls <- readIORef (eLabels env)
  case ls of
    (a, tbl) : rest -> do
      writeIORef (eLabels env) rest
      case defAt (eImage env) a of
        Just d | dKind d == Haskell -> do
          when tbl (parseTable env d a)
          let hk = mkHooks env d
          case imArch (eImage env) of
            Arm64 -> Arm64.decodeFrom hk a
            X86_64 -> X86.decodeFrom hk a
        -- GHC code can sit in a fixed region (the unlabeled lead of the code
        -- section, a local closure after a C symbol): its instructions are
        -- swept, but its info tables' fields are only found through the
        -- labels that reach them, and what they point at must stay
        Just d | tbl -> parseTable env d a
        _ -> pure ()
      drain env
    [] -> do
      ds <- readIORef (eDataLabels env)
      case ds of
        a : rest -> do
          writeIORef (eDataLabels env) rest
          parseClosure env a
          drain env
        [] -> do
          -- jump tables last, when every table reachable so far is known
          ts <- readIORef (eTableQueue env)
          case ts of
            [] -> pure ()
            _ -> do
              writeIORef (eTableQueue env) []
              forM_ (reverse ts) $ \(di, t, size, relative) -> do
                let d = imDefs (eImage env) V.! di
                X86.tableEntries (mkHooks env d) t size relative
              drain env

-- | Parses a static closure in data when the loader gives no list of
-- pointer slots: its info pointer and the pointer fields its info table's
-- layout names are the slots. A pointer field names another closure,
-- which is parsed in turn. A non-pointer field that holds the address of a
-- Haskell string is taken for an Addr# literal and rewritten too.
parseClosure :: Env -> Word64 -> IO ()
parseClosure env c = do
  done <- S.member c <$> readIORef (eClosuresDone env)
  let exact = isJust (ldSlots (imLoaded (eImage env)))
  unless (done || exact) $ do
    modifyIORef' (eClosuresDone env) (S.insert c)
    case defAt (eImage env) c of
      Just d
        | dKind d == Haskell,
          secRole (imSections (eImage env) V.! dSect d) == Data -> do
            let im = eImage env
                bytes = eBytes env
                info = word64At' env c
                label = info
                tbl = case fileOffsetOf im label of
                  Just o | inText env label -> tableSize bytes (fromIntegral o)
                  _ -> 0
                slot s v = do
                  addRef env (dIndex d) s RPointer v 0
                  modifyIORef' (eFoundSlots env) ((s, v) :)
            if tbl == 0
              then when (c == dStart d && not (dLocal d)) $ do
                bump env "data definitions that are not closures"
                modifyIORef' (eNotes env) (("not a closure: " ++ hsDisplay d ++ " (first word " ++ hex info ++ ")") :)
              else do
                slot c info
                pushLabel env info True
                let off = fromIntegral (fromMaybe 0 (fileOffsetOf im label)) :: Int
                    ty = u32 bytes (off - 8)
                    ptrs = fromIntegral (u32 bytes (off - 16)) :: Int
                    nptrs = fromIntegral (u32 bytes (off - 12)) :: Int
                    -- where the pointer fields begin and how many there are
                    -- a static thunk keeps its payload behind the thunk header, the
                    -- static link and the saved info (stg_unpack_cstring's
                    -- precomputed closure has an Addr# there that its table does
                    -- not declare), so its first words are all looked at
                    (payload, count, nonPtrs)
                      | ty >= 1 && ty <= 14 = (c + 8, ptrs, nptrs)
                      | ty >= 15 && ty <= 22 = (c + 8, 0, ptrs + nptrs + 4)
                      | ty == 28 = (c + 8, 1, 0) -- IND_STATIC: the indirectee
                      | ty >= 43 && ty <= 46 = (c + 24, fromIntegral (word64At' env (c + 8)), 0) -- MUT_ARR_PTRS: ptrs, size, payload
                      | ty >= 55 && ty <= 58 = (c + 16, fromIntegral (word64At' env (c + 8)), 0) -- SMALL_MUT_ARR_PTRS: ptrs, payload
                      | otherwise = (c + 8, 0, 0)
                forM_ [0 .. count - 1] $ \k -> do
                  let s = payload + 8 * fromIntegral k
                      v = word64At' env s
                  when (v /= 0 && isJust (fileOffsetOf im s) && isJust (fileOffsetOf im (v .&. complement 7))) $ do
                    slot s v
                    case defAt im (v .&. complement 7) of
                      Just t | dKind t == Haskell && secRole (imSections im V.! dSect t) == Data -> pushData env (v .&. complement 7)
                      _ -> pure ()
                    when (inText env v) (pushLabel env v True)
                forM_ [count .. count + nonPtrs - 1] $ \k -> do
                  let s = payload + 8 * fromIntegral k
                      v = word64At' env s
                  case defAt im (v .&. complement 7) of
                    Just t | dKind t == Haskell && secRole (imSections im V.! dSect t) `elem` [Strings, Const] -> do
                      slot s v
                      bump env "addresses in non-pointer fields"
                    Just t | dKind t == Haskell && secRole (imSections im V.! dSect t) == Data && ty >= 15 && ty <= 22 -> do
                      slot s v
                      pushData env (v .&. complement 7)
                      bump env "pointers in static thunks"
                    _ -> pure ()
      _ -> pure ()

-- | Parses the info table before a label, once.
parseTable :: Env -> Def -> Word64 -> IO ()
parseTable env d label = do
  let ix = textIndex env label
  done <- VUM.read (eTabled env) ix
  unless done $ do
    VUM.write (eTabled env) ix True
    first <- readIORef (eFirstPass env)
    when first (modifyIORef' (eTableLabels env) (label :))
    case fileOffsetOf (eImage env) label of
      Nothing -> pure ()
      Just off0 -> do
        let off = fromIntegral off0 :: Int
            bytes = eBytes env
            hd = tableSize bytes off
            di = dIndex d
            ty = u32 bytes (off - 8)
            rel :: Int -> Word64
            rel fieldOff = fromIntegral (fromIntegral label + i64 bytes (off + fieldOff))
            -- In a fixed region the table may be hand-written runtime code
            -- whose layout differs: only the SRT field, present in every
            -- table, is rewritten; the other fields pin what they point at.
            check :: RefKind -> Int -> Word64 -> IO ()
            check kind fieldOff target
              | isJust (fileOffsetOf (eImage env) target) || target == label = do
                  if dKind d == Haskell || (kind == RSrt && ePatchFixedSrt env)
                    then addRef env di (fromIntegral (fromIntegral label + fieldOff :: Int)) kind target 0
                    else pinTargetAs env "table field in fixed code" target
                  -- an SRT is a static closure of pointers to other closures
                  when (kind == RSrt) (pushData env target)
              -- hand-written runtime tables need not have every field; in a fixed region the field is left alone
              | dKind d == Haskell = anomaly env (show kind ++ " at " ++ hex label ++ " in " ++ BC.unpack (dName d) ++ " points outside the file: " ++ hex target)
              | otherwise = pure ()
        if hd == 0 || label - fromIntegral hd < dStart d
          then when (dKind d == Haskell) (anomaly env ("no info table before label " ++ hex label ++ " in " ++ BC.unpack (dName d)))
          else do
            -- in a constructor's table the field holds the constructor tag, not an SRT offset
            let srt = i32 bytes (off - 4)
                isConstructor = ty >= 1 && ty <= 7
            when (srt /= 0 && not isConstructor) (check RSrt (-4) (fromIntegral (fromIntegral label + fromIntegral srt :: Int64)))
            when (ty >= 1 && ty <= 7) (check RConDesc (-24) (rel (-24)))
            when (ty >= 8 && ty <= 14 && hd == 40) $ do
              -- the slow entry is code without a table of its own (a local
              -- function's lives unlabeled before its fast entry) and must be
              -- decoded: its jump back to the fast entry is a reference too
              check RSlowApply (-40) (rel (-40))
              pushLabel env (rel (-40)) False
              when (u32 bytes (off - 24) == 1) (check RFunBitmap (-32) (rel (-32)))
            when (ty == 31) (check RLargeBitmap (-16) (rel (-16)))

hex :: Word64 -> String
hex a = "0x" ++ go a
  where
    go 0 = ""
    go n = go (n `shiftR` 4) ++ [("0123456789abcdef" !!) (fromIntegral (n .&. 15))]

-- | Identical code folding. Two kept, movable definitions of code or
-- strings whose bytes agree once their reference fields are masked, and
-- whose references agree once their targets are named canonically (the
-- representative of the target's definition and the offset into it, or
-- the fixed address), can share one copy: nothing compares code or string
-- addresses for identity. Static closures are never folded, since a CAF
-- or a constructor is observable by identity. Folding a target makes more
-- pairs equal, so the grouping is iterated to a fixed point.
fold :: Image -> B.ByteString -> V.Vector (VU.Vector (Word64, Word8, Word64, Word64)) -> VU.Vector Bool -> VU.Vector Bool -> VU.Vector Bool -> [(Word64, Word64)] -> VU.Vector Int
fold im bytes perDef kept pinned cut ranges = go (VU.generate n id) (5 :: Int)
  where
    n = V.length (imDefs im)
    defs = imDefs im
    candidate i =
      let d = defs V.! i
          sect = imSections im V.! dSect d
          inRange = null ranges || any (\(lo, hi) -> dStart d >= lo && dStart d < hi) ranges
       in kept VU.! i && not (pinned VU.! i) && not (cut VU.! i) && dKind d == Haskell && (isCode sect || secRole sect == Strings) && dEnd d > dStart d && inRange
    -- the bytes with each reference field's address bits cleared: the
    -- immediate of an instruction (its registers and opcode stay), the whole
    -- of a data field
    masked i = case fileOffsetOf im (dStart (defs V.! i)) of
      Nothing -> B.empty
      Just off ->
        let d = defs V.! i
            raw = B.take (fromIntegral (dEnd d - dStart d)) (B.drop (fromIntegral off) bytes)
            holes = sortOn fst [(fromIntegral (at - dStart d), toEnum (fromIntegral k)) | (at, k, _, _) <- VU.toList (perDef V.! i)]
         in goMask 0 holes raw
    goMask _ [] rest = rest
    goMask pos ((o, k) : more) rest
      | o < pos = goMask pos more rest
      | otherwise =
          let l = fieldLen k
              (keep, after) = B.splitAt (o - pos) rest
              field = B.take l after
           in keep <> maskField k field <> goMask (o + l) more (B.drop l after)
    maskField :: RefKind -> B.ByteString -> B.ByteString
    maskField k field
      | k `elem` [RAdrpAdd, RAdrpLdr, RB, RBl, RBcond, RCbz, RTbz] =
          B.concat (map word (Arm64.maskInsn k [u32 field (4 * j) | j <- [0 .. B.length field `div` 4 - 1]]))
      | otherwise = B.replicate (B.length field) 0
    word :: Word32 -> B.ByteString
    word w = B.pack [fromIntegral (w `shiftR` (8 * j)) | j <- [0 .. 3]]
    canon rep i =
      let d = defs V.! i
       in [ (at - dStart d, k, target)
          | (at, k, t, _) <- VU.toList (perDef V.! i),
            let target = case defAt im t of
                  Just e | dIndex e == i -> Left (-1, t - dStart e) -- self
                  Just e -> Left (rep VU.! dIndex e, t - dStart e)
                  Nothing -> Right t
          ]
    go rep 0 = rep
    go rep k =
      let groups =
            M.fromListWith
              (\_ old -> old) -- keep the first (lowest index) as the representative
              [ ((dEnd d - dStart d, masked i, canon rep i), i)
              | i <- [0 .. n - 1],
                candidate i,
                let d = defs V.! i
              ]
          rep' = VU.generate n $ \i ->
            if candidate i
              then M.findWithDefault i (dEnd (defs V.! i) - dStart (defs V.! i), masked i, canon rep i) groups
              else i
       in if rep' == rep then rep else go rep' (k - 1)
