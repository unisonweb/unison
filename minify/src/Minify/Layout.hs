-- Optimized in every build, --fast too: minifying is slow without it
{-# OPTIONS_GHC -O2 #-}
-- | Where every kept definition goes, and how the segments are split so the
-- file omits the pages compaction frees. See design.md, "Layout".
module Minify.Layout
  ( Plan (..),
    Piece (..),
    Stub (..),
    plan,
    newAddrOf,
    newFileOffsetOf,
    stubCodeSize,
  )
where

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Ord (Down (..))
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word
import Minify.Analysis
import qualified Minify.Arm64 as Arm64
import Minify.Elf (Elf (..), Phdr (..), ptLoad, ptPhdr)
import Minify.Format
import qualified Minify.X86 as X86

-- | A segment of the output: a run of the original segment's address space
-- that the file keeps, with its clipped sections.
data Piece = Piece
  { pcName :: B.ByteString,
    -- | the segment it was cut from
    pcSegment :: Segment,
    -- | not the first piece of its segment
    pcExtra :: !Bool,
    pcVmStart :: !Word64,
    -- | end of the address range the piece covers
    pcVmEnd :: !Word64,
    -- | end of the file-backed part (the rest is zero fill)
    pcFileEnd :: !Word64,
    pcFileOff :: !Word64,
    pcSections :: [Section]
  }
  deriving (Show)

data Plan = Plan
  { -- | new start address of each definition (meaningful for kept ones)
    plNewStart :: VU.Vector Word64,
    -- | where each placed definition's own bytes go (0 when not placed)
    plPlacedStart :: VU.Vector Word64,
    plPieces :: [Piece],
    -- | file offset where the pieces end: the link-edit data (Mach-O) or the
    -- section headers and symbol table (ELF) follow
    plLinkeditOff :: !Word64,
    plLinkeditVm :: !Word64,
    -- | bytes the holes save
    plSaved :: !Word64,
    plHoles :: [(Word64, Word64)],
    -- | the pieces in load-command order (the segment numbering the loader sees)
    plOrder :: [Piece],
    -- | branch islands: target address to island address
    plIslands :: M.Map Word64 Word64,
    -- | the patch area in the code section (islands, abort stubs, their messages, ELF program headers): address and size
    plIslandPool :: (Word64, Word64),
    -- | the patch area in data (closure stubs): address and size
    plDataArea :: (Word64, Word64),
    -- | abort stubs for cut definitions still referred to, by definition index
    plStubs :: M.Map Int Stub,
    -- | the empty string constructor names point at when they are dropped (0 when kept)
    plEmptyString :: !Word64,
    -- | where an ELF output's program headers go: address and the bytes reserved
    plPhdrArea :: (Word64, Word64)
  }

-- | A replacement for a cut definition that is still referred to.
data Stub = Stub
  { -- | the code label (its info table precedes it)
    sbCode :: !Word64,
    -- | start of the info table
    sbTable :: !Word64,
    sbTableSize :: !Int,
    -- | the message string
    sbMsg :: !Word64,
    sbMsgText :: B.ByteString,
    -- | the static closure, for a cut data definition
    sbClosure :: Maybe Word64
  }
  deriving (Show)

-- | The abort stub's code, on either architecture.
stubCodeSize :: Word64
stubCodeSize = 16

alignUp :: Word64 -> Word64 -> Word64
alignUp a n = (a + n - 1) .&. complement (n - 1)

alignDown :: Word64 -> Word64 -> Word64
alignDown a n = a .&. complement (n - 1)

-- | The alignment a definition had, up to 8.
alignOf :: Word64 -> Word64
alignOf a = min 8 (a .&. negate a)

-- | The second argument caps the number of holes (segments added); the
-- third puts the extra code-segment pieces after the data pieces in the
-- load commands, so that the data segments keep indices dyld's opcodes can
-- express however many holes the code segment has (Mach-O only).
-- 'packRoles' names the sections whose movable definitions are packed
-- first-fit; the rest keep their order (a debugging knob).
plan :: Analysis -> Int -> Bool -> [Role] -> Maybe (Word64, Word64) -> Bool -> Maybe (Word64, Word64) -> Either String Plan
plan an maxHoles textLast packRoles crossRange keepCopies padAfter = do
  let im = anImage an
      defs = imDefs im
      sects = imSections im
      pageSize = imPageSize im
      isMachO = imFormat im == MachOFormat
      (needsIslands, branchRange, islandSize) = case imArch im of
        Arm64 -> (Arm64.needsIslands, Arm64.branchRange, Arm64.islandSize)
        X86_64 -> (X86.needsIslands, X86.branchRange, 0)
      kept i = anKept an VU.! i
      pinned i = anPinned an VU.! i
      ndefs = V.length defs
      -- Placement, section by section. Fixed and pinned definitions are
      -- anchors that stay put; the movable ones fill the gaps before each
      -- anchor first-fit, in address order with a bounded look-ahead for one
      -- that fits when the next in line does not, so that dead space collects
      -- at the end of the section where whole pages leave the file.
      sectionOf i = dSect (defs V.! i)
      sizeOf i = let d = defs V.! i in dEnd d - dStart d
      isAnchor i = dKind (defs V.! i) == Fixed || pinned i
      placedDef i = kept i && (keepCopies || anRep an VU.! i == i)
      perSection = groupBy' sectionOf [i | i <- [0 .. ndefs - 1], placedDef i]
      lookAhead = 4000 :: Int
      -- debugging: a definition may be placed before an anchor it originally
      -- followed only when its address is in the range (none: never)
      mayCross i = case crossRange of
        Nothing -> False
        Just (lo, hi) -> let a = dStart (defs V.! i) in a >= lo && a < hi
      -- fills [cursor, limit) from the queue; returns placements and the rest of the queue
      fill :: Bool -> Word64 -> Word64 -> Word64 -> Seq.Seq Int -> ([(Int, Word64)], Seq.Seq Int)
      fill pack cursor limit anchorAt queue = case Seq.viewl queue of
        Seq.EmptyL -> ([], queue)
        i Seq.:< rest ->
          let a = alignUp cursor (alignOf (dStart (defs V.! i)))
              allowed j = dStart (defs V.! j) < anchorAt || mayCross j
              -- debugging: extra space after one definition
              padded j = case padAfter of
                Just (addr, n) | dStart (defs V.! j) == addr -> n
                _ -> 0
           in if a + sizeOf i + padded i <= limit && allowed i
                then let (more, q') = fill pack (a + sizeOf i + padded i) limit anchorAt rest in ((i, a) : more, q')
                else if not pack then ([], queue) else case Seq.findIndexL (\j -> let b = alignUp cursor (alignOf (dStart (defs V.! j))) in b + sizeOf j <= limit && allowed j) (Seq.take lookAhead rest) of
                  Nothing -> ([], queue)
                  Just k ->
                    let j = Seq.index rest k
                        b = alignUp cursor (alignOf (dStart (defs V.! j)))
                        (more, q') = fill pack (b + sizeOf j) limit anchorAt (i Seq.<| Seq.deleteAt k rest)
                     in ((j, b) : more, q')
      placeSection :: [Int] -> Either String [(Int, Word64)]
      placeSection is =
        let sect = sects V.! sectionOf (headOr is)
            pack = secRole sect `elem` packRoles
            anchors = [i | i <- is, isAnchor i]
            queue0 = Seq.fromList [i | i <- is, not (isAnchor i)]
            go cursor [] q =
              let (ps, q') = fill pack cursor (secAddr sect + secSize sect) maxBound q
               in if Seq.null q' then Right ps else Left ("no room left in " ++ show (secName sect) ++ " for " ++ show (Seq.length q') ++ " definitions")
            go cursor (anc : more) q =
              let d = defs V.! anc
                  (ps, q') = fill pack cursor (dStart d) (dStart d) q
               in if cursor > dStart d
                    then Left ("compaction overran the fixed definition " ++ show (dName d))
                    else (\later -> (anc, dStart d) : ps ++ later) <$> go (dEnd d) more q'
         in go (secAddr sect) anchors queue0
      headOr (x : _) = x
      headOr [] = 0
      groupBy' f = foldr step []
        where
          step x [] = [[x]]
          step x (g : gs) | f x == f (headOr g) = (x : g) : gs
          step x gs = [x] : gs
  placed <- concat <$> mapM placeSection perSection
  let placedStart = VU.replicate ndefs 0 VU.// placed
      -- a folded definition lives where its representative does
      newStart = VU.generate ndefs (\i -> placedStart VU.! (anRep an VU.! i))
      segs = imSegments im
      newAddr a = case defAt im a of
        Just d | kept (dIndex d) -> Just (a - dStart d + newStart VU.! dIndex d)
        Just _ -> Nothing
        Nothing -> Just a
      -- branches that no longer reach their target
      far =
        [ (at', t')
          | needsIslands,
            (i, a) <- placed,
            let d = defs V.! i,
            dKind d == Haskell,
            anRep an VU.! i == i,
            (at, k, t, _) <- VU.toList (anRefs an V.! i),
            k == fromIntegral (fromEnum RB) || k == fromIntegral (fromEnum RBl),
            let at' = at - dStart d + a,
            Just t' <- [newAddr t],
            distance at' t' >= branchRange - islandSize
        ]
      distance x y = if x > y then x - y else y - x
      -- live address ranges in the new layout, per segment
      liveRangesWith extra seg =
        let segEnd = segVmAddr seg + segFileSize seg
            inSeg a = a >= segVmAddr seg && a < segEnd
            fromDefs = [(a, a + (dEnd d - dStart d)) | (i, a) <- placed, let d = defs V.! i, inSeg a]
            fromSects = [(secAddr s, secAddr s + secSize s) | s <- segSections seg, not (isCompactedSection s), not (secNoBits s)]
            -- compacted sections' fixed leads are definitions already; the file header area is live
            header = [(segVmAddr seg, minimum (map secAddr (segSections seg))) | segFileOff seg == 0, not (null (segSections seg))]
            extras = [(a, a + n) | (a, n) <- extra, inSeg a]
         in mergeRanges (sortOn fst (header ++ fromSects ++ fromDefs ++ extras))
      farTargets = S.toList (S.fromList (map snd far))
      islandsSize = alignUp (fromIntegral (length farTargets) * islandSize) 16
      -- cut definitions a kept one refers to, each with its stub's sizes
      stubbed =
        [ (i, tableSz, msg)
          | i <- [0 .. ndefs - 1],
            anCutReferenced an VU.! i,
            let d = defs V.! i,
            let tableSz = if isCode (sects V.! dSect d) then max 16 (dHead d) else 24,
            let msg = BC.pack ("minify: " ++ hsDisplay d ++ " was removed") <> B.singleton 0
        ]
      stubsSize = sum [fromIntegral (alignUp (fromIntegral tsz) 8) + stubCodeSize | (_, tsz, _) <- stubbed]
      msgsSize = alignUp (sum [fromIntegral (B.length m) | (_, _, m) <- stubbed]) 16
      emptySize = if anKeepConDesc an then 0 else 16
      -- an ELF output's program headers move into the patch area too, since
      -- the holes add program headers and the original table has no room
      phdrSize = case imRaw im of
        RawElfImage e -> alignUp (56 * fromIntegral (length (elfPhdrs e) + maxHoles + 4)) 16
        _ -> 0
      poolSize = islandsSize + stubsSize + msgsSize + emptySize + phdrSize
      dataStubs = [i | (i, _, _) <- stubbed, not (isCode (sects V.! dSect (defs V.! i)))]
      -- a position-independent ELF keeps a scratch word for the relative
      -- relocations whose slots were removed (the loader applies the first
      -- DT_RELACOUNT entries blindly, so they cannot become no-ops)
      scratch = if imFormat im == ElfFormat && imPie im then 16 else 0
      dataAreaSize = 16 * fromIntegral (length dataStubs) + scratch
      midText = secAddr textSect + secSize textSect `div` 2
      textSect = case [s | s <- V.toList sects, isCode s] of
        s : _ -> s
        [] -> error "no code section"
  -- The patch area goes in the gap of the placed layout nearest the middle
  -- of the code section, which every branch in the section reaches.
  pool <-
    if poolSize == 0
      then Right (0, 0)
      else
        let gaps = concat [deadGaps (liveRangesWith [] sg) (segVmAddr sg + segFileSize sg) | sg <- segs, segRole sg == SegText]
            fits (g0, g1) =
              let a = alignUp g0 16
               in a + poolSize <= g1 && a >= secAddr textSect && a + poolSize <= secAddr textSect + secSize textSect
            candidates = sortOn (distance midText) [alignUp g0 16 | g@(g0, _) <- gaps, fits g]
            reaches a = all (\(src, _) -> distance a src < branchRange - poolSize) far
         in case filter reaches candidates of
              a : _ -> Right (a, poolSize)
              [] -> Left ("no gap of " ++ show poolSize ++ " bytes in the code section for the islands and stubs")
  dataArea <-
    if dataAreaSize == 0
      then Right (0, 0)
      else
        let dataSect = [sc | sc <- V.toList sects, secRole sc == Data]
            gaps = concat [deadGaps (liveRangesWith [] sg) (segVmAddr sg + segFileSize sg) | sg <- segs, segRole sg == SegData]
            fits sc (g0, g1) = let a = alignUp g0 16 in a + dataAreaSize <= g1 && a >= secAddr sc && a + dataAreaSize <= secAddr sc + secSize sc
         in case [alignUp g0 16 | sc <- dataSect, g@(g0, _) <- gaps, fits sc g] of
              a : _ -> Right (a, dataAreaSize)
              [] -> Left ("no gap of " ++ show dataAreaSize ++ " bytes in the data section for the closure stubs")
  let islands = M.fromList (zip farTargets [fst pool, fst pool + islandSize ..])
      stubsStart = fst pool + islandsSize
      msgsStart = stubsStart + stubsSize
      stubList = go stubsStart msgsStart (fst dataArea + scratch) stubbed
        where
          go _ _ _ [] = []
          go c m dc ((i, tsz, msg) : rest) =
            let tableStart = c
                code = c + alignUp (fromIntegral tsz) 8
                isData = i `elem` dataStubs
                st = Stub {sbCode = code, sbTable = tableStart, sbTableSize = tsz, sbMsg = m, sbMsgText = msg, sbClosure = if isData then Just dc else Nothing}
             in (i, st) : go (code + stubCodeSize) (m + fromIntegral (B.length msg)) (if isData then dc + 16 else dc) rest
      stubs = M.fromList stubList
      poolLive = [pool | snd pool > 0] ++ [dataArea | snd dataArea > 0]
      emptyString = if emptySize == 0 then 0 else msgsStart + msgsSize
      phdrArea = if phdrSize == 0 then (0, 0) else (msgsStart + msgsSize + emptySize, phdrSize)
      liveRanges = liveRangesWith poolLive
      -- dead gaps rounded inward to whole pages
      holesOf seg =
        [ (s, e)
          | (g0, g1) <- deadGaps (liveRanges seg) (segVmAddr seg + segFileSize seg),
            let s = alignUp g0 pageSize,
            let e = alignDown g1 pageSize,
            e > s
        ]
      candidates = [(seg, h) | seg <- segs, segFileSize seg > 0, segRole seg /= SegLinkedit, h <- holesOf seg]
      -- the segment-index immediates in dyld's opcodes are 4 bits; the Linux
      -- kernel loads the program header table into one page (4 KB on x86-64
      -- and most arm64 kernels), so an ELF may have 73 program headers in all
      maxSegments = case imRaw im of
        RawMachOImage _ _ -> 16
        RawElfImage e -> 4096 `div` 56 - 1 - length [p | p <- elfPhdrs e, phType p /= ptLoad, phType p /= ptPhdr]
      budget = min maxHoles (maxSegments - length segs)
      byLargest = sortOn (\(_, (s, e)) -> Down (e - s)) candidates
      chosen
        | isMachO && textLast = take budget [c | c@(sg, _) <- byLargest, segRole sg /= SegText] ++ take maxHoles [c | c@(sg, _) <- byLargest, segRole sg == SegText]
        | otherwise = take budget byLargest
      holesFor seg = sortOn fst [h | (sg, h) <- chosen, segName sg == segName seg]
      -- pieces, with file offsets assigned in order; a piece's offset is
      -- congruent to its address modulo the page size, as the loaders require
      mkPieces [] _ = []
      mkPieces (seg : rest) cursor
        | segRole seg == SegLinkedit || segRole seg == SegPageZero || segFileSize seg == 0 && null (segSections seg) = mkPieces rest cursor
        | otherwise =
            let bounds = splitAt' (segVmAddr seg) (segVmAddr seg + segVmSize seg) (holesFor seg)
                fileBackedEnd = segVmAddr seg + segFileSize seg
                go _ [] = []
                go c ((s, e) : more) =
                  let fe = min e fileBackedEnd
                      k = length (takeWhile ((/= s) . fst) bounds)
                      off = if c == 0 && s == segVmAddr seg && segFileOff seg == 0 then 0 else alignUp c pageSize + (s .&. (pageSize - 1))
                      p =
                        Piece
                          { -- dyld allows one segment named __TEXT (the one at file offset 0)
                            pcName = if k == 0 then segName seg else segName seg <> "_" <> BC.pack (show k),
                            pcSegment = seg,
                            pcExtra = k > 0,
                            pcVmStart = s,
                            pcVmEnd = e,
                            pcFileEnd = fe,
                            pcFileOff = off,
                            pcSections = [clip s e off sec | sec <- segSections seg, secAddr sec < e, secAddr sec + secSize sec > s || (secSize sec == 0 && secAddr sec >= s)]
                          }
                   in p : go (off + (fe - s)) more
                ps = go cursor bounds
                next = case ps of
                  [] -> cursor
                  _ -> let l = last ps in pcFileOff l + (pcFileEnd l - pcVmStart l)
             in ps ++ mkPieces rest next
      clip s e off sec =
        let a = max s (secAddr sec)
            b = min e (secAddr sec + secSize sec)
         in sec {secAddr = a, secSize = b - a, secOffset = if secNoBits sec then 0 else off + (a - s)}
      splitAt' s e [] = [(s, e)]
      splitAt' s e ((hs, he) : more) = (s, hs) : splitAt' he e more
      pieces = mkPieces segs 0
      order
        | isMachO && textLast = [p | p <- pieces, not (isExtraText p)] ++ [p | p <- pieces, isExtraText p]
        | otherwise = pieces
      isExtraText p = pcExtra p && segRole (pcSegment p) == SegText
      linkeditOff = case pieces of
        [] -> 0
        _ -> let l = last pieces in alignUp (pcFileOff l + (pcFileEnd l - pcVmStart l)) pageSize
      linkeditVm = head ([segVmAddr s | s <- segs, segRole s == SegLinkedit] ++ [0])
  pure
    Plan
      { plNewStart = newStart,
        plPlacedStart = placedStart,
        plPieces = pieces,
        plLinkeditOff = linkeditOff,
        plLinkeditVm = linkeditVm,
        plSaved = sum [e - s | (_, (s, e)) <- chosen],
        plHoles = [h | (_, h) <- chosen],
        plOrder = order,
        plIslands = islands,
        plIslandPool = pool,
        plDataArea = dataArea,
        plStubs = stubs,
        plEmptyString = emptyString,
        plPhdrArea = phdrArea
      }

mergeRanges :: [(Word64, Word64)] -> [(Word64, Word64)]
mergeRanges [] = []
mergeRanges ((a, b) : rest) = go a b rest
  where
    go s e [] = [(s, e)]
    go s e ((c, d) : more)
      | c <= e = go s (max e d) more
      | otherwise = (s, e) : go c d more

-- | The gaps between live ranges, up to an end address.
deadGaps :: [(Word64, Word64)] -> Word64 -> [(Word64, Word64)]
deadGaps live end = [(g0, g1) | (g0, g1) <- zip (map snd live) (map fst (drop 1 live) ++ [end]), g1 > g0]

-- | The new address of an old one: moved with its definition, or unchanged
-- when it lies in nothing that moves. Nothing for an address inside a
-- definition that is not kept.
newAddrOf :: Analysis -> Plan -> Word64 -> Maybe Word64
newAddrOf an pl a = case defAt (anImage an) a of
  Just d
    | anKept an VU.! dIndex d -> Just (a - dStart d + plNewStart pl VU.! dIndex d)
    | otherwise -> Nothing
  Nothing -> Just a

-- | The file offset of a new address in the output.
newFileOffsetOf :: Plan -> Word64 -> Maybe Word64
newFileOffsetOf pl a =
  case [p | p <- plPieces pl, a >= pcVmStart p, a < pcFileEnd p] of
    p : _ -> Just (pcFileOff p + (a - pcVmStart p))
    [] -> Nothing
