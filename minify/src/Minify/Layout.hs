-- | Where every kept definition goes, and how the segments are split so the
-- file omits the pages compaction frees. See design.md, "Layout".
module Minify.Layout
  ( Plan (..),
    Piece (..),
    plan,
    pageSize,
    newAddrOf,
    newFileOffsetOf,
  )
where

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Ord (Down (..))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word
import Minify.Analysis
import Minify.MachO

pageSize :: Word64
pageSize = 16384

-- | A segment of the output: a run of the original segment's address space
-- that the file keeps, with its clipped sections.
data Piece = Piece
  { pcName :: B.ByteString,
    pcVmStart :: !Word64,
    -- | end of the address range the piece covers
    pcVmEnd :: !Word64,
    -- | end of the file-backed part (the rest is zero fill)
    pcFileEnd :: !Word64,
    pcFileOff :: !Word64,
    pcMaxProt :: !Word32,
    pcInitProt :: !Word32,
    pcFlags :: !Word32,
    pcSections :: [Section]
  }
  deriving (Show)

data Plan = Plan
  { -- | new start address of each definition (meaningful for kept ones)
    plNewStart :: VU.Vector Word64,
    plPieces :: [Piece],
    -- | file offset where __LINKEDIT begins
    plLinkeditOff :: !Word64,
    plLinkeditVm :: !Word64,
    -- | bytes the holes save
    plSaved :: !Word64,
    plHoles :: [(Word64, Word64)],
    -- | branch islands: target address to island address
    plIslands :: M.Map Word64 Word64,
    -- | the island pool's address and size
    plIslandPool :: (Word64, Word64)
  }

-- | A b or bl reaches this far either way.
branchRange :: Word64
branchRange = 128 * 1024 * 1024

islandSize :: Word64
islandSize = 12

-- | The segment-index immediates in dyld's opcodes are 4 bits.
maxSegments :: Int
maxSegments = 16

alignUp :: Word64 -> Word64 -> Word64
alignUp a n = (a + n - 1) .&. complement (n - 1)

alignDown :: Word64 -> Word64 -> Word64
alignDown a n = a .&. complement (n - 1)

-- | The alignment a definition had, up to 8.
alignOf :: Word64 -> Word64
alignOf a = min 8 (a .&. negate a)

-- | The second argument caps the number of holes (segments added).
plan :: Analysis -> Int -> Either String Plan
plan an maxHoles = do
  let im = anImage an
      defs = imDefs im
      sects = imSections im
      kept i = anKept an VU.! i
      pinned i = anPinned an VU.! i
      ndefs = V.length defs
      -- placement, section by section (definitions are in address order)
      place :: Int -> Word64 -> Int -> [(Int, Word64)] -> Either String [(Int, Word64)]
      place i cursor sect acc
        | i >= ndefs = Right (reverse acc)
        | otherwise =
            let d = defs V.! i
                cursor0 = if dSect d /= sect then secAddr (sects V.! dSect d) else cursor
                size = dEnd d - dStart d
             in if not (kept i)
                  then place (i + 1) cursor0 (dSect d) acc
                  else
                    if dKind d == Fixed || pinned i
                      then
                        if cursor0 > dStart d
                          then Left ("compaction overran the fixed definition " ++ show (dName d))
                          else place (i + 1) (dEnd d) (dSect d) ((i, dStart d) : acc)
                      else
                        let a = alignUp cursor0 (alignOf (dStart d))
                         in if a > dStart d
                              then Left ("compaction moved " ++ show (dName d) ++ " upward")
                              else place (i + 1) (a + size) (dSect d) ((i, a) : acc)
  placed <- place 0 0 (-1) []
  let newStart = VU.replicate ndefs 0 VU.// placed
      segs = imSegments im
      newAddr a = case defAt im a of
        Just d | kept (dIndex d) -> Just (a - dStart d + newStart VU.! dIndex d)
        Just _ -> Nothing
        Nothing -> Just a
      -- branches that no longer reach their target
      far =
        [ (at', t')
          | (i, a) <- placed,
            let d = defs V.! i,
            dKind d == Haskell,
            (at, k, t) <- VU.toList (anRefs an V.! i),
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
            fromSects = [(secAddr s, secAddr s + secSize s) | s <- segSections seg, not (isCompactedSection s), secFlags s .&. 0xff /= 1]
            -- compacted sections' fixed leads are definitions already; the header area is live
            header = [(segVmAddr seg, secAddr (head (segSections seg))) | segName seg == "__TEXT", not (null (segSections seg))]
            extras = [(a, a + n) | (a, n) <- extra, inSeg a]
         in mergeRanges (sortOn fst (header ++ fromSects ++ fromDefs ++ extras))
      farTargets = S.toList (S.fromList (map snd far))
      poolSize = alignUp (fromIntegral (length farTargets) * islandSize) 16
      textSect = case [s | s <- V.toList sects, isCode s] of
        s : _ -> s
        [] -> error "no __text"
  -- The pool goes in a gap of the placed layout that every far source reaches.
  pool <-
    if null farTargets
      then Right (0, 0)
      else
        let textSeg = [sg | sg <- segs, segName sg == "__TEXT"]
            gaps = case textSeg of
              sg : _ -> deadGaps (liveRangesWith [] sg) (segVmAddr sg + segFileSize sg)
              [] -> []
            srcLo = minimum (map fst far)
            srcHi = maximum (map fst far)
            fits (g0, g1) =
              let a = alignUp g0 16
               in a + poolSize <= g1
                    && a >= secAddr textSect
                    && a + poolSize <= secAddr textSect + secSize textSect
                    && distance a srcLo < branchRange - poolSize
                    && distance a srcHi < branchRange - poolSize
         in case [alignUp g0 16 | g@(g0, _) <- gaps, fits g] of
              a : _ -> Right (a, poolSize)
              [] -> Left ("no gap for " ++ show (length farTargets) ++ " branch islands within reach of every far branch")
  let islands = M.fromList (zip farTargets [fst pool, fst pool + islandSize ..])
      poolLive = [pool | snd pool > 0]
      liveRanges = liveRangesWith poolLive
      -- dead gaps rounded inward to whole pages
      holesOf seg =
        [ (s, e)
          | (g0, g1) <- deadGaps (liveRanges seg) (segVmAddr seg + segFileSize seg),
            let s = alignUp g0 pageSize,
            let e = alignDown g1 pageSize,
            e > s
        ]
      candidates = [(seg, h) | seg <- segs, segFileSize seg > 0, segName seg /= "__LINKEDIT", h <- holesOf seg]
      budget = min maxHoles (maxSegments - length segs)
      chosen = take budget (sortOn (\(_, (s, e)) -> Down (e - s)) candidates)
      holesFor seg = sortOn fst [h | (sg, h) <- chosen, segName sg == segName seg]
      -- pieces, with file offsets assigned in order
      mkPieces [] _ = []
      mkPieces (seg : rest) cursor
        | segName seg == "__LINKEDIT" || segFileSize seg == 0 && segName seg == "__PAGEZERO" = mkPieces rest cursor
        | otherwise =
            let bounds = splitAt' (segVmAddr seg) (segVmAddr seg + segVmSize seg) (holesFor seg)
                fileBackedEnd = segVmAddr seg + segFileSize seg
                go _ [] = []
                go c ((s, e) : more) =
                  let fe = min e fileBackedEnd
                      -- dyld allows one segment named __TEXT (the one at file offset 0)
                      k = length (takeWhile ((/= s) . fst) bounds)
                      p =
                        Piece
                          { pcName = if k == 0 then segName seg else segName seg <> "_" <> BC.pack (show k),
                            pcVmStart = s,
                            pcVmEnd = e,
                            pcFileEnd = fe,
                            pcFileOff = c,
                            pcMaxProt = segMaxProt seg,
                            pcInitProt = segInitProt seg,
                            pcFlags = segFlags seg,
                            pcSections = [clip s e c sec | sec <- segSections seg, secAddr sec < e, secAddr sec + secSize sec > s || (secSize sec == 0 && secAddr sec >= s)]
                          }
                   in p : go (alignUp (c + (fe - s)) pageSize) more
                ps = go cursor bounds
                next = case ps of
                  [] -> cursor
                  _ -> let l = last ps in alignUp (pcFileOff l + (pcFileEnd l - pcVmStart l)) pageSize
             in ps ++ mkPieces rest next
      clip s e c sec =
        let a = max s (secAddr sec)
            b = min e (secAddr sec + secSize sec)
            zerofill = secFlags sec .&. 0xff == 1
         in sec {secAddr = a, secSize = b - a, secOffset = if zerofill then 0 else fromIntegral (c + (a - s))}
      splitAt' s e [] = [(s, e)]
      splitAt' s e ((hs, he) : more) = (s, hs) : splitAt' he e more
      pieces = mkPieces segs 0
      linkeditOff = case pieces of
        [] -> 0
        _ -> let l = last pieces in alignUp (pcFileOff l + (pcFileEnd l - pcVmStart l)) pageSize
      linkeditVm = head ([segVmAddr s | s <- segs, segName s == "__LINKEDIT"] ++ [0])
  pure
    Plan
      { plNewStart = newStart,
        plPieces = pieces,
        plLinkeditOff = linkeditOff,
        plLinkeditVm = linkeditVm,
        plSaved = sum [e - s | (_, (s, e)) <- chosen],
        plHoles = [h | (_, h) <- chosen],
        plIslands = islands,
        plIslandPool = pool
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
