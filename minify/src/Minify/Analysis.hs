-- | Carving the file into definitions, finding every reference between
-- them, and deciding which ones are kept. See design.md.
module Minify.Analysis
  ( Kind (..),
    Def (..),
    Image (..),
    RefKind (..),
    Ref (..),
    Analysis (..),
    Options (..),
    loadImage,
    analyze,
    defAt,
    fileOffsetOf,
    isCompactedSection,
    isCode,
    hsDisplay,
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
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word
import Minify.Arm64
import Minify.MachO
import Minify.ZEncoding

data Kind = Haskell | Fixed deriving (Eq, Show)

-- | A definition: the bytes from one external symbol to the next, less the
-- info table that belongs to the next.
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
    -- | index into 'sectionsInOrder'
    dSect :: !Int,
    dKind :: !Kind,
    dHead :: !Int,
    -- | a local closure, static closure or string split off the definition named by its symbol
    dLocal :: !Bool
  }
  deriving (Show)

data Image = Image
  { imMachO :: MachO,
    imSections :: V.Vector Section,
    -- | (address, size, file offset) of sections with file contents, by address
    imSectMap :: VU.Vector (Word64, Word64, Word64),
    imDefs :: V.Vector Def,
    imStarts :: VU.Vector Word64,
    imSymtab :: Symtab,
    imSymbols :: [(Nlist, B.ByteString)],
    -- | rebase slots as (slot address, value stored there), by address
    imRebases :: VU.Vector (Word64, Word64),
    imRebaseRecords :: [Rebase],
    imSegments :: [Segment]
  }

data RefKind
  = RAdrpAdd
  | RAdrpLdr
  | RB
  | RBl
  | RBcond
  | RCbz
  | RTbz
  | RSrt
  | RConDesc
  | RLargeBitmap
  | RSlowApply
  | RFunBitmap
  | RPointer
  deriving (Eq, Show, Enum, Bounded)

data Ref = Ref {rAt :: !Word64, rKind :: !RefKind, rTarget :: !Word64}
  deriving (Show, Eq)

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
    optFoldRange :: [(Word64, Word64)]
  }

data Analysis = Analysis
  { anImage :: Image,
    -- | references, per definition index
    anRefs :: V.Vector (VU.Vector (Word64, Word8, Word64)),
    anPinned :: VU.Vector Bool,
    anKept :: VU.Vector Bool,
    anCut :: VU.Vector Bool,
    -- | cut definitions that a kept one still refers to
    anCutReferenced :: VU.Vector Bool,
    anAnomalies :: [String],
    anRoots :: Int,
    anKeepConDesc :: Bool,
    anPinCounts :: M.Map String Int,
    -- | references from slots outside every definition: (slot, kind, target)
    anLoose :: VU.Vector (Word64, Word8, Word64),
    -- | the representative of each definition: itself, or the identical one it is folded onto
    anRep :: VU.Vector Int
  }

isCompactedSection :: Section -> Bool
isCompactedSection s =
  (secSegName s, secName s) `elem` [("__TEXT", "__text"), ("__TEXT", "__cstring"), ("__TEXT", "__const"), ("__DATA", "__data")]

isCode :: Section -> Bool
isCode s = secSegName s == "__TEXT" && secName s == "__text"

hsDisplay :: Def -> String
hsDisplay d = maybe (BC.unpack (dName d)) displayName (dHs d) ++ (if dLocal d then " (local at " ++ hex (dStart d) ++ ")" else "")

-- * Loading

loadImage :: B.ByteString -> Either String Image
loadImage bytes = do
  mo <- parseMachO bytes
  (st, syms) <- maybe (Left "no symbol table: the file is stripped") Right (machoSymbols mo)
  let sects = V.fromList (sectionsInOrder mo)
      named = [(nl, symbolName mo st nl) | nl <- syms]
      sectMap =
        VU.fromList
          ( sortOn (\(a, _, _) -> a)
              [(secAddr s, secSize s, fromIntegral (secOffset s)) | s <- V.toList sects, secFlags s .&. 0xff /= 1] -- not S_ZEROFILL
          )
      segs = segments mo
      dyld = case [d | LCDyldInfo d <- moCommands mo] of
        d : _ -> Just d
        [] -> Nothing
      rebaseRecs = maybe [] (\d -> decodeRebases (B.take (fromIntegral (rebaseSize d)) (B.drop (fromIntegral (rebaseOff d)) bytes))) dyld
      segAddr i = segVmAddr (segs !! i)
      rebases =
        VU.fromList
          ( sortOn fst
              [ (a, v)
                | r <- rebaseRecs,
                  let a = segAddr (rbSeg r) + rbOff r,
                  Just off <- [fileOffsetOf' sectMap a],
                  let v = u64 bytes (fromIntegral off)
              ]
          )
      headOf a = maybe 0 (tableSize bytes . fromIntegral) (fileOffsetOf' sectMap a)
      defs = carve headOf sects named
  when (V.null defs) (Left "no definitions found in the compactable sections")
  pure
    Image
      { imMachO = mo,
        imSections = sects,
        imSectMap = sectMap,
        imDefs = defs,
        imStarts = VU.fromList (map dStart (V.toList defs)),
        imSymtab = st,
        imSymbols = named,
        imRebases = rebases,
        imRebaseRecords = rebaseRecs,
        imSegments = segs
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

-- | Splits each compacted section into definitions. The first argument gives
-- the size of the info table before a code label.
carve :: (Word64 -> Int) -> V.Vector Section -> [(Nlist, B.ByteString)] -> V.Vector Def
-- sections need not be in address order in the load commands (the output's are not)
carve headOf sects named = V.fromList (zipWith (\i d -> d {dIndex = i}) [0 ..] (sortOn dStart (concat perSection)))
  where
    -- symbols are filed by address, not by their n_sect, which a split
    -- section would make ambiguous
    perSection =
      [ carveSection si s [(nValue nl, nm) | (nl, nm) <- named, nType nl .&. 0x0e == 0x0e, nValue nl >= secAddr s, nValue nl < secAddr s + secSize s]
        | (si, s) <- zip [0 ..] (V.toList sects),
          isCompactedSection s
      ]
    carveSection si s syms =
      -- __mh_execute_header is filed under __text but lies before it
      let byAddr = M.toAscList (M.fromListWith (++) [(a, [nm]) | (a, nm) <- syms, a >= secAddr s, a < secAddr s + secSize s])
          -- the name of a group: a Haskell-looking one if any, else the first
          pick nms = case [n | n <- nms, isJust (parseHsName n)] of
            n : _ -> (n, filter (/= n) nms)
            [] -> case nms of
              n : rest -> (n, rest)
              [] -> error "empty symbol group"
          starts =
            [ (a, nm, rest, hs, kind, hd)
              | (a, nms) <- byAddr,
                let (nm, rest) = pick nms,
                let hs = parseHsName nm,
                let kind = if isJust hs then Haskell else Fixed,
                let hd = if isCode s && infoLike nm then headOf a else 0
            ]
          secStart = secAddr s
          secEnd = secAddr s + secSize s
          leadDef en = Def 0 (BC.pack ("<" ++ BC.unpack (secName s) ++ " lead>")) [] Nothing secStart secStart en si Fixed 0 False
          lead = case starts of
            (a, _, _, _, _, hd) : _ | fromIntegral a - fromIntegral hd > (fromIntegral secStart :: Int64) -> [leadDef (a - fromIntegral hd)]
            (_ : _) -> []
            -- a section (or a piece of one, in this tool's own output) with no symbol at all
            [] -> [leadDef secEnd | secEnd > secStart]
          mk (a, nm, rest, hs, kind, hd) next =
            let st = a - fromIntegral hd
                en = case next of
                  Just (a2, _, _, _, _, hd2) -> a2 - fromIntegral hd2
                  Nothing -> secEnd
             in Def 0 nm rest hs a st (max st en) si kind hd False
          defs = zipWith mk starts (map Just (drop 1 starts) ++ [Nothing])
       in lead ++ defs
    infoLike nm = any (`B.isSuffixOf` nm) ["_info", "_entry"]

-- * Analysis

data Env = Env
  { eImage :: Image,
    eBytes :: B.ByteString,
    eText :: Section,
    eVisited :: VUM.IOVector Bool,
    eTabled :: VUM.IOVector Bool,
    -- | words that belong to an info table (known after the first pass)
    eIsTable :: VUM.IOVector Bool,
    -- | the first pass only discovers labels; its references and anomalies are discarded
    eFirstPass :: IORef Bool,
    -- | labels with info tables, collected by the first pass
    eTableLabels :: IORef [Word64],
    -- | address ranges fixed code or data points at: what they overlap stays put
    ePins :: IORef [(String, Word64, Word64)],
    -- | how many pins of each source: exact adrp, page adrp, branch, pointer, table field
    ePinCounts :: IORef (M.Map String Int),
    eRefs :: IORef (VUM.IOVector (Int, Word64, Word8, Word64)),
    -- | references from slots outside every definition (patched in place; roots)
    eLoose :: IORef [(Word64, Word8, Word64)],
    ePatchFixedSrt :: Bool,
    eRefCount :: IORef Int,
    eLabels :: IORef [(Word64, Bool)],
    eAnomalies :: IORef [String]
  }

analyze :: Options -> Image -> IO Analysis
analyze opts im0 = do
  let im = im0
      bytes = moBytes (imMachO im)
      text = fromMaybe (error "no __text section") (V.find isCode (imSections im))
  visited <- VUM.replicate (fromIntegral (secSize text `div` 4) + 1) False
  tabled <- VUM.replicate (fromIntegral (secSize text `div` 4) + 1) False
  isTable <- VUM.replicate (fromIntegral (secSize text `div` 4) + 1) False
  firstPass <- newIORef True
  tableLabels <- newIORef []
  pins <- newIORef []
  pinCounts <- newIORef M.empty
  refs <- VUM.new (1 `shiftL` 20) >>= newIORef
  loose <- newIORef []
  refCount <- newIORef 0
  labels <- newIORef []
  anomalies <- newIORef []
  let env = Env im bytes text visited tabled isTable firstPass tableLabels pins pinCounts refs loose (optPatchFixedSrt opts) refCount labels anomalies
  -- Fixed code is swept for what it points at; the targets stay put.
  forM_ (V.toList (imDefs im)) $ \d ->
    when (dKind d == Fixed && isCode (imSections im V.! dSect d)) (sweepFixed env d)
  -- Pointer slots are references wherever they are: a pointer in C data is
  -- an absolute address that can be rewritten like any other. A slot in
  -- no definition at all (the GOT, a C constant section) is a loose
  -- reference, patched in place and a root.
  let pointerSlot slot value = case defAt im slot of
        Just d | dKind d == Haskell || optPatchFixedPointers opts -> do
          addRef env (dIndex d) slot RPointer value
          when (inText env value) (pushLabel env value True)
        Just _ -> pinTargetAs env "pointer in fixed data" value
        Nothing | optPatchFixedPointers opts -> addLoose env slot RPointer value
        Nothing -> pinTargetAs env "pointer in fixed data" value
  VU.forM_ (imRebases im) (uncurry pointerSlot)
  -- Every Haskell code definition is decoded from its own label. The first
  -- pass finds the labels and so the info tables; the second decodes with
  -- the tables known, so that a fall-through into one (after an exhaustive
  -- compare chain) stops instead of reading table words as code.
  let seed = forM_ (V.toList (imDefs im)) $ \d ->
        when (dKind d == Haskell && isCode (imSections im V.! dSect d)) (pushLabel env (dSym d) (dHead d > 0))
  seed
  drain env
  tls <- readIORef tableLabels
  forM_ tls $ \l -> case fileOffsetOf im l of
    Just off -> do
      let hd = tableSize bytes (fromIntegral off)
      forM_ [0 .. hd `div` 4 - 1] $ \k -> VUM.write isTable (textIndex env l - 1 - k) True
    Nothing -> pure ()
  writeIORef firstPass False
  writeIORef refCount 0
  writeIORef anomalies []
  VUM.set visited False
  VUM.set tabled False
  -- the pointer references are re-added; the pins from the fixed sweep stay
  writeIORef (eLoose env) []
  VU.forM_ (imRebases im) (uncurry pointerSlot)
  seed
  drain env
  -- References, by address.
  n <- readIORef refCount
  buf <- readIORef refs
  byAddr <- do
    v <- VU.freeze (VUM.slice 0 n buf)
    mv <- VU.thaw (VU.map (\(_, a, k, t) -> (a, k, t)) v)
    VA.sortBy (\(a, _, _) (b, _, _) -> compare a b) mv
    VU.unsafeFreeze mv
  anoms <- reverse <$> readIORef anomalies
  tls <- readIORef tableLabels
  -- Definitions are split where something inside them can live on its
  -- own: in code at a label with a function, thunk or constructor table (a
  -- local closure; return-point tables belong to their procedure), in data
  -- at a referenced closure start (a word that is itself a pointer slot),
  -- in strings at a constructor name.
  let typeAt l = maybe 0 (\o -> u32 bytes (fromIntegral o - 8)) (fileOffsetOf im l)
      isSlot a = let (lo, hi) = (0, VU.length (imRebases im)) in slotSearch lo hi
        where
          slotSearch lo hi
            | lo >= hi = False
            | otherwise =
                let mid = (lo + hi) `div` 2
                    (sa, _) = imRebases im VU.! mid
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
          | l <- tls,
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
          | (_, k, t) <- refTargets,
            k `elem` map (fromIntegral . fromEnum) (optDataSplitKinds opts),
            let t' = t .&. complement 7,
            inRange t',
            Just d <- [defAt im t'],
            dKind d == Haskell,
            secName (sectOf d) == "__data",
            t' - dStart d >= 8,
            isSlot t'
        ]
      strSplits =
        [ (t, 0)
          | (_, k, t) <- refTargets,
            k == fromIntegral (fromEnum RConDesc),
            Just d <- [defAt im t],
            dKind d == Haskell,
            secName (sectOf d) == "__cstring",
            t > dStart d
        ]
      splits = M.fromList ((if optSplitText opts then textSplits else []) ++ (if optSplitData opts then dataSplits else []) ++ (if optSplitStrings opts then strSplits else []))
      refined = V.fromList (zipWith (\i d -> d {dIndex = i}) [0 ..] (concatMap (refine splits) (V.toList (imDefs im))))
      im' = im {imDefs = refined, imStarts = VU.fromList (map dStart (V.toList refined))}
      ndefs' = V.length refined
      lowerAt a =
        let total = VU.length byAddr
            go lo hi
              | lo >= hi = lo
              | otherwise = let mid = (lo + hi) `div` 2; (x, _, _) = byAddr VU.! mid in if x < a then go (mid + 1) hi else go lo mid
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
  pinnedV <- VU.freeze pinnedM
  pinCountsV <- readIORef pinWhy
  looseV <- readIORef loose
  -- Cut by prefix, then mark from the roots.
  -- Only code and closures are cut: a literal a kept definition still uses
  -- has nothing sensible to be replaced by, so it stays.
  let cut = VU.generate ndefs' $ \i ->
        let d = refined V.! i
            sect = sectOf d
         in dKind d == Haskell
              && (isCode sect || secName sect == "__data")
              && any (\p -> any (p `isPrefixOf`) (prefixForms d)) (optRemovePrefixes opts)
      looseTargets = [dIndex d | (_, _, t) <- looseV, Just d <- [defAt im' t]]
      roots = looseTargets ++ [i | i <- [0 .. ndefs' - 1], let d = refined V.! i, dKind d == Fixed || pinnedV VU.! i, not (cut VU.! i)]
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
            let targets = [t | (_, k, t) <- VU.toList (perDef V.! i), follows k]
                nexts = flip mapMaybe targets $ \t -> case defAt im' t of
                  Just d
                    | cut VU.! dIndex d -> Nothing
                    | otherwise -> Just (dIndex d)
                  Nothing -> Nothing
            forM_ targets $ \t -> case defAt im' t of
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
        anRep = repV
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
tableSize :: B.ByteString -> Int -> Int
tableSize bytes off
  | off < 40 = 0
  | otherwise =
      let ty = u32 bytes (off - 8)
       in if
            | ty >= 1 && ty <= 7 -> 24
            | ty >= 8 && ty <= 14 -> let ft = u32 bytes (off - 24) in if ft <= 1 then 40 else 24
            | ty >= 15 && ty <= 22 -> 16
            | ty >= 29 && ty <= 36 -> 16
            | ty >= 55 && ty <= 57 -> 16
            | otherwise -> 0

inText :: Env -> Word64 -> Bool
inText env a = a >= secAddr (eText env) && a < secAddr (eText env) + secSize (eText env)

textIndex :: Env -> Word64 -> Int
textIndex env a = fromIntegral ((a - secAddr (eText env)) `shiftR` 2)

wordAt :: Env -> Word64 -> Word32
wordAt env a = case fileOffsetOf (eImage env) a of
  Just off -> u32 (eBytes env) (fromIntegral off)
  Nothing -> 0

anomaly :: Env -> String -> IO ()
anomaly env s = modifyIORef' (eAnomalies env) (s :)

addRef :: Env -> Int -> Word64 -> RefKind -> Word64 -> IO ()
addRef env di at kind target = do
  n <- readIORef (eRefCount env)
  buf <- readIORef (eRefs env)
  buf' <-
    if n < VUM.length buf
      then pure buf
      else do
        b2 <- VUM.grow buf (VUM.length buf)
        writeIORef (eRefs env) b2
        pure b2
  VUM.write buf' n (di, at, fromIntegral (fromEnum kind), target)
  writeIORef (eRefCount env) (n + 1)

addLoose :: Env -> Word64 -> RefKind -> Word64 -> IO ()
addLoose env at kind target = modifyIORef' (eLoose env) ((at, fromIntegral (fromEnum kind), target) :)

pushLabel :: Env -> Word64 -> Bool -> IO ()
pushLabel env a tbl = when (inText env a) $ modifyIORef' (eLabels env) ((a, tbl) :)

pinRange :: Env -> String -> Word64 -> Word64 -> IO ()
pinRange env why lo hi = do
  modifyIORef' (ePins env) ((why, lo, hi) :)
  modifyIORef' (ePinCounts env) (M.insertWith (+) why 1)

pinTargetAs :: Env -> String -> Word64 -> IO ()
pinTargetAs env why a = pinRange env why a (a + 1)

-- | Pins the 4 KB page an adrp names.
pinPage :: Env -> Word64 -> IO ()
pinPage env page = pinRange env "page" page (page + 4096)

-- | A linear sweep of fixed code for what it refers to.
sweepFixed :: Env -> Def -> IO ()
sweepFixed env d = go (dStart d)
  where
    go pc
      | pc + 4 > dEnd d = pure ()
      | otherwise = do
          case decode pc (wordAt env pc) of
            Adrp rd page ->
              -- the exact targets when the register's uses can be followed, else the page
              case adrpConsumers rd [wordAt env (pc + 4 * k) | k <- [1 .. 8], pc + 4 * k + 4 <= dEnd d] of
                Just offs -> mapM_ (pinTargetAs env "adrp" . (page +)) offs
                Nothing -> pinPage env page
            Adr t -> pinTargetAs env "adr" t
            B t -> pinTargetAs env "branch" t
            Bl t -> pinTargetAs env "branch" t
            LdrLit t -> pinTargetAs env "literal" t
            _ -> pure ()
          go (pc + 4)

drain :: Env -> IO ()
drain env = do
  ls <- readIORef (eLabels env)
  case ls of
    [] -> pure ()
    (a, tbl) : rest -> do
      writeIORef (eLabels env) rest
      case defAt (eImage env) a of
        Just d | dKind d == Haskell -> do
          when tbl (parseTable env d a)
          decodeFrom env d a
        -- GHC code can sit in a fixed region (the unlabeled lead of __text,
        -- a local closure after a C symbol): its instructions are swept,
        -- but its info tables' fields are only found through the labels
        -- that reach them, and what they point at must stay
        Just d | tbl -> parseTable env d a
        _ -> pure ()
      drain env

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
              | isJust (fileOffsetOf (eImage env) target) || target == label =
                  if dKind d == Haskell || (kind == RSrt && ePatchFixedSrt env)
                    then addRef env di (fromIntegral (fromIntegral label + fieldOff :: Int)) kind target
                    else pinTargetAs env "table field in fixed code" target
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

-- | Recursive-descent decoding from a label within a Haskell definition.
decodeFrom :: Env -> Def -> Word64 -> IO ()
decodeFrom env d = go
  where
    di = dIndex d
    inDef a = a >= dStart d && a < dEnd d
    go pc
      | pc + 4 > dEnd d = pure ()
      | otherwise = do
          let ix = textIndex env pc
          seen <- VUM.read (eVisited env) ix
          table <- VUM.read (eIsTable env) ix
          unless (seen || table) $ do
            VUM.write (eVisited env) ix True
            let w = wordAt env pc
            case decode pc w of
              Adrp rd page -> do
                let w2 = wordAt env (pc + 4)
                if
                  | isAdd12For rd w2 -> do
                      let t = page + add12Imm w2
                      addRef env di pc RAdrpAdd t
                      pushLabel env t True
                      VUM.write (eVisited env) (ix + 1) True
                      go (pc + 8)
                  | isLdr12For rd w2 -> do
                      addRef env di pc RAdrpLdr (page + ldr12Imm w2)
                      VUM.write (eVisited env) (ix + 1) True
                      go (pc + 8)
                  | otherwise -> anomaly env ("adrp without add or ldr at " ++ hex pc ++ " in " ++ BC.unpack (dName d))
              B t -> do
                addRef env di pc RB t
                if inDef t then go t else pushLabel env t False
              Bl t -> do
                addRef env di pc RBl t
                -- a call into a Haskell definition's region means C code lives there; it must not move
                when (inDef t || isHaskellAt t) (pinTargetAs env "bl from Haskell" t)
                go (pc + 4)
              BCond t -> branch pc t RBcond
              Cbz t -> branch pc t RCbz
              Tbz t -> branch pc t RTbz
              Adr _ -> anomaly env ("adr at " ++ hex pc ++ " in " ++ BC.unpack (dName d))
              LdrLit _ -> anomaly env ("literal load at " ++ hex pc ++ " in " ++ BC.unpack (dName d))
              Udf -> do
                -- a zero word before an info table is alignment padding after a fall-through
                nextIsTable <- if pc + 4 < dEnd d then VUM.read (eIsTable env) (ix + 1) else pure False
                first <- readIORef (eFirstPass env)
                unless (first || (w == 0 && nextIsTable)) $
                  anomaly env ("data decoded as code at " ++ hex pc ++ " in " ++ BC.unpack (dName d))
              RegJump -> pure ()
              Other -> go (pc + 4)
    branch pc t kind = do
      addRef env di pc kind t
      if inDef t
        then go t >> go (pc + 4)
        else anomaly env ("conditional branch leaves the definition at " ++ hex pc ++ " in " ++ BC.unpack (dName d))
    isHaskellAt t = maybe False ((== Haskell) . dKind) (defAt (eImage env) t)

-- | Identical code folding. Two kept, movable definitions of code or
-- strings whose bytes agree once their reference fields are masked, and
-- whose references agree once their targets are named canonically (the
-- representative of the target's definition and the offset into it, or
-- the fixed address), can share one copy: nothing compares code or string
-- addresses for identity. Static closures are never folded, since a CAF
-- or a constructor is observable by identity. Folding a target makes more
-- pairs equal, so the grouping is iterated to a fixed point.
fold :: Image -> B.ByteString -> V.Vector (VU.Vector (Word64, Word8, Word64)) -> VU.Vector Bool -> VU.Vector Bool -> VU.Vector Bool -> [(Word64, Word64)] -> VU.Vector Int
fold im bytes perDef kept pinned cut ranges = go (VU.generate n id) (5 :: Int)
  where
    n = V.length (imDefs im)
    defs = imDefs im
    candidate i =
      let d = defs V.! i
          sect = imSections im V.! dSect d
          inRange = null ranges || any (\(lo, hi) -> dStart d >= lo && dStart d < hi) ranges
       in kept VU.! i && not (pinned VU.! i) && not (cut VU.! i) && dKind d == Haskell && (isCode sect || secName sect == "__cstring") && dEnd d > dStart d && inRange
    fieldLen :: RefKind -> Int
    fieldLen = \case
      RAdrpAdd -> 8
      RAdrpLdr -> 8
      RSrt -> 4
      RB -> 4
      RBl -> 4
      RBcond -> 4
      RCbz -> 4
      RTbz -> 4
      _ -> 8
    -- the bytes with each reference field's address bits cleared: the
    -- immediate of an instruction (its registers and opcode stay), the whole
    -- of a data field
    masked i = case fileOffsetOf im (dStart (defs V.! i)) of
      Nothing -> B.empty
      Just off ->
        let d = defs V.! i
            raw = B.take (fromIntegral (dEnd d - dStart d)) (B.drop (fromIntegral off) bytes)
            holes = sortOn fst [(fromIntegral (at - dStart d), toEnum (fromIntegral k)) | (at, k, _) <- VU.toList (perDef V.! i)]
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
    maskField k field = case k of
      RAdrpAdd -> word (u32 field 0 .&. 0x9f00001f) <> word (u32 field 4 .&. complement 0x003ffc00)
      RAdrpLdr -> word (u32 field 0 .&. 0x9f00001f) <> word (u32 field 4 .&. complement 0x003ffc00)
      RB -> word (u32 field 0 .&. 0xfc000000)
      RBl -> word (u32 field 0 .&. 0xfc000000)
      RBcond -> word (u32 field 0 .&. complement 0x00ffffe0)
      RCbz -> word (u32 field 0 .&. complement 0x00ffffe0)
      RTbz -> word (u32 field 0 .&. complement 0x0007ffe0)
      _ -> B.replicate (B.length field) 0
    word :: Word32 -> B.ByteString
    word w = B.pack [fromIntegral (w `shiftR` (8 * j)) | j <- [0 .. 3]]
    canon rep i =
      let d = defs V.! i
       in [ (at - dStart d, k, target)
            | (at, k, t) <- VU.toList (perDef V.! i),
              let target = case defAt im t of
                    Just e | dIndex e == i -> Left (-1, t - dStart e) -- self
                    Just e -> Left (rep VU.! dIndex e, t - dStart e)
                    Nothing -> Right t
          ]
    go rep 0 = rep
    go rep k =
      let groups =
            M.fromListWith (\_ old -> old) -- keep the first (lowest index) as the representative
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
