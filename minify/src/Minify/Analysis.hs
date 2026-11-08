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
import Data.Maybe (fromMaybe, isJust, mapMaybe)
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
    dHead :: !Int
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
    optKeepDead :: Bool
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
    anRoots :: Int
  }

isCompactedSection :: Section -> Bool
isCompactedSection s =
  (secSegName s, secName s) `elem` [("__TEXT", "__text"), ("__TEXT", "__cstring"), ("__TEXT", "__const"), ("__DATA", "__data")]

isCode :: Section -> Bool
isCode s = secSegName s == "__TEXT" && secName s == "__text"

hsDisplay :: Def -> String
hsDisplay d = maybe (BC.unpack (dName d)) displayName (dHs d)

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
carve headOf sects named = V.fromList (zipWith (\i d -> d {dIndex = i}) [0 ..] (concat perSection))
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
          lead = case starts of
            (a, _, _, _, _, hd) : _ | fromIntegral a - fromIntegral hd > (fromIntegral secStart :: Int64) ->
              [Def 0 (BC.pack ("<" ++ BC.unpack (secName s) ++ " lead>")) [] Nothing secStart secStart (a - fromIntegral hd) si Fixed 0]
            _ -> []
          mk (a, nm, rest, hs, kind, hd) next =
            let st = a - fromIntegral hd
                en = case next of
                  Just (a2, _, _, _, _, hd2) -> a2 - fromIntegral hd2
                  Nothing -> secEnd
             in Def 0 nm rest hs a st (max st en) si kind hd
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
    ePinned :: VUM.IOVector Bool,
    eRefs :: IORef (VUM.IOVector (Int, Word64, Word8, Word64)),
    eRefCount :: IORef Int,
    eLabels :: IORef [(Word64, Bool)],
    eAnomalies :: IORef [String]
  }

analyze :: Options -> Image -> IO Analysis
analyze opts im0 = do
  let im = im0
      bytes = moBytes (imMachO im)
      text = fromMaybe (error "no __text section") (V.find isCode (imSections im))
      ndefs = V.length (imDefs im)
  visited <- VUM.replicate (fromIntegral (secSize text `div` 4) + 1) False
  tabled <- VUM.replicate (fromIntegral (secSize text `div` 4) + 1) False
  isTable <- VUM.replicate (fromIntegral (secSize text `div` 4) + 1) False
  firstPass <- newIORef True
  tableLabels <- newIORef []
  pinned <- VUM.replicate ndefs False
  refs <- VUM.new (1 `shiftL` 20) >>= newIORef
  refCount <- newIORef 0
  labels <- newIORef []
  anomalies <- newIORef []
  let env = Env im bytes text visited tabled isTable firstPass tableLabels pinned refs refCount labels anomalies
  -- Fixed code is swept for what it points at; the targets stay put.
  forM_ (V.toList (imDefs im)) $ \d ->
    when (dKind d == Fixed && isCode (imSections im V.! dSect d)) (sweepFixed env d)
  -- Pointer slots: inside Haskell data they are references; elsewhere they pin.
  VU.forM_ (imRebases im) $ \(slot, value) ->
    case defAt im slot of
      Just d | dKind d == Haskell -> do
        addRef env (dIndex d) slot RPointer value
        when (inText env value) (pushLabel env value True)
      _ -> pinTarget env value
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
  VU.forM_ (imRebases im) $ \(slot, value) ->
    case defAt im slot of
      Just d | dKind d == Haskell -> do
        addRef env (dIndex d) slot RPointer value
        when (inText env value) (pushLabel env value True)
      _ -> pure ()
  seed
  drain env
  -- Collect references per definition.
  n <- readIORef refCount
  buf <- readIORef refs
  sorted <- do
    v <- VU.freeze (VUM.slice 0 n buf)
    mv <- VU.thaw v
    VA.sortBy (\(a, b, _, _) (c, e, _, _) -> compare (a, b) (c, e)) mv
    VU.unsafeFreeze mv
  let perDef = V.generate ndefs $ \i ->
        let lo = lowerBound sorted i
            hi = lowerBound sorted (i + 1)
         in VU.map (\(_, a, k, t) -> (a, k, t)) (VU.slice lo (hi - lo) sorted)
  pinnedV <- VU.freeze pinned
  anoms <- reverse <$> readIORef anomalies
  -- Cut by prefix, then mark from the roots.
  -- Only code and closures are cut: a literal a kept definition still uses
  -- has nothing sensible to be replaced by, so it stays.
  let cut = VU.generate ndefs $ \i ->
        let d = imDefs im V.! i
            sect = imSections im V.! dSect d
         in dKind d == Haskell
              && (isCode sect || secName sect == "__data")
              && any (\p -> any (p `isPrefixOf`) (prefixForms d)) (optRemovePrefixes opts)
      roots = [i | i <- [0 .. ndefs - 1], let d = imDefs im V.! i, dKind d == Fixed || pinnedV VU.! i, not (cut VU.! i)]
  kept <- VUM.replicate ndefs False
  cutRef <- VUM.replicate ndefs False
  let mark [] = pure ()
      mark (i : rest) = do
        seen <- VUM.read kept i
        if seen
          then mark rest
          else do
            VUM.write kept i True
            let targets = [t | (_, _, t) <- VU.toList (perDef V.! i)]
                nexts = flip mapMaybe targets $ \t -> case defAt im t of
                  Just d
                    | cut VU.! dIndex d -> Nothing
                    | otherwise -> Just (dIndex d)
                  Nothing -> Nothing
            forM_ targets $ \t -> case defAt im t of
              Just d | cut VU.! dIndex d -> VUM.write cutRef (dIndex d) True
              _ -> pure ()
            mark (nexts ++ rest)
  if optKeepDead opts
    then forM_ [0 .. ndefs - 1] $ \i -> VUM.write kept i (not (cut VU.! i))
    else mark roots
  keptV <- VU.freeze kept
  cutRefV <- VU.freeze cutRef
  pure
    Analysis
      { anImage = im,
        anRefs = perDef,
        anPinned = pinnedV,
        anKept = keptV,
        anCut = cut,
        anCutReferenced = cutRefV,
        anAnomalies = anoms,
        anRoots = length roots
      }
  where
    lowerBound v i =
      let n = VU.length v
          go lo hi
            | lo >= hi = lo
            | otherwise =
                let mid = (lo + hi) `div` 2
                    (di, _, _, _) = v VU.! mid
                 in if di < i then go (mid + 1) hi else go lo mid
       in go 0 n

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

pushLabel :: Env -> Word64 -> Bool -> IO ()
pushLabel env a tbl = when (inText env a) $ modifyIORef' (eLabels env) ((a, tbl) :)

pinDef :: Env -> Def -> IO ()
pinDef env d = when (dKind d == Haskell) (VUM.write (ePinned env) (dIndex d) True)

pinTarget :: Env -> Word64 -> IO ()
pinTarget env a = mapM_ (pinDef env) (defAt (eImage env) a)

-- | Pins every Haskell definition overlapping the 4 KB page an adrp names.
pinPage :: Env -> Word64 -> IO ()
pinPage env page = do
  let im = eImage env
      end = page + 4096
      -- first definition whose end is past the page start
      n = V.length (imDefs im)
      go lo hi
        | lo >= hi = lo
        | otherwise = let mid = (lo + hi) `div` 2 in if dEnd (imDefs im V.! mid) <= page then go (mid + 1) hi else go lo mid
      from = go 0 n
      loop i
        | i >= n = pure ()
        | otherwise = do
            let d = imDefs im V.! i
            when (dStart d < end) $ do
              pinDef env d
              loop (i + 1)
  loop from

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
                Just offs -> mapM_ (pinTarget env . (page +)) offs
                Nothing -> pinPage env page
            Adr t -> pinTarget env t
            B t -> pinTarget env t
            Bl t -> pinTarget env t
            LdrLit t -> pinTarget env t
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
            check :: RefKind -> Int -> Word64 -> IO ()
            check kind fieldOff target =
              if isJust (fileOffsetOf (eImage env) target) || target == label
                then addRef env di (fromIntegral (fromIntegral label + fieldOff :: Int)) kind target
                else anomaly env (show kind ++ " at " ++ hex label ++ " in " ++ BC.unpack (dName d) ++ " points outside the file: " ++ hex target)
        if hd == 0 || label - fromIntegral hd < dStart d
          then anomaly env ("no info table before label " ++ hex label ++ " in " ++ BC.unpack (dName d))
          else do
            let srt = i32 bytes (off - 4)
            when (srt /= 0) (check RSrt (-4) (fromIntegral (fromIntegral label + fromIntegral srt :: Int64)))
            when (ty >= 1 && ty <= 7) (check RConDesc (-24) (rel (-24)))
            when (ty >= 8 && ty <= 14 && hd == 40) $ do
              check RSlowApply (-40) (rel (-40))
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
                when (inDef t || isHaskellAt t) (pinTarget env t)
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
