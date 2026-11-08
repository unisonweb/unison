{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main (main) where

import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Ord (Down (..))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Minify.Analysis
import Minify.Layout
import Minify.MachO
import Minify.Rewrite
import Minify.Verify
import System.Process (callProcess)
import Minify.ZEncoding
import Options.Applicative
import System.Exit
import System.IO
import Text.Printf
import Numeric (readHex)

data Args = Args
  { argInput :: FilePath,
    argOutput :: Maybe FilePath,
    argRemove :: [String],
    argKeepDead :: Bool,
    argReport :: Bool,
    argVerify :: Bool,
    argTop :: Int,
    argHoles :: Int,
    argDump :: Maybe String,
    argTextInOrder :: Bool,
    argForce :: Bool,
    argKeepHaskellExports :: Bool,
    argStrip :: Bool,
    argKeepConDesc :: Bool,
    argNoSplit :: [String],
    argRefsTo :: Maybe String,
    argSplitKinds :: Maybe String,
    argSplitRange :: Maybe String,
    argPack :: Maybe String,
    argNoFixedPatch :: [String],
    argCross :: Maybe String,
    argFold :: Bool,
    argFoldRange :: Maybe String,
    argWhere :: [String],
    argFoldKeepCopies :: Bool,
    argFoldZeroCopies :: Bool,
    argPadAfter :: Maybe String
  }

args :: Parser Args
args =
  Args
    <$> strArgument (metavar "INPUT" <> help "A GHC-linked Mach-O executable with its symbol table")
    <*> optional (strOption (short 'o' <> long "output" <> metavar "OUTPUT" <> help "Where to write the minified executable; without it, only the report is printed"))
    <*> many (strOption (long "remove" <> short 'r' <> metavar "PREFIX" <> help "Remove definitions whose name (Module.binding or package:Module.binding) starts with PREFIX, then whatever only they referred to"))
    <*> switch (long "keep-dead" <> help "Do not remove unreferenced definitions (only compact and apply --remove)")
    <*> switch (long "report" <> help "Print the report even when writing an output")
    <*> switch (long "verify" <> help "Analyze the written file and check that every reference survived")
    <*> option auto (long "top" <> value 25 <> showDefault <> metavar "N" <> help "How many packages to list in the report")
    <*> option auto (long "holes" <> value 200 <> showDefault <> metavar "N" <> help "At most this many runs of dead pages are dropped from the file (each adds a segment)")
    <*> optional (strOption (long "dump" <> metavar "NAME" <> help "Print the references of the definition with this decoded name and stop"))
    <*> switch (long "text-in-order" <> help "Keep the segments in address order in the load commands, which limits the holes to 11 (the extra __TEXT pieces otherwise come after the data segments)")
    <*> switch (long "force" <> help "Write the output even after anomalies in the analysis")
    <*> switch (long "keep-haskell-exports" <> help "Keep Haskell definitions in the export trie (dlsym by name); by default only the other symbols stay exported")
    <*> switch (long "strip" <> help "Drop the symbol table and function starts from the output (the output then cannot be minified again)")
    <*> switch (long "keep-con-desc" <> help "Keep constructor name strings (used by heap profiling and GHC.Exts.Heap); by default they are dropped")
    <*> many (strOption (long "no-split" <> metavar "text|data|strings" <> help "Do not split definitions in this section (debugging)"))
    <*> optional (strOption (long "refs-to" <> metavar "0xADDR" <> help "Print every reference into the definition containing this address and stop"))
    <*> optional (strOption (long "data-split-kinds" <> metavar "pointer,srt,adrp" <> help "Debugging: which reference kinds may split data definitions"))
    <*> optional (strOption (long "split-range" <> metavar "0xLO-0xHI" <> help "Debugging: only split data definitions at addresses in this range"))
    <*> optional (strOption (long "pack" <> metavar "__text,__data,__cstring" <> help "Debugging: which sections' definitions are packed first-fit (default all)"))
    <*> many (strOption (long "no-fixed-patch" <> metavar "pointers|srt" <> help "Debugging: pin instead of rewriting these in fixed regions"))
    <*> optional (strOption (long "cross" <> metavar "0xLO-0xHI|none" <> help "Debugging: only definitions in this address range (or none) may move before a fixed definition they followed; by default all may"))
    <*> switch (long "fold" <> help "Fold identical code and strings onto one copy (off by default until a wrong result in jit-tests.md is understood, 2026-11-08)")
    <*> optional (strOption (long "fold-range" <> metavar "0xLO-0xHI" <> help "Debugging: only fold definitions starting in this range"))
    <*> many (strOption (long "where" <> metavar "0xADDR" <> help "Debugging: print where this input address goes in the output (with -o, after planning)"))
    <*> switch (long "fold-keep-copies" <> help "Debugging: redirect references to the representative but keep the folded copies in place")
    <*> switch (long "fold-zero-copies" <> help "Debugging: with --fold-keep-copies, reserve the folded copies' space but zero their bytes")
    <*> optional (strOption (long "pad-after" <> metavar "0xADDR:N" <> help "Debugging: leave N zero bytes after the definition starting at ADDR"))

main :: IO ()
main = do
  a <- execParser (info (args <**> helper) (fullDesc <> progDesc "Removes definitions a GHC executable does not use, and compacts it"))
  bytes <- B.readFile (argInput a)
  im <- either die pure (loadImage bytes)
  let foldRange = case argFoldRange a of
        Just spec | (lo, '-' : hi) <- break (== '-') spec, [(l, "")] <- readHex (drop 2 lo), [(h, "")] <- readHex (drop 2 hi) -> Just (l, h)
        _ -> Nothing
      kinds = case argSplitKinds a of
        Nothing -> [RPointer, RAdrpAdd, RSrt]
        Just spec -> [k | (w, k) <- [("pointer", RPointer), ("adrp", RAdrpAdd), ("srt", RSrt)], w `elem` splitOn ',' spec]
      range = case argSplitRange a of
        Just spec | (lo, '-' : hi) <- break (== '-') spec, [(l, "")] <- readHex (drop 2 lo), [(h, "")] <- readHex (drop 2 hi) -> Just (l, h)
        _ -> Nothing
      splitOn c str = case break (== c) str of
        (x, []) -> [x]
        (x, _ : rest) -> x : splitOn c rest
  an <- analyze (Options (argRemove a) (argKeepDead a) (argKeepConDesc a) ("text" `notElem` argNoSplit a) ("data" `notElem` argNoSplit a) ("strings" `notElem` argNoSplit a) kinds range ("pointers" `notElem` argNoFixedPatch a) ("srt" `notElem` argNoFixedPatch a) (argFold a) foldRange) im
  forM_ (argRefsTo a) $ \spec -> do
    let im' = anImage an
    case spec of
      '0' : 'x' : hexDigits | [(addr, "")] <- readHex hexDigits, Just target <- defAt im' addr -> do
        printf "%s [%x, %x) kept %s pinned %s\n" (hsDisplay target) (dStart target) (dEnd target) (show (anKept an VU.! dIndex target)) (show (anPinned an VU.! dIndex target))
        forM_ (V.toList (imDefs im')) $ \d ->
          VU.forM_ (anRefs an V.! dIndex d) $ \(at, k, t) ->
            when (t >= dStart target && t < dEnd target) $
              printf "  from %s at %x: %s -> %x (kept %s)\n" (hsDisplay d) at (show (toEnum (fromIntegral k) :: RefKind)) t (show (anKept an VU.! dIndex d))
      _ -> putStrLn "no definition there"
    exitSuccess
  forM_ (argDump a) $ \name -> do
    -- a name, or a hex address (the definition containing it and its neighbours)
    let picked = case name of
          '0' : 'x' : hexDigits | [(addr, "")] <- readHex hexDigits ->
            case defAt (anImage an) addr of
              Just d -> [imDefs (anImage an) V.! j | j <- [max 0 (dIndex d - 2) .. min (V.length (imDefs (anImage an)) - 1) (dIndex d + 2)]]
              Nothing -> []
          _ | (lo, '-' : hi) <- break (== '-') name, [(l, "")] <- readHex (drop 2 lo), [(h, "")] <- readHex (drop 2 hi) ->
            [d | d <- V.toList (imDefs (anImage an)), dStart d < h, dEnd d > l]
          _ -> [d | d <- V.toList (imDefs (anImage an)), hsDisplay d == name || BC.unpack (dName d) == name]
    forM_ picked $ \d -> do
      printf "%s %s [%x, %x) sym %x head %d kept %s pinned %s%s\n" (hsDisplay d) (show (dKind d)) (dStart d) (dEnd d) (dSym d) (dHead d) (show (anKept an VU.! dIndex d)) (show (anPinned an VU.! dIndex d)) (if anRep an VU.! dIndex d /= dIndex d then " folded onto " ++ hsDisplay (imDefs (anImage an) V.! (anRep an VU.! dIndex d)) else "")
      VU.forM_ (anRefs an V.! dIndex d) $ \(at, k, t) -> printf "  %x %s -> %x %s\n" at (show (toEnum (fromIntegral k) :: RefKind)) t (maybe "" hsDisplay (defAt (anImage an) t))
    exitSuccess
  when (argReport a || argOutput a == Nothing) (report (argTop a) an)
  unless (null (anAnomalies an)) $ do
    hPutStrLn stderr (show (length (anAnomalies an)) ++ " anomalies; the first 40:")
    mapM_ (hPutStrLn stderr . ("  " ++)) (take 40 (anAnomalies an))
    -- an anomaly means code this tool would move was not understood; the output would be unsound
    when (argOutput a /= Nothing && not (argForce a)) $
      die "not writing an output after anomalies (--force overrides)"
  case argOutput a of
    Nothing -> pure ()
    Just out -> do
      let packs = maybe ["__text", "__data", "__cstring", "__const"] (map BC.pack . splitOn ',') (argPack a)
      let cross = case argCross a of
            Nothing -> Just (0, maxBound)
            Just "none" -> Nothing
            Just "all" -> Just (0, maxBound)
            Just spec | (lo, '-' : hi) <- break (== '-') spec, [(l, "")] <- readHex (drop 2 lo), [(h, "")] <- readHex (drop 2 hi) -> Just (l, h)
            _ -> Nothing
      let padAfter = case argPadAfter a of
            Just spec | (addr, ':' : n) <- break (== ':') spec, [(ad, "")] <- readHex (drop 2 addr), [(k, "")] <- reads n -> Just (ad, k)
            _ -> Nothing
      pl <- either die pure (plan an (argHoles a) (not (argTextInOrder a)) packs cross (argFoldKeepCopies a) padAfter)
      forM_ (argWhere a) $ \spec -> case readHex (drop 2 spec) of
        [(addr, "")] -> case defAt (anImage an) addr of
          Just d -> printf "%s: %s [%x,%x) kept %s rep %s -> %s\n" spec (hsDisplay d) (dStart d) (dEnd d) (show (anKept an VU.! dIndex d)) (hsDisplay (imDefs (anImage an) V.! (anRep an VU.! dIndex d))) (maybe "nowhere" (printf "%x") (newAddrOf an pl addr) :: String)
          Nothing -> printf "%s: in no definition\n" spec
        _ -> pure ()
      printf "%d segments; %d holes (%d bytes) dropped from the file; %d branch islands\n" (length (plPieces pl)) (length (plHoles pl)) (plSaved pl) (M.size (plIslands pl))
      -- dead bytes per compacted section that stay in the file (not in a dropped hole)
      forM_ (V.toList (imSections (anImage an))) $ \sc -> when (isCompactedSection sc) $ do
        let lo = secAddr sc
            hi = lo + secSize sc
            keptBytes = sum [dEnd d - dStart d | d <- V.toList (imDefs (anImage an)), anKept an VU.! dIndex d, dSect d == maybe 0 id (V.findIndex (\x -> secAddr x == secAddr sc) (imSections (anImage an)))]
            dropped = sum [min hi e - max lo s | (s, e) <- plHoles pl, e > lo, s < hi]
            si = maybe 0 id (V.findIndex (\x -> secAddr x == secAddr sc) (imSections (anImage an)))
            anchors = length [() | d <- V.toList (imDefs (anImage an)), dSect d == si, anKept an VU.! dIndex d, dKind d == Fixed || anPinned an VU.! dIndex d]
            holesHere = [(s, e) | (s, e) <- plHoles pl, e > lo, s < hi]
        printf "  %s,%s: %d dead bytes, %d dropped in %d holes, %d remain in the file; %d anchors\n" (BC.unpack (secSegName sc)) (BC.unpack (secName sc)) (secSize sc - keptBytes) dropped (length holesHere) (secSize sc - keptBytes - dropped) anchors
      (bytes', stats) <- rewrite an pl (Choices (argKeepHaskellExports a) (argStrip a) (argFoldZeroCopies a))
      B.writeFile out bytes'
      when (argVerify a) $ do
        problems <- verify an pl bytes'
        if null problems
          then putStrLn "verify: every reference in the output resolves as planned"
          else do
            hPutStrLn stderr ("verify: " ++ show (length problems) ++ " problems; the first 30:")
            mapM_ (hPutStrLn stderr . ("  " ++)) (take 30 problems)
            exitFailure
      printf "symbols kept %d, dropped %d; rebases %d, binds %d, exports %d; link-edit %d bytes\n" (stSymbolsKept stats) (stSymbolsDropped stats) (stRebases stats) (stBinds stats) (stExports stats) (stLinkeditBytes stats)
      printf "%d -> %d bytes (%.1f%%)\n" (B.length bytes) (stFileBytes stats) (100 * fromIntegral (stFileBytes stats) / fromIntegral (B.length bytes) :: Double)
      callProcess "codesign" ["-f", "-s", "-", out]

report :: Int -> Analysis -> IO ()
report top an = do
  let im = anImage an
      defs = V.toList (imDefs im)
      sects = imSections im
      kept i = anKept an VU.! i
      size d = fromIntegral (dEnd d - dStart d) :: Int
      sectName d = let s = sects V.! dSect d in BC.unpack (secSegName s) ++ "," ++ BC.unpack (secName s)
      bySect =
        M.toList $
          M.fromListWith
            add4
            [ (sectName d, (if kept (dIndex d) then (1, size d, 0, 0) else (0, 0, 1, size d)))
              | d <- defs
            ]
      add4 :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
      add4 (a, b, c, d) (e, f, g, h) = (a + e, b + f, c + g, d + h)
      total = sum [size d | d <- defs]
      removed = sum [size d | d <- defs, not (kept (dIndex d))]
      fixedCount = length [() | d <- defs, dKind d == Fixed]
      pinnedCount = VU.length (VU.filter id (anPinned an))
      refsTotal = sum [VU.length r | r <- V.toList (anRefs an)]
  printf "%d definitions (%d fixed, %d pinned), %d references, %d roots\n" (length defs) fixedCount pinnedCount refsTotal (anRoots an)
  printf "pins by source: %s\n" (show (M.toList (anPinCounts an)))
  let folded = [d | d <- defs, anKept an VU.! dIndex d, anRep an VU.! dIndex d /= dIndex d]
  printf "folded: %d definitions, %d bytes, onto identical copies\n" (length folded) (sum (map size folded))
  forM_ (take 12 (sortOn (Down . size) folded)) $ \d ->
    printf "  %d bytes: %s -> %s\n" (size d) (hsDisplay d) (hsDisplay (imDefs im V.! (anRep an VU.! dIndex d)))
  printf "%-20s %12s %12s %12s %12s\n" ("section" :: String) ("kept" :: String) ("kept bytes" :: String) ("removed" :: String) ("removed bytes" :: String)
  forM_ bySect $ \(s, (kc, kb, rc, rb)) -> printf "%-20s %12d %12d %12d %12d\n" s kc kb rc rb
  printf "removable: %d of %d bytes (%.1f%%)\n" removed total (100 * fromIntegral removed / fromIntegral (max 1 total) :: Double)
  let byPkg =
        M.toList $
          M.fromListWith
            add4
            [ (maybe "(fixed)" (packageBase . hsPackage) (dHs d), if kept (dIndex d) then (1, size d, 0, 0) else (0, 0, 1, size d))
              | d <- defs
            ]
  putStrLn ""
  printf "%-50s %8s %12s %8s %12s\n" ("package" :: String) ("kept" :: String) ("kept bytes" :: String) ("removed" :: String) ("removed bytes" :: String)
  forM_ (take top (sortOn (\(_, (_, _, _, rb)) -> Down rb) byPkg)) $ \(p, (kc, kb, rc, rb)) ->
    printf "%-50s %8d %12d %8d %12d\n" (take 50 p) kc kb rc rb
  let cutRef = [d | d <- defs, anCutReferenced an VU.! dIndex d]
  unless (null cutRef) $ do
    printf "\n%d removed definitions are still referred to and become aborts:\n" (length cutRef)
    forM_ (take 40 cutRef) $ \d -> putStrLn ("  " ++ hsDisplay d)
