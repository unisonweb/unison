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
    argKeepHaskellExports :: Bool
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

main :: IO ()
main = do
  a <- execParser (info (args <**> helper) (fullDesc <> progDesc "Removes definitions a GHC executable does not use, and compacts it"))
  bytes <- B.readFile (argInput a)
  im <- either die pure (loadImage bytes)
  an <- analyze (Options (argRemove a) (argKeepDead a)) im
  forM_ (argDump a) $ \name -> do
    forM_ [d | d <- V.toList (imDefs im), hsDisplay d == name || BC.unpack (dName d) == name] $ \d -> do
      printf "%s %s [%x, %x) sym %x head %d kept %s pinned %s\n" (hsDisplay d) (show (dKind d)) (dStart d) (dEnd d) (dSym d) (dHead d) (show (anKept an VU.! dIndex d)) (show (anPinned an VU.! dIndex d))
      VU.forM_ (anRefs an V.! dIndex d) $ \(at, k, t) -> printf "  %x %s -> %x %s\n" at (show (toEnum (fromIntegral k) :: RefKind)) t (maybe "" hsDisplay (defAt im t))
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
      pl <- either die pure (plan an (argHoles a) (not (argTextInOrder a)))
      printf "%d segments; %d holes (%d bytes) dropped from the file; %d branch islands\n" (length (plPieces pl)) (length (plHoles pl)) (plSaved pl) (M.size (plIslands pl))
      (bytes', stats) <- rewrite an pl (argKeepHaskellExports a)
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
