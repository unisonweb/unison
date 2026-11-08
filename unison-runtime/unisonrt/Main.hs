{-# LANGUAGE CPP #-}

-- | unisonrt: runs a compiled Unison program (a .uc file, which ucm's
-- `compile` writes), with no codebase.
--
--   unisonrt [+RTS <GHC RTS flags> -RTS] [--jit MODE] [--jit-dump-ir DIR] FILE.uc [ARGS]
--
-- The flags come before the file; everything after it is the program's.
-- See unisonrt/README.md.
module Main (main) where

import Control.Concurrent (runInUnboundThread)
import Control.Exception (SomeException (..), catch, displayException, finally, fromException)
import Control.Monad (when)
import Data.ByteString qualified as B
import Data.Foldable (for_)
import Data.List (isPrefixOf, stripPrefix)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import GHC.Stack (prettyCallStack)
import System.Environment (getArgs, getExecutablePath, setEnv, withArgs)
import System.Exit (ExitCode (..), exitWith)
#if !defined(mingw32_HOST_OS)
import System.Posix.Files (setFileMode)
#endif
import Bundle (readBundle, writeBundle)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Runtime.Exception (RuntimeExn (..))
import Unison.Runtime.JIT qualified as JIT
import Unison.Runtime.JIT.Config qualified as JITConfig
import Unison.Runtime.Render (renderTracer, renderValue)
import Unison.Runtime.Standalone
import Unison.Runtime.Version (runtimeVersion)
import Unison.ShortHash qualified as SH

data Flags = Flags
  { jitMode :: Maybe String,
    dumpIR :: Maybe String
  }

usage :: String
usage =
  unlines
    [ "usage: unisonrt [+RTS <GHC RTS flags> -RTS] [--jit MODE] [--jit-dump-ir DIR] FILE.uc [ARGS]",
      "",
      "Runs a program compiled by ucm's `compile` command. The flags come",
      "before the file; the arguments after it are the program's.",
      "",
      "  --jit MODE          JIT compilation: off, on or eager (UNISON_JIT)",
      "  --jit-dump-ir DIR   write the LLVM IR the JIT generates to DIR, or to",
      "                      stderr if DIR is '-' (UNISON_JIT_DUMP_IR)",
      "  --version           print the version of Unison this runs programs of",
      "",
      "       unisonrt bundle FILE.uc [-o OUTPUT]",
      "",
      "Writes a self-contained executable that runs FILE.uc (named FILE by",
      "default). All of its arguments are the program's; GHC RTS flags can be",
      "given in the GHCRTS environment variable."
    ]

-- | The flags, the file, and the program's arguments. The flags that take
-- a value as the next argument are also listed in main.c.
parseArgs :: Flags -> [String] -> Either String (Flags, FilePath, [String])
parseArgs flags = \case
  ("--jit" : mode : rest) -> parseArgs flags {jitMode = Just mode} rest
  ("--jit-dump-ir" : dir : rest) -> parseArgs flags {dumpIR = Just dir} rest
  (arg : rest)
    | Just mode <- stripPrefix "--jit=" arg -> parseArgs flags {jitMode = Just mode} rest
    | Just dir <- stripPrefix "--jit-dump-ir=" arg -> parseArgs flags {dumpIR = Just dir} rest
    | "-" `isPrefixOf` arg -> Left ("unknown flag " ++ arg)
    | otherwise -> Right (flags, arg, rest)
  [] -> Left "no .uc file given"

main :: IO ()
main = (`catch` escaped) do
  self <- getExecutablePath
  readBundle self >>= \case
    -- a bundle runs its program, and every argument is the program's
    Just program -> getArgs >>= runWith (Flags Nothing Nothing) self program
    Nothing ->
      getArgs >>= \case
        ["--version"] -> Text.putStrLn runtimeVersion
        ["--help"] -> putStr usage
        ("bundle" : rest) -> bundle self rest
        args -> case parseArgs (Flags Nothing Nothing) args of
          Left err -> failWith (err ++ "\n\n" ++ usage)
          Right (flags, file, progArgs) -> do
            program <- B.readFile file
            runWith flags file program progArgs

-- | Runs a compiled program, given as the bytes of a .uc file.
runWith :: Flags -> FilePath -> B.ByteString -> [String] -> IO ()
runWith flags file program progArgs = do
  -- the runtime reads its JIT settings from the environment, once
  for_ (jitMode flags) (setEnv "UNISON_JIT")
  for_ (dumpIR flags) \dir -> setEnv "UNISON_JIT_DEBUG" "1" >> setEnv "UNISON_JIT_DUMP_IR" dir
  jit <- JIT.newJIT =<< JIT.readConfig
  runInUnboundThread (run jit file program progArgs `catch` escaped) `finally` JIT.jitShutdown jit

-- | An exception the runtime didn't turn into a Unison failure (an
-- arithmetic error, a missing file), which would otherwise reach GHC's
-- handler.
escaped :: SomeException -> IO ()
escaped e = case fromException e of
  Just (code :: ExitCode) -> exitWith code
  Nothing -> failWith (displayExceptionWithoutContext e)
  where
    displayExceptionWithoutContext (SomeException x) = displayException x

run :: JIT.JIT -> FilePath -> B.ByteString -> [String] -> IO ()
run jit file bytes progArgs = do
  loadJIT jit
  compiled <- decodeProgram file bytes
  let names = compiledConstructorNames compiled
  cc <- loadStandalone (renderTracer names) jit False (compiledCode compiled)
  result <- withArgs progArgs (runMainComb (compiledEntry compiled) cc)
  JIT.printJITStats jit
  either (failWith . describe names) pure result

-- | A .uc file's program, if this version of the runtime can run it.
decodeProgram :: FilePath -> B.ByteString -> IO Compiled
decodeProgram file bytes = do
  compiled <-
    decodeStandalone bytes >>= \case
      Left err -> failWith (file ++ " is not a compiled program: " ++ err)
      Right c -> pure c
  when (compiledVersion compiled /= runtimeVersion) . failWith . unlines $
    [ file ++ " was compiled by a different version of Unison.",
      "  compiled by: " ++ Text.unpack (compiledVersion compiled),
      "  this is:     " ++ Text.unpack runtimeVersion,
      "Compile it again (its main is " ++ Text.unpack (compiledMain compiled) ++ ") with a ucm of this version."
    ]
  pure compiled

-- | unisonrt bundle FILE.uc [-o OUTPUT]: a self-contained executable that
-- runs FILE.uc, named FILE by default.
bundle :: FilePath -> [String] -> IO ()
bundle self = \case
  [file] -> go file (defaultOutput file)
  [file, "-o", out] -> go file out
  ["-o", out, file] -> go file out
  _ -> failWith "usage: unisonrt bundle FILE.uc [-o OUTPUT]"
  where
    defaultOutput file = maybe (file ++ ".bundle") reverse (stripPrefix "cu." (reverse file))
    go file out = do
      program <- B.readFile file
      -- refuse what the bundle could not run
      _ <- decodeProgram file program
      writeBundle self program out >>= \case
        Left err -> failWith ("can't bundle " ++ file ++ ": " ++ err)
        Right () -> do
          makeExecutable out
          putStrLn ("unisonrt: wrote " ++ out)

-- | Lets everyone run it, as the linker leaves an executable
makeExecutable :: FilePath -> IO ()
#if defined(mingw32_HOST_OS)
makeExecutable _ = pure ()
#else
makeExecutable out = setFileMode out 0o755
#endif

-- | With the JIT on, loads LLVM. When it can't be, says why and runs the
-- program interpreted, as with the JIT off.
loadJIT :: JIT.JIT -> IO ()
loadJIT jit = case JITConfig.mode (JIT.jitConfig jit) of
  JITConfig.Off -> pure ()
  _ ->
    JIT.loadJIT jit >>= \case
      Just (Left msgs) -> mapM_ (hPutStrLn stderr . ("unisonrt: " ++)) msgs
      _ -> pure ()

-- | What went wrong, for a program that failed.
describe :: ConstructorNames -> RuntimeExn -> String
describe names = \case
  BU trace name value ->
    unlines $
      (what (Text.unpack name) ++ Text.unpack (renderValue names value))
        : ["  at " ++ frame r n | (r, n) <- trace]
  PE stack issues msg ->
    unlines $
      ["internal error: " ++ Text.unpack msg]
        ++ ["  (this may be GitHub issue #" ++ show i ++ ")" | i <- issues]
        ++ [prettyCallStack stack]
  where
    what = \case
      "builtin.raise" -> "unhandled exception: "
      "builtin.bug" -> "bug: "
      name -> name ++ ": "
    frame :: Reference -> Int -> String
    frame r n = Text.unpack (SH.toText (SH.shortenTo 8 (Reference.toShortHash r))) ++ (if n > 1 then " (" ++ show n ++ " copies)" else "")

failWith :: String -> IO a
failWith msg = do
  -- the program's output first, which is buffered when it isn't a terminal
  hFlush stdout
  hPutStrLn stderr ("unisonrt: " ++ dropWhileEnd' msg)
  exitWith (ExitFailure 1)
  where
    dropWhileEnd' = reverse . dropWhile (== '\n') . reverse
