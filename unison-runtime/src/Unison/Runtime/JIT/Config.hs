-- | Settings for the JIT, read once from the environment.
-- The mode also has a command-line option, @--jit@, which overrides the
-- variable. How to use these is in development.md next to this module.
module Unison.Runtime.JIT.Config
  ( Mode (..),
    Config (..),
    config,
    jitLog,
    jitDump,
    outputHook,
    noteOutput,
  )
where

import Control.Monad (join, when)
import Data.Char (toLower)
import Data.IORef (IORef, newIORef, readIORef)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe, mapMaybe)
import System.Environment (lookupEnv)
import System.IO (hFlush, hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)

data Mode
  = -- | never compile
    Off
  | -- | compile what gets hot, in the background (LLVM is loaded at startup)
    On
  | -- | compile everything as it is loaded (for testing)
    Eager
  deriving (Show, Eq)

data Config = Config
  { mode :: Mode,
    -- | with 'On': interpreted calls before a definition is compiled
    -- (@UNISON_JIT_THRESHOLD@)
    threshold :: Int,
    -- | with 'On': the most definitions compiled together as one module
    -- (@UNISON_JIT_BATCH@)
    batch :: Int,
    -- | with 'On': the estimated calls between a definition and the batch
    -- being formed that make it join the batch (@UNISON_JIT_BATCH_GATE@;
    -- a quarter of the threshold by default). See JIT.formBatch.
    batchGate :: Int,
    -- | with 'On': the size of a typical definition, in units of its frame
    -- size times its call sites (the generated code grows with both). A
    -- candidate's estimated calls are divided by its size in these units,
    -- so that a large definition needs proportionally more calls across its
    -- edges to join a batch (@UNISON_JIT_SIZE_UNIT@). See JIT.formBatch.
    sizeUnit :: Int,
    -- | the LLVM pass pipeline a module is optimized with
    -- (@UNISON_JIT_PASSES@; the empty string runs none). The default is
    -- 'defaultPasses', the parts of @default<O2>@ that matter for our
    -- code; @default<O2>@ itself and @UNISON_JIT_CODEGEN_LEVEL@ (which
    -- jit_llvm.c reads itself) are there for comparison.
    passes :: String,
    -- | exits and unwinds write the frame back through one call to the C
    -- routine @unison_jit_exit_frame@ per site, the live slots as its
    -- arguments, rather than through generated write-back blocks
    -- (@UNISON_JIT_EXITS=call@, the default, or @blocks@). See
    -- Codegen.joinShared.
    exitCall :: Bool,
    -- | with 'On': a compiled callee of a definition being compiled is
    -- compiled again as a private copy in the new module, so that calls to
    -- it are direct and LLVM can inline it, when the callee's estimated
    -- work per call (JIT.Estimate's saving) is at most this
    -- (@UNISON_JIT_COPY@; 0 turns copies off)
    copyBound :: Double,
    -- | with 'On': milliseconds the compile thread waits for more re-entry
    -- function requests before compiling the ones it holds as one module
    -- (@UNISON_JIT_REENTRY_WAIT@; 0 compiles each at once)
    reentryWait :: Int,
    -- | what leaving native code and coming back costs, in units of the
    -- interpreter's overhead for one simple instruction: a function is
    -- left interpreted when its certain exits times this exceed what
    -- compiling it saves (@UNISON_JIT_EXIT_COST@; 0 compiles everything).
    -- See JIT.Estimate.
    exitCost :: Double,
    -- | what entering native code from the interpreter costs, in the same
    -- units: a compiled function that saves less than this per call is
    -- still interpreted when the interpreter calls it
    -- (@UNISON_JIT_ENTRY_COST@; 0 always enters)
    entryCost :: Double,
    -- | log each compiled module to stderr
    logging :: Bool,
    -- | directory to write each module's IR into, with the MCode of each
    -- function as a comment; @-@ prints it to stderr instead
    dumpIR :: Maybe FilePath,
    -- | print the MCode of every loaded definition to stderr
    dumpMCode :: Bool,
    -- | log every transition between native code and the interpreter
    trace :: Bool,
    -- | print exit counts after each evaluation (@UNISON_JIT_STATS@)
    stats :: Bool,
    -- | with stats: print the exits taken since the last time whenever
    -- the program writes output, and start counting again
    -- (@UNISON_JIT_STATS=each@), so that a program that prints a line per
    -- phase gets its exits listed per phase
    statsEach :: Bool,
    -- | with stats: also print the counts every this many exits (for
    -- evaluations that never finish)
    statsEvery :: Int,
    -- | fire the entry poll every N entries (stress mode @poll=N@)
    stressPoll :: Int,
    -- | initial Unison stack size in slots (stress mode @ustack=N@)
    stressStack :: Maybe Int,
    -- | treat every Nth callee as not compiled (stress mode @callee=N@)
    stressCallee :: Int,
    -- | C stack budget for native calls, in bytes (stress mode @cstack=N@)
    stressCStack :: Int,
    -- | allocation budget between polls, in words (stress mode @alloc=N@)
    stressAlloc :: Int,
    -- | milliseconds to sleep between installing each function of a
    -- module (stress mode @install=N@)
    stressInstall :: Int,
    -- | size of the constant pool before it has to grow (stress mode @pool=N@)
    stressPool :: Maybe Int,
    -- | random operations to run through the list helpers at startup,
    -- each checked against the interpreter's (stress mode @lists=N@)
    stressLists :: Int,
    -- | the same for the text helpers (stress mode @texts=N@)
    stressTexts :: Int,
    -- | and for the bytes helpers (stress mode @bytes=N@)
    stressBytes :: Int,
    -- | features turned off for debugging, from @UNISON_JIT_DISABLE@
    -- (comma separated): @app@ (closure calls), @apply@ (the interpreter
    -- entering native code for a closure), @ref@, @array@, @cmp@
    -- (universal comparison), @list@, @text@, @bytes@ (the C helpers for
    -- those), @callout@ (call-outs become resumes), @direct@ (calls within a
    -- module go through cells), @worker@ (no workers with register arguments),
    -- @copy@ (no private copies of compiled callees), @hash@ (the murmur hash),
    -- @irhelpers@ (the helpers compiled from IR: generated code calls the C
    -- copies instead), @requests@ (ability requests and the affine handler
    -- instructions: every request exits)
    disabled :: [String]
  }
  deriving (Show)

config :: Config
config = unsafePerformIO $ do
  mode <- lookupEnv "UNISON_JIT"
  logging <- lookupEnv "UNISON_JIT_LOG"
  dump <- lookupEnv "UNISON_JIT_DUMP_IR"
  stats <- lookupEnv "UNISON_JIT_STATS"
  every <- lookupEnv "UNISON_JIT_STATS_EVERY"
  thresh <- lookupEnv "UNISON_JIT_THRESHOLD"
  batchSize <- lookupEnv "UNISON_JIT_BATCH"
  gate <- lookupEnv "UNISON_JIT_BATCH_GATE"
  sizeU <- lookupEnv "UNISON_JIT_SIZE_UNIT"
  pipeline <- lookupEnv "UNISON_JIT_PASSES"
  exits <- lookupEnv "UNISON_JIT_EXITS"
  copyB <- lookupEnv "UNISON_JIT_COPY"
  reWait <- lookupEnv "UNISON_JIT_REENTRY_WAIT"
  cost <- lookupEnv "UNISON_JIT_EXIT_COST"
  entry <- lookupEnv "UNISON_JIT_ENTRY_COST"
  mcode <- lookupEnv "UNISON_JIT_DUMP_MCODE"
  tr <- lookupEnv "UNISON_JIT_TRACE"
  stress <- maybe [] (splitOn ',') <$> lookupEnv "UNISON_JIT_STRESS"
  disabledFeatures <- maybe [] (splitOn ',') <$> lookupEnv "UNISON_JIT_DISABLE"
  let setting key = listToMaybe' (mapMaybe (stripPrefix (key ++ "=")) stress) >>= readMaybe
  pure
    Config
      { mode = case map toLower (fromMaybe "off" mode) of
          "eager" -> Eager
          "on" -> On
          _ -> Off,
        threshold = max 1 (fromMaybe 100 (thresh >>= readMaybe)),
        batch = max 1 (fromMaybe 32 (batchSize >>= readMaybe)),
        batchGate = max 1 (fromMaybe (max 1 (fromMaybe 100 (thresh >>= readMaybe)) `div` 4) (gate >>= readMaybe)),
        sizeUnit = max 1 (fromMaybe 500 (sizeU >>= readMaybe)),
        passes = fromMaybe defaultPasses pipeline,
        exitCall = fmap (map toLower) exits /= Just "blocks",
        copyBound = max 0 (fromMaybe 40 (copyB >>= readMaybe)),
        reentryWait = max 0 (fromMaybe 20 (reWait >>= readMaybe)),
        exitCost = max 0 (fromMaybe 7 (cost >>= readMaybe)),
        entryCost = max 0 (fromMaybe 3 (entry >>= readMaybe)),
        logging = maybe False (not . null) logging,
        dumpIR = dump,
        dumpMCode = maybe False (not . null) mcode,
        trace = maybe False (not . null) tr,
        stats = maybe False (not . null) stats,
        statsEach = stats == Just "each",
        stressPoll = fromMaybe 0 (setting "poll"),
        stressStack = setting "ustack",
        stressCallee = fromMaybe 0 (setting "callee"),
        stressCStack = fromMaybe 0 (setting "cstack"),
        stressAlloc = fromMaybe 0 (setting "alloc"),
        stressInstall = fromMaybe 0 (setting "install"),
        stressPool = setting "pool",
        stressLists = fromMaybe 0 (setting "lists"),
        stressTexts = fromMaybe 0 (setting "texts"),
        stressBytes = fromMaybe 0 (setting "bytes"),
        statsEvery = fromMaybe 0 (every >>= readMaybe),
        disabled = disabledFeatures
      }
  where
    splitOn c s = case break (== c) s of
      (a, []) -> [a]
      (a, _ : rest) -> a : splitOn c rest
    listToMaybe' [] = Nothing
    listToMaybe' (x : _) = Just x
{-# NOINLINE config #-}

jitLog :: String -> IO ()
jitLog msg
  | logging config = hPutStrLn stderr ("[jit] " ++ msg)
  | otherwise = pure ()

-- | Writes diagnostic text to stderr, unconditionally.
jitDump :: String -> IO ()
jitDump msg = hPutStrLn stderr msg >> hFlush stderr

-- | What 'noteOutput' runs; set by the JIT when it starts.
outputHook :: IORef (IO ())
outputHook = unsafePerformIO (newIORef (pure ()))
{-# NOINLINE outputHook #-}

-- | Called by the runtime after the program has written to a handle.
noteOutput :: IO ()
noteOutput = when (statsEach config) (join (readIORef outputHook))

-- | The pass pipeline, in LLVM's syntax. @default<O2>@ with the parts our
-- code can't use left out: it is the same shape (function simplification,
-- then the inliner over the call graph with the simplification again
-- inside it, then a clean-up), and on the suite the code it produces runs
-- the same, at 60% of @O2@'s pass time (2026-10-09, see
-- benchmarks/2026-10-09-pipeline.md for what the lighter pipelines on the
-- way lost: without @ipsccp@ and @function-attrs@ the inlined callees'
-- attributes, without @jump-threading@ and @loop-rotate@ the status
-- switch after an inlined call and the loop shape, without @loop-unroll@
-- the peeled first iteration of a loop that calls a closure).
defaultPasses :: String
defaultPasses =
  "function(mem2reg,sroa,early-cse,simplifycfg,instcombine),ipsccp,"
    ++ "cgscc(inline,function-attrs,function(sroa,early-cse<memssa>,simplifycfg,instcombine,"
    ++ "jump-threading,correlated-propagation,loop-mssa(loop-rotate,licm,simple-loop-unswitch),"
    ++ "simplifycfg,instcombine,gvn,sccp,dse,loop-unroll,instcombine,adce,simplifycfg)),"
    ++ "function(simplifycfg),globaldce"
