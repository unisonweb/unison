-- | The JIT's entry points for the rest of the runtime, and the handle
-- that holds its state. See unison-runtime/src/Unison/Runtime/JIT/design.md.
module Unison.Runtime.JIT
  ( JIT,
    newJIT,
    readConfig,
    jitConfig,
    jitExits,
    jitFrames,
    jitPool,
    jitTracing,
    jitStats,
    initialNativeCount,
    stackSlots,
    loadJIT,
    startJIT,
    jitCompileGroup,
    jitRequestGroup,
    jitRequestCell,
    noteAdHocExit,
    registerDataTypes,
    registerTypeNumbers,
    printJITStats,
    noteOutput,
  )
where

import Control.Concurrent (forkOn, getNumCapabilities, myThreadId, threadCapability, yield)
import Control.Concurrent.STM (TBQueue, TVar, atomically, isFullTBQueue, newTBQueueIO, readTBQueue, readTVarIO, writeTBQueue)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (foldM, forM, forM_, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE, withExceptT)
import Data.IORef
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Word (Word64)
import Foreign.Ptr (FunPtr, Ptr, castFunPtrToPtr, nullPtr)
import GHC.Clock (getMonotonicTimeNSec)
import System.IO (hFlush, stdout)
import System.Info (os)
import System.Timeout (timeout)
import Unison.Reference (Reference)
import Unison.Runtime.Foreign.Function.Type (ForeignFunc (..))
import Unison.Runtime.JIT.Codegen (CtxOffsets (..), RtsFacts (..))
import Unison.Runtime.JIT.Compile
import Unison.Runtime.JIT.Config
import Unison.Runtime.JIT.Estimate (Estimate (..), judge)
import Unison.Runtime.JIT.Exits
import Unison.Runtime.JIT.Frames (Frames)
import Unison.Runtime.JIT.Helpers (exitIR, helpersIR)
import Unison.Runtime.JIT.LLVM
import Unison.Runtime.JIT.Layout (probeArrays, probeBytes, probeCompare, probeEnv, probeLayouts, probeLists, probeMurmur, probeNames, probeRecords, probeTexts)
import Unison.Runtime.JIT.Native (configureNative, ctxLayout, helperAddresses, murmurCAddress, rtsFacts, tableAddresses, ucmpCAddress)
import Unison.Runtime.JIT.Pool (Pool)
import Unison.Runtime.MCode (CombIx (..), GComb (..), GCombInfo (..), NativeCell, bumpNativeCount, claimNativeCell, combDeps, nativeCellRequested, noNativeCell, prettyIns, prettySection, readNativeCode, readNativeCount, releaseNativeCell, takeNativeCell, writeNativeCount)
import Unison.Runtime.Stack (MCombs, defaultStackSlots)
import Unison.Util.EnumContainers qualified as EC

-- | The JIT: its settings and everything it keeps while the process runs.
-- There is one per process, made by ucm at startup ('newJIT') and given to
-- every runtime it starts, since what the JIT compiles is shared: generated
-- code refers to the exits and frames tables and the constant pool by
-- index, and native code in one module calls into another directly. Each
-- code cache carries the handle (CCache's @jit@), which is how the
-- interpreter reaches it.
data JIT = JIT
  { jitCompiler :: !Compiler,
    -- | the constructor arities of every data type loaded so far, which
    -- the code generator needs to take apart constructors with three or
    -- more fields (their field count isn't in the closure's pointer tag)
    jitDataTypes :: !(IORef (Map.Map Reference [Int])),
    -- | the numbers the code cache gives types (abilities among them),
    -- with their references: a request match names its ability by number,
    -- so the generator needs the arities by number too ('abilityArities')
    jitTypeNumbers :: !(IORef (EC.EnumMap Word64 Reference)),
    -- | set once by 'initJIT' when everything checks out; Nothing means off
    jitState :: !(IORef (Maybe JITState)),
    -- | set once a load has been tried, so that it is tried once only: a
    -- failure would be the same the second time, and would print again
    jitInitTried :: !(IORef (Maybe (Either [String] String))),
    -- | what the interpreter asks the compile thread for; bounded, so a
    -- burst of requests costs nothing but the requests: one that doesn't
    -- fit is dropped, and made again if the code stays hot
    jitRequests :: !(TBQueue Request),
    jitCompileThreadStarted :: !(IORef Bool),
    -- | the re-entry functions each group may still get because of ad
    -- hoc exits, and what those exits asked for (see 'noteAdHocExit')
    jitAdHoc :: !(IORef AdHoc)
  }

data AdHoc = AdHoc
  { -- | by group number; absent means the full allowance
    ahBudgets :: !(EC.EnumMap Word64 Int),
    -- | re-entry functions asked for because of ad hoc exits
    ahRequested :: !Int,
    -- | ad hoc exits through an uncompiled Let of a group at its cap
    ahRefused :: !Int
  }

-- | A JIT with these settings, not started: nothing is loaded or compiled
-- until 'loadJIT' or 'startJIT'. In the off mode it is inert, and the
-- runtime's calls into it return at once.
newJIT :: Config -> IO JIT
newJIT config =
  JIT
    <$> newCompiler config
    <*> newIORef Map.empty
    <*> newIORef (EC.mapFromList [])
    <*> newIORef Nothing
    <*> newIORef Nothing
    <*> newTBQueueIO 256
    <*> newIORef False
    <*> newIORef (AdHoc (EC.mapFromList []) 0 0)

jitConfig :: JIT -> Config
jitConfig = cConfig . jitCompiler

jitExits :: JIT -> Exits
jitExits = cExits . jitCompiler

jitFrames :: JIT -> Frames
jitFrames = cFrames . jitCompiler

jitPool :: JIT -> Pool
jitPool = cPool . jitCompiler

-- | Whether every transition between native code and the interpreter is
-- logged (@UNISON_JIT_TRACE@).
jitTracing :: JIT -> Bool
jitTracing = trace . jitConfig

-- | Whether entries and exits are counted (@UNISON_JIT_STATS@).
jitStats :: JIT -> Bool
jitStats = stats . jitConfig

-- | The count a combinator's cell starts with: minus the threshold in the
-- @on@ mode, zero (never hot) otherwise.
initialNativeCount :: JIT -> Int
initialNativeCount jit = case mode (jitConfig jit) of
  On -> negate (threshold (jitConfig jit))
  _ -> 0

-- | The slots a fresh Unison stack has: the stress mode @ustack=N@ makes
-- it tiny, so that growing the stack is exercised constantly.
stackSlots :: JIT -> Int
stackSlots = fromMaybe defaultStackSlots . stressStack . jitConfig

-- | Records the constructor arities of some data types. Called by the
-- interface whenever it learns about types, before their code is loaded.
registerDataTypes :: JIT -> Map.Map Reference [Int] -> IO ()
registerDataTypes jit m = case mode (jitConfig jit) of
  Off -> pure ()
  _ -> atomicModifyIORef' (jitDataTypes jit) (\old -> (Map.union m old, ()))

-- | Records the type numbering of the code cache. Called when types are
-- numbered: for the builtins at startup, and whenever code is loaded.
registerTypeNumbers :: JIT -> EC.EnumMap Word64 Reference -> IO ()
registerTypeNumbers jit m = case mode (jitConfig jit) of
  Off -> pure ()
  _ -> atomicModifyIORef' (jitTypeNumbers jit) (\old -> (m <> old, ()))

-- | The constructor arities of the types known so far, by type number.
abilityArities :: JIT -> Map.Map Reference [Int] -> IO (EC.EnumMap Word64 [Int])
abilityArities jit types = do
  nums <- readIORef (jitTypeNumbers jit)
  pure (EC.mapFromList [(n, as) | (n, r) <- EC.mapToList nums, Just as <- [Map.lookup r types]])

-- | Gets the JIT going if it is enabled. Called by each runtime as it
-- starts. LLVM is normally loaded already, by 'loadJIT' at ucm's startup;
-- if not (another host of the runtime), eager mode loads it here and
-- @on@ mode on the compile thread, when the first request arrives.
startJIT :: JIT -> IO ()
startJIT jit = case mode (jitConfig jit) of
  Off -> pure ()
  Eager -> void (initJIT jit)
  On -> do
    first <- atomicModifyIORef' (jitCompileThreadStarted jit) (\started -> (True, not started))
    -- On a capability other than this thread's, so that the compile
    -- thread and the interpreter don't slow each other down.
    when first $ do
      (cap, _) <- threadCapability =<< myThreadId
      n <- getNumCapabilities
      void (forkOn ((cap + 1) `mod` n) (compileThread jit))

-- | Loads LLVM at startup, when the JIT is turned on, and says how it
-- went: Nothing when the JIT is off, otherwise whether it is activated,
-- and if not, lines explaining why. Whatever the answer, the program runs:
-- without the JIT everything is interpreted.
loadJIT :: JIT -> IO (Maybe (Either [String] String))
loadJIT jit = case mode (jitConfig jit) of
  Off -> pure Nothing
  _ -> Just <$> initJIT jit

-- | Loads LLVM, starts the JIT and probes the runtime, the first time it
-- is called; later calls return the first outcome. On success 'jitState'
-- is set and the result names the LLVM loaded; if anything is wrong the
-- JIT stays off and the result says why, in lines meant for the user.
initJIT :: JIT -> IO (Either [String] String)
initJIT jit =
  readIORef (jitInitTried jit) >>= \case
    Just r -> pure r
    Nothing -> do
      r <- initJIT' jit
      writeIORef (jitInitTried jit) (Just r)
      either (mapM_ (jitLog config)) (jitLog config) r
      pure r
  where
    config = jitConfig jit

initJIT' :: JIT -> IO (Either [String] String)
initJIT' jit = runExceptT $ do
  (version, path) <-
    withExceptT
      ( \case
          NotFound tried -> ("JIT compilation needs LLVM version >= " ++ show minLLVM ++ ", searched these locations:") : map ("  " ++) tried
          InitError e -> [e ++ "; the JIT is off"]
      )
      (ExceptT initLLVM)
  -- The closure layouts, and every helper checked against the interpreter:
  -- the list helpers are part of the generated code's contract too, and
  -- bytes go before texts because the text checks build bytes (toUtf8).
  ls <- off $ do
    ls <- ExceptT probeLayouts
    timed "list helper checks" (ExceptT (probeLists ls (stressLists config)))
    timed "bytes helper checks" (ExceptT (probeBytes ls (stressBytes config)))
    timed "text helper checks" (ExceptT (probeTexts ls (stressTexts config)))
    ExceptT (probeArrays ls)
    ExceptT (probeMurmur ls)
    ExceptT (probeCompare ls)
    ExceptT probeEnv
    ExceptT (probeRecords ls)
    ExceptT (probeNames ls)
    pure ls
  ctx <-
    liftIO ctxLayout >>= \case
      ([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w], _) -> pure (CtxOffsets a b c d e f g h i j k l m n o p q r s t u v w)
      _ -> throwE ["unexpected Ctx layout; the JIT is off"]
  liftIO (configureNative (stressPoll config) (stressCallee config) (stressCStack config) (stressAlloc config) (trace config))
  rts <-
    liftIO rtsFacts >>= \case
      -- the allocator's address isn't needed: generated code calls
      -- unison_jit_alloc_words by name, and the JIT resolves it from the process
      [arrWordsInfo, arrPtrsInfo, bytesHdr, bytesCount, ptrsHdr, ptrsCount, ptrsSize, cardBits, _allocator, mutVarVar, _barrier, arrPtrsDirtyInfo] ->
        pure (RtsFacts arrWordsInfo arrPtrsInfo bytesHdr bytesCount ptrsHdr ptrsCount ptrsSize cardBits mutVarVar arrPtrsDirtyInfo)
      _ -> throwE ["unexpected runtime facts; the JIT is off"]
  -- The C helpers' addresses go in first, as absolute symbols, so that
  -- nothing is looked up in the process's own symbol table (Linux doesn't
  -- expose an executable's symbols to dlsym without -rdynamic, Windows not
  -- at all). The names the IR modules define themselves are left to them:
  -- the exit routine (cbits/jit_exit.ll), which goes in whatever the
  -- switches say since every exit calls it, and the helpers written as IR
  -- (cbits/jit_helpers.ll), whose definitions shadow the C copies for
  -- generated code; with irhelpers disabled the C copies are defined
  -- instead.
  -- (On Windows the C copies can't stand in for the IR helpers: they
  -- return their (ok, value) pair through a hidden pointer under the Win64
  -- convention, where generated code expects the pair in registers, so the
  -- irhelpers switch is ignored.)
  let irDefines ir = [T.unpack (T.takeWhile (/= '(') (T.drop 1 (T.dropWhile (/= '@') rest))) | l <- T.lines ir, Just rest <- [T.stripPrefix "define " l]]
      noIRHelpers = "irhelpers" `elem` disabled config && os /= "mingw32"
      irDefined = irDefines exitIR ++ (if noIRHelpers then [] else irDefines helpersIR)
  when ("irhelpers" `elem` disabled config && not noIRHelpers) $
    liftIO (jitLog config "UNISON_JIT_DISABLE=irhelpers is ignored on Windows: the C copies return the (ok, value) pair differently from generated code there")
  cHelpers <- liftIO helperAddresses
  withExceptT (\e -> ["the C helpers could not be defined: " ++ e ++ "; the JIT is off"]) $
    forM_ [h | h@(n, _) <- cHelpers, n `notElem` irDefined] (ExceptT . uncurry defineSymbol)
  withExceptT (\e -> ["the exit routine (cbits/jit_exit.ll) " ++ e ++ "; the JIT is off"]) $ do
    withExceptT ("could not be added: " ++) (void (timed "exit routine" (ExceptT (addModule False (passes config) exitIR))))
    withExceptT ("not found after adding: " ++) (void (ExceptT (lookupSymbol "unison_jit_exit_frame") :: ExceptT String IO (FunPtr ())))
  unless noIRHelpers $
    withExceptT (\e -> ["the IR helpers (cbits/jit_helpers.ll) " ++ e ++ "; the JIT is off"]) $ do
      withExceptT ("could not be added: " ++) (timed "IR helpers" (ExceptT (addHelpers config)))
      -- the module's definition must be the one generated code gets, not
      -- the C copy the process also has
      (p, q) <- withExceptT ("not found after adding: " ++) (timed "IR helpers compiled" ((,) <$> ExceptT (lookupSymbol "unison_jit_murmur") <*> ExceptT (lookupSymbol "unison_jit_ucmp")))
      when (castFunPtrToPtr p == castFunPtrToPtr murmurCAddress || castFunPtrToPtr q == castFunPtrToPtr ucmpCAddress) $
        throwE "did not shadow the C copies"
  liftIO (writeIORef (jitState jit) (Just (JITState ls ctx rts)))
  triple <- liftIO targetTriple
  pure ("mode " ++ show (mode config) ++ ", LLVM " ++ version ++ " from " ++ path ++ ", target " ++ triple)
  where
    config = jitConfig jit
    off = withExceptT (\e -> [e ++ "; the JIT is off"])
    timed :: String -> ExceptT e IO a -> ExceptT e IO a
    timed what act = do
      t0 <- liftIO getMonotonicTimeNSec
      x <- act
      t1 <- liftIO getMonotonicTimeNSec
      x <$ liftIO (jitLog config (what ++ ": " ++ show (fromIntegral (t1 - t0) / 1e6 :: Double) ++ " ms"))

-- | Adds the IR helpers' module, after defining the names of the runtime's
-- tables it refers to ("The IR helpers" in design.md).
addHelpers :: Config -> IO (Either String ())
addHelpers config = do
  tables <- tableAddresses
  defined <- sequence_ <$> traverse (uncurry defineSymbol) tables
  case defined of
    Left e -> pure (Left e)
    Right () -> void <$> addModule False (passes config) helpersIR

-- | The oldest LLVM the shim works with (why: cbits/jit_llvm.c).
minLLVM :: Int
minLLVM = 20

-- | Compiles a freshly loaded top-level definition, in eager mode.
jitCompileGroup :: JIT -> Bool -> Reference -> Word64 -> MCombs -> IO ()
jitCompileGroup jit sandbox ref grp cmbs = case mode (jitConfig jit) of
  Eager ->
    readIORef (jitState jit) >>= \case
      Nothing -> pure ()
      Just st -> do
        types <- readIORef (jitDataTypes jit)
        abilities <- abilityArities jit types
        (now, _) <- groupUnits (jitCompiler jit) sandbox False ref grp cmbs
        compileUnits (jitCompiler jit) st types abilities ("unison_" ++ show grp) False now
  _ -> pure ()

-- ---------------------------------------------------------------------------
-- The @on@ mode: compiling what gets hot, on a thread of its own.
-- See design.md, "What gets compiled, and when".

-- | What the interpreter asks the compile thread for.
data Request
  = -- | compile this definition: its group number, and how to read the
    -- code cache (every group's combinators, and their references)
    ReqGroup !Word64 !Bool !Word64 (IO (EC.EnumMap Word64 MCombs, EC.EnumMap Word64 Reference))
  | -- | generate a re-entry function that was left for later
    ReqUnit Unit

-- | Queues a request if there is room; says whether there was.
offer :: JIT -> Request -> IO Bool
offer jit r = atomically $ do
  full <- isFullTBQueue (jitRequests jit)
  unless full (writeTBQueue (jitRequests jit) r)
  pure (not full)

-- | The cell of a definition's entry combinator. Its state stands for the
-- whole definition: requested from the moment a request for the
-- definition is queued, taken once the compile thread has it in a batch.
-- It never goes back, so a definition is queued at most once.
entryCell :: MCombs -> Maybe (Ptr NativeCell)
entryCell cmbs = case EC.lookup 0 cmbs of
  Just (Comb (LamI _ _ _ cell)) | cell /= noNativeCell -> Just cell
  _ -> Nothing

-- | Asks for the definition this combinator belongs to to be compiled.
-- Called by the interpreter when the combinator's call count says it is
-- hot (see 'bumpNativeCount'). If the queue is full the flag is cleared
-- again and the count set to ask again after another 1024 calls.
-- The cache's combinators and their references are given as the TVars,
-- read again on the compile thread when the request is served.
jitRequestGroup :: JIT -> Bool -> TVar (EC.EnumMap Word64 MCombs) -> TVar (EC.EnumMap Word64 Reference) -> CombIx -> Ptr NativeCell -> IO ()
jitRequestGroup jit sandboxed combs combRefs (CIx _ grp _) cell
  | cell == noNativeCell = pure () -- a builtin, or code not loaded through the cache
  | otherwise = do
      cache <- readTVarIO combs
      forM_ (EC.lookup grp cache >>= entryCell) $ \entry -> do
        mine <- claimNativeCell entry
        when mine $ do
          t0 <- getMonotonicTimeNSec
          queued <- offer jit (ReqGroup grp sandboxed t0 ((,) <$> readTVarIO combs <*> readTVarIO combRefs))
          if queued
            then
              -- Entering the scheduler is what hands a runnable thread to an
              -- idle capability: without this the compile thread, if it is
              -- on this capability, starts at this capability's next GC or
              -- context-switch tick, a thousand interpreted calls later on
              -- an optimized build (UNISON_JIT_LOG reports the lag).
              yield
            else do
              releaseNativeCell entry
              writeNativeCount cell (-1024)
{-# NOINLINE jitRequestGroup #-}

-- | Asks for the re-entry function that belongs in this cell to be
-- generated, if there is one waiting (see 'Compile.pending'). Called by
-- the interpreter when it has found the cell empty often enough. The
-- cell's request state keeps it from being queued twice.
jitRequestCell :: JIT -> Ptr NativeCell -> IO ()
jitRequestCell jit cell =
  lookupPending (jitCompiler jit) cell >>= \case
    Nothing -> pure ()
    Just u -> do
      mine <- claimNativeCell cell
      when mine $ do
        queued <- offer jit (ReqUnit u)
        -- addPending starts the count again
        unless queued (releaseNativeCell cell >> addPending (jitCompiler jit) u)
{-# NOINLINE jitRequestCell #-}

-- | The trampoline's report of an ad hoc exit (the poll, the allocation
-- budget, stack growth: 'isAdHoc') that unwound through a Let whose body
-- has no code yet. The exits the code itself causes count toward the
-- body's re-entry function when its frame is popped (Machine.countReentry)
-- and are bounded by the code; these land wherever the program is, so
-- a long-running function would in time be interrupted at every Let it
-- has and get a copy of its suffix after each. So they count here, and
-- only while the group's budget lasts ('reentryAdHoc', 3): the Lets most
-- often on the stack at a tick, where the time goes, are the ones that
-- get in. The frame the trampoline builds for such an exit carries no
-- cell, so the pop interprets the body without counting it again.
noteAdHocExit :: JIT -> Word64 -> Ptr NativeCell -> IO ()
noteAdHocExit jit grp cell =
  lookupPending (jitCompiler jit) cell >>= \case
    Nothing -> pure ()
    Just _ -> do
      let allowance = reentryAdHoc (jitConfig jit)
          left a = EC.lookupWithDefault allowance grp (ahBudgets a)
      allowed <- atomicModifyIORef' (jitAdHoc jit) $ \a ->
        if left a > 0 then (a, True) else (a {ahRefused = ahRefused a + 1}, False)
      when allowed $ do
        hot <- bumpNativeCount cell
        when hot $ do
          atomicModifyIORef' (jitAdHoc jit) $ \a ->
            (a {ahBudgets = EC.mapInsert grp (left a - 1) (ahBudgets a), ahRequested = ahRequested a + 1}, ())
          jitRequestCell jit cell

-- | Which definitions call which: for each group, the groups whose code
-- refers to it. The code cache only records the other direction, so the
-- compile thread keeps this, adding the groups loaded since it last looked.
data Callers = Callers !(Set.Set Word64) !(Map.Map Word64 (Set.Set Word64))

indexCallers :: EC.EnumMap Word64 MCombs -> Callers -> Callers
indexCallers cache (Callers seen callers) =
  Callers
    (Set.union seen (Set.fromList (map fst new)))
    (Map.unionWith Set.union callers (Map.fromListWith Set.union [(d, Set.singleton g) | (g, cmbs) <- new, d <- groupDeps g cmbs]))
  where
    new = [gc | gc@(g, _) <- EC.mapToList cache, not (Set.member g seen)]

-- | The other groups a group's code refers to.
groupDeps :: Word64 -> MCombs -> [Word64]
groupDeps g cmbs = Set.toList (Set.delete g (Set.fromList [d | (_, c) <- EC.mapToList cmbs, d <- combDeps c]))

-- | Which side of an edge a batch member is on.
data Role = Callee | Caller

-- | The estimated calls from a group's code to group @d@: for each of its
-- combinators, the combinator's own call count times the static call
-- sites of @d@ in it.
edgeCalls :: Config -> MCombs -> Word64 -> IO Double
edgeCalls config cmbs d = fmap sum . forM (EC.mapToList cmbs) $ \(_, c) -> case c of
  Comb (LamI _ _ _ cell)
    | cell /= noNativeCell,
      sites <- length (filter (== d) (combDeps c)),
      sites > 0 -> do
        -- counts start at minus the threshold
        n <- readNativeCount cell
        pure (fromIntegral (max 0 (n + threshold config)) * fromIntegral sites)
  _ -> pure 0

-- | A group's size, as a proxy for the code generated for it: for each
-- combinator, its frame size times its call sites. On the suite the IR
-- grows with both (the slots live across a call are written to the stack
-- around it): the suite's own body, 183 lines of MCode with a frame of 128
-- slots and 56 sites, was 55k lines of IR, and a typical hot function
-- under 500 (2026-10-09).
groupSize :: MCombs -> Int
groupSize cmbs = sum [f * length (combDeps c) | (_, c@(Comb (LamI _ f _ _))) <- EC.mapToList cmbs]

-- | Serves requests, forever. LLVM is only ever used from this thread (in
-- @on@ mode); it is normally loaded at startup already, and if not, by the
-- first request.
--
-- A definition that got hot is compiled as soon as its request comes up.
-- Requests for re-entry functions are held instead, and compiled together
-- as one module once there are a batch of them or no request has come for
-- 'reentryWait' milliseconds: each would otherwise be a module of its own,
-- paying LLVM's per-module cost for a few dozen instructions, and nothing
-- is lost by the wait, since a re-entry point that doesn't exist yet is
-- resumed by the interpreter as it was before the counter said to generate
-- it. A definition's request is served ahead of the held re-entry
-- functions, so the wait never delays a hot loop.
compileThread :: JIT -> IO ()
compileThread jit = do
  initialized <- newIORef False
  callersRef <- newIORef (Callers Set.empty Map.empty)
  let loop held = do
        next <-
          if null held
            then Just <$> atomically (readTBQueue (jitRequests jit))
            else timeout (reentryWait config * 1000) (atomically (readTBQueue (jitRequests jit)))
        case next of
          Nothing -> flush held >> loop []
          Just (ReqUnit u) | reentryWait config > 0 -> do
            let held' = u : held
            if length held' >= batch config then flush held' >> loop [] else loop held'
          Just req -> do
            r <- try (serve initialized callersRef req)
            case r of
              Left (e :: SomeException) -> jitLog config ("compile thread: " ++ show e)
              Right () -> pure ()
            loop held
      flush [] = pure ()
      flush held = do
        r <-
          try
            ( withState initialized $ \st types abilities -> do
                let name = "unison_" ++ unitName (last held) ++ "_x" ++ show (length held)
                guarded name (compileUnits c st types abilities name True (reverse held))
                forM_ held (dropPending c)
            )
        case r of
          Left (e :: SomeException) -> jitLog config ("compile thread: " ++ show e)
          Right () -> pure ()
  loop []
  where
    config = jitConfig jit
    c = jitCompiler jit
    withState initialized k = do
      ready <- readIORef initialized
      unless ready (void (initJIT jit) >> writeIORef initialized True)
      readIORef (jitState jit) >>= \case
        Nothing -> pure ()
        Just st -> do
          types <- readIORef (jitDataTypes jit)
          abilities <- abilityArities jit types
          k st types abilities
    serve initialized callersRef req =
      withState initialized $ \st types abilities -> do
        case req of
          ReqGroup grp sandbox t0 find -> do
            when (logging config) $ do
              t1 <- getMonotonicTimeNSec
              jitLog config ("request for " ++ show grp ++ " served after " ++ show ((t1 - t0) `div` 1000) ++ " us")
            (cache, refs) <- find
            -- a request whose definition went into an earlier batch is dropped
            mine <- maybe (pure False) takeNativeCell (EC.lookup grp cache >>= entryCell)
            when mine $ do
              modifyIORef' callersRef (indexCallers cache)
              Callers _ callers <- readIORef callersRef
              members <- formBatch cache callers grp
              (now, later) <-
                unzip
                  <$> sequence
                    [ groupUnits c sandbox True ref g cmbs
                    | g <- members,
                      Just cmbs <- [EC.lookup g cache],
                      Just ref <- [EC.lookup g refs]
                    ]
              -- pending before the groups' code can exit to them
              forM_ (concat later) (addPending c)
              copies <- copyCandidates sandbox cache refs members
              guarded ("unison_" ++ show grp) (compileUnits c st types abilities ("unison_" ++ show grp) True (concat now ++ copies))
          ReqUnit u -> do
            guarded (unitName u) (compileUnits c st types abilities ("unison_" ++ unitName u) True [u])
            dropPending c u
    -- The definitions to compile together with one that got hot, so that
    -- calls between them are direct and LLVM can inline across them. The
    -- batch grows from the hot definition one neighbour at a time (Prim's
    -- algorithm): the candidate with the most estimated calls across its
    -- edges to the batch so far joins next, once those calls reach the
    -- gate, up to the batch size. See design.md for more.
    formBatch cache callers grp = do
      seq0 <- newIORef (0 :: Int)
      let full taken = length taken >= batch config
          -- the neighbours of g, with the role g plays for each
          around g = map ((,) Callee) (maybe [] (groupDeps g) (EC.lookup g cache)) ++ map ((,) Caller) (Set.toList (Map.findWithDefault Set.empty g callers))
          -- the estimated calls across the edge between g and v
          weight role g v = case role of
            Callee -> maybe (pure 0) (\cmbs -> edgeCalls config cmbs v) (EC.lookup g cache)
            Caller -> maybe (pure 0) (\cmbs -> edgeCalls config cmbs g) (EC.lookup v cache)
          -- adds g's neighbours to the candidates, or more weight to ones already there
          expand g (seen, cands) = foldM add cands (around g)
            where
              add cs (role, v)
                | Set.member v seen = pure cs
                | otherwise = do
                    w <- weight role g v
                    n <- readIORef seq0
                    writeIORef seq0 (n + 1)
                    pure (Map.insertWith (\(w', _) (w0, s0) -> (w0 + w', s0)) v (w, n) cs)
          -- whether a candidate may join now: hot on its own, or enough
          -- estimated calls for its size (a large candidate costs more to
          -- compile, so cutting an edge into it has to save more calls)
          passes v (w, _) = case EC.lookup v cache of
            Just cmbs | Just cell <- entryCell cmbs -> do
              hot <- nativeCellRequested cell
              let scale = max 1 (fromIntegral (groupSize cmbs) / fromIntegral (sizeUnit config))
              pure (hot || w / scale >= fromIntegral (batchGate config))
            _ -> pure False
          -- the candidate with the most weight, then the earliest seen
          best =
            Map.foldlWithKey'
              ( \acc v (w, n) -> case acc of
                  Just (_, (w', n')) | (w', negate n') >= (w, negate n) -> acc
                  _ -> Just (v, (w, n))
              )
              Nothing
          go taken seen cands
            | full taken = pure (reverse taken, cands)
            | otherwise = do
                passing <- Map.traverseMaybeWithKey (\v c -> (\ok -> if ok then Just c else Nothing) <$> passes v c) cands
                case best passing of
                  Nothing -> pure (reverse taken, cands)
                  Just (v, (w, _)) -> do
                    claimed <- maybe (pure False) takeNativeCell (EC.lookup v cache >>= entryCell)
                    let seen' = Set.insert v seen
                        cands' = Map.delete v cands
                    if claimed
                      then do
                        jitLog config ("  " ++ show v ++ " joins the batch for " ++ show grp ++ ", weight " ++ show w)
                        expand v (seen', cands') >>= go (v : taken) seen'
                      else go taken seen' cands'
      cands0 <- expand grp (Set.singleton grp, Map.empty)
      (members, left) <- go [grp] (Set.singleton grp) cands0
      when (logging config && not (Map.null left)) $
        jitLog config ("  left out of the batch for " ++ show grp ++ ": " ++ unwords [show v ++ " (weight " ++ show w ++ ")" | (v, (w, _)) <- Map.toList left])
      pure members
    -- The compiled callees of the batch that are worth a private copy in
    -- its module (see 'copyUnits'): those where a call is a real share of
    -- the work per call, by the estimate's path saving, and that don't
    -- loop or recurse (a copy of those would stay as a second body and
    -- gain only the outer call).
    copyCandidates sandbox cache refs members
      | copyBound config <= 0 || "copy" `elem` disabled config = pure []
      | otherwise = do
          let inBatch = Set.fromList members
              callees = Set.toList (Set.fromList [d | g <- members, Just cmbs <- [EC.lookup g cache], d <- groupDeps g cmbs, not (Set.member d inBatch)])
          picked <- forM callees $ \d -> case (EC.lookup d cache, EC.lookup d refs) of
            (Just cmbs, Just ref)
              | Just cell <- entryCell cmbs,
                Just (Comb (LamI _ _ entry _)) <- EC.lookup 0 cmbs -> do
                  code <- readNativeCode cell
                  if code == nullPtr
                    then pure []
                    else do
                      (_, est) <- judge config cell entry
                      case est of
                        Just (Estimate saving _ False) | saving <= copyBound config -> copyUnits c sandbox ref d cmbs
                        _ -> pure []
            _ -> pure []
          let units = take (batch config) (concat picked)
          unless (null units) (jitLog config ("private copies: " ++ unwords (map unitName units)))
          pure units
    guarded what act =
      try (act >>= evaluate) >>= \case
        Left (e :: SomeException) -> jitLog config (what ++ ": " ++ show e)
        Right () -> pure ()

-- | Prints how often each exit was taken, when UNISON_JIT_STATS is set.
-- Called after each evaluation.
printJITStats :: JIT -> IO ()
printJITStats jit = when (stats config) $ do
  t <- compileTotals (jitCompiler jit)
  jitDump
    ( "[jit] compiled "
        ++ show (ctModules t)
        ++ " modules: "
        ++ show (ctFunctions t)
        ++ " functions, "
        ++ show (ctAuxiliary t)
        ++ " auxiliary functions with them, "
        ++ show (ctOnDemand t)
        ++ " re-entry functions on demand ("
        ++ show (ctPending t)
        ++ " never asked for); "
        ++ show (ctNotWorthIt t)
        ++ " left interpreted by the exit rule; "
        ++ show (ctIRBytes t `div` 1024)
        ++ " KB of IR, "
        ++ show (fromIntegral (ctNanoseconds t) / 1e6 :: Double)
        ++ " ms"
        ++ " (generate "
        ++ show (fromIntegral (ctGenerate t) / 1e6 :: Double)
        ++ ", parse and optimize "
        ++ show (fromIntegral (ctOptimize t) / 1e6 :: Double)
        ++ ", code "
        ++ show (fromIntegral (ctCodegen t) / 1e6 :: Double)
        ++ ")"
    )
  a <- readIORef (jitAdHoc jit)
  jitDump
    ( "[jit] ad hoc exits (the poll, stack growth): "
        ++ show (ahRequested a)
        ++ " re-entry functions asked for, "
        ++ show (ahRefused a)
        ++ " exits through an uncompiled Let past the cap"
    )
  printExits jit maxBound
  when (statsEach config) (resetExitCounts (jitExits jit))
  where
    config = jitConfig jit

-- | Called by the interpreter after each foreign call. With
-- @UNISON_JIT_STATS=each@, a call that wrote output is followed by the
-- exits taken since the program last wrote (the busiest sites only), after
-- which counting starts again; so a program that prints a line per phase
-- gets its exits listed per phase.
noteOutput :: JIT -> ForeignFunc -> IO ()
noteOutput jit f =
  when (statsEach (jitConfig jit) && f == IO_putBytes_impl_v3) $ do
    hFlush stdout
    printExits jit 12
    resetExitCounts (jitExits jit)

printExits :: JIT -> Int -> IO ()
printExits jit most = do
  entered <- entryCounts (jitExits jit)
  let enteredTotal = sum (map snd entered)
  when (enteredTotal > 0) $ do
    jitDump ("[jit] " ++ show enteredTotal ++ " entries into native code from the trampoline (" ++ show (length entered) ++ " functions):")
    forM_ (take most (sortOn (negate . snd) entered)) $ \(name, n) ->
      jitDump ("  " ++ show n ++ "\t" ++ name)
  counts <- exitCounts (jitExits jit)
  let total = sum [n | (_, _, n) <- counts]
  when (total > 0) $ do
    jitDump ("[jit] " ++ show total ++ " exits taken (" ++ show (length counts) ++ " sites):")
    forM_ (take most (sortOn (\(_, _, n) -> negate n) counts)) $ \(i, e, n) ->
      jitDump ("  " ++ show n ++ "\t#" ++ show i ++ "\t" ++ describe e)
  where
    describe = \case
      Resume cix sect -> "resume " ++ show cix ++ " at " ++ takeWhile (/= '\n') (dropWhile (== ' ') (prettySection 0 sect ""))
      GrowStack n _ -> "grow stack by " ++ show n
      Reenter _ -> "reenter (poll)"
      CallOut cix instr _ _ _ -> "call out " ++ show cix ++ " for " ++ takeWhile (/= '\n') (prettyIns instr "")
      Named name e -> name ++ ": " ++ describe e
