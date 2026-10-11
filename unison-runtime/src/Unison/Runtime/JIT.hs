-- | The JIT's entry points for the rest of the runtime. See unison-runtime/src/Unison/Runtime/JIT/design.md.
module Unison.Runtime.JIT
  ( startJIT,
    loadJIT,
    jitCompileGroup,
    jitRequestGroup,
    jitRequestCell,
    registerDataTypes,
    printJITStats,
  )
where

import Control.Concurrent (forkOn, getNumCapabilities, myThreadId, threadCapability, yield)
import Control.Concurrent.STM (TBQueue, atomically, isFullTBQueue, newTBQueueIO, readTBQueue, readTVarIO, writeTBQueue)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (foldM, forM, forM_, unless, void, when)
import Data.IORef
import Data.Set qualified as Set
import Data.Functor ((<&>))
import Foreign.Ptr (Ptr, castFunPtrToPtr, nullPtr)
import System.Timeout (timeout)
import GHC.Clock (getMonotonicTimeNSec)
import Data.Map.Strict qualified as Map
import Data.List (sortOn)
import Data.Word (Word64)
import System.IO.Unsafe (unsafePerformIO)
import Unison.Reference (Reference)
import Unison.Runtime.JIT.Codegen (CtxOffsets (..), RtsFacts (..))
import Unison.Runtime.JIT.Compile
import Unison.Runtime.JIT.Config
import Unison.Runtime.JIT.Estimate (Estimate (..), judge)
import Unison.Runtime.JIT.Exits
import Unison.Runtime.JIT.Helpers (helpersIR)
import Unison.Runtime.JIT.LLVM
import Unison.Runtime.JIT.Layout (probeArrays, probeBytes, probeEnv, probeLayouts, probeLists, probeMurmur, probeNames, probeTexts)
import Unison.Runtime.JIT.Native (configureNative, ctxLayout, murmurCAddress, rtsFacts, tableAddresses)
import Unison.Runtime.MCode (CombIx (..), GComb (..), GCombInfo (..), NativeCell, claimNativeCell, combDeps, nativeCellRequested, noNativeCell, prettyIns, prettySection, readNativeCode, readNativeCount, releaseNativeCell, takeNativeCell, writeNativeCount)
import Unison.Runtime.Machine.Types (CCache (combRefs, combs, sandboxed), MCombs)
import Unison.Util.EnumContainers qualified as EC

-- | The constructor arities of every data type loaded so far, which the
-- code generator needs to take apart constructors with three or more
-- fields (their field count isn't in the closure's pointer tag).
dataTypes :: IORef (Map.Map Reference [Int])
dataTypes = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE dataTypes #-}

-- | Records the constructor arities of some data types. Called by the
-- interface whenever it learns about types, before their code is loaded.
registerDataTypes :: Map.Map Reference [Int] -> IO ()
registerDataTypes m = case mode config of
  Off -> pure ()
  _ -> atomicModifyIORef' dataTypes (\old -> (Map.union m old, ()))

-- | Set once by 'startJIT' when everything checks out. Nothing means off.
jitState :: IORef (Maybe JITState)
jitState = unsafePerformIO (newIORef Nothing)
{-# NOINLINE jitState #-}

-- | Gets the JIT going if it is enabled. Called once when the runtime
-- starts. LLVM is normally loaded already, by 'loadJIT' at ucm's startup;
-- if not (another host of the runtime), eager mode loads it here and
-- @on@ mode on the compile thread, when the first request arrives.
startJIT :: IO ()
startJIT = do
  when (statsEach config) (writeIORef outputHook printExitsSinceOutput)
  startCompiler

startCompiler :: IO ()
startCompiler = case mode config of
  Off -> pure ()
  Eager -> void initJIT
  On -> do
    first <- atomicModifyIORef' compileThreadStarted (\started -> (True, not started))
    -- On a capability other than this thread's, so that the compile
    -- thread and the interpreter don't slow each other down.
    when first $ do
      (cap, _) <- threadCapability =<< myThreadId
      n <- getNumCapabilities
      void (forkOn ((cap + 1) `mod` n) compileThread)

-- | Loads LLVM at startup, when the JIT is turned on, and says how it
-- went: Nothing when the JIT is off, otherwise whether it is activated,
-- and if not, lines explaining why. Whatever the answer, the program runs:
-- without the JIT everything is interpreted.
loadJIT :: IO (Maybe (Either [String] String))
loadJIT = case mode config of
  Off -> pure Nothing
  _ -> Just <$> initJIT

-- | Set once a load has been tried, so that it is tried once only: a
-- failure would be the same the second time, and would print again.
initTried :: IORef (Maybe (Either [String] String))
initTried = unsafePerformIO (newIORef Nothing)
{-# NOINLINE initTried #-}

-- | Loads LLVM, starts the JIT and probes the runtime, the first time it
-- is called; later calls return the first outcome. On success 'jitState'
-- is set and the result names the LLVM loaded; if anything is wrong the
-- JIT stays off and the result says why, in lines meant for the user.
initJIT :: IO (Either [String] String)
initJIT =
  readIORef initTried >>= \case
    Just r -> pure r
    Nothing -> do
      r <- initJIT'
      writeIORef initTried (Just r)
      either (mapM_ jitLog) jitLog r
      pure r

initJIT' :: IO (Either [String] String)
initJIT' = do
  let timed what io = do
        t0 <- getMonotonicTimeNSec
        x <- io
        t1 <- getMonotonicTimeNSec
        x <$ jitLog (what ++ ": " ++ show (fromIntegral (t1 - t0) / 1e6 :: Double) ++ " ms")
  r <- initLLVM
  case r of
    Left (NotFound tried) ->
      pure (Left (("JIT compilation needs LLVM version >= " ++ show minLLVM ++ ", searched these locations:") : map ("  " ++) tried))
    Left (InitError e) -> pure (Left [e ++ "; the JIT is off"])
    Right (version, path) -> do
      layouts <- probeLayouts >>= \case
        Left e -> pure (Left e)
        -- the list helpers are part of the generated code's contract too
        Right ls ->
          timed "list helper checks" (probeLists ls (stressLists config)) >>= \case
            Left e -> pure (Left e)
            Right () ->
              -- bytes before texts: the text checks build bytes too (toUtf8)
              timed "bytes helper checks" (probeBytes ls (stressBytes config)) >>= \case
                Left e -> pure (Left e)
                Right () ->
                  timed "text helper checks" (probeTexts ls (stressTexts config)) >>= \case
                    Left e -> pure (Left e)
                    Right () ->
                      probeArrays ls >>= \case
                        Left e -> pure (Left e)
                        Right () ->
                          probeMurmur ls >>= \case
                            Left e -> pure (Left e)
                            Right () ->
                              probeEnv >>= \case
                                Left e -> pure (Left e)
                                Right () -> fmap (const ls) <$> probeNames ls
      (offs, _) <- ctxLayout
      case (layouts, offs) of
        (Left e, _) -> pure (Left [e ++ "; the JIT is off"])
        (Right ls, [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v]) -> do
          configureNative (stressPoll config) (stressCallee config) (stressCStack config) (stressAlloc config) (trace config)
          facts <- rtsFacts
          case facts of
            -- the allocator's address isn't needed: generated code calls
            -- unison_jit_alloc_words by name, and the JIT resolves it from the process
            [arrWordsInfo, arrPtrsInfo, bytesHdr, bytesCount, ptrsHdr, ptrsCount, ptrsSize, cardBits, _allocator, mutVarVar, _barrier, arrPtrsDirtyInfo] -> do
              let rts = RtsFacts arrWordsInfo arrPtrsInfo bytesHdr bytesCount ptrsHdr ptrsCount ptrsSize cardBits mutVarVar arrPtrsDirtyInfo
              -- the helpers written as IR (cbits/jit_helpers.ll) go in as a
              -- module of their own, ahead of any module that calls them;
              -- their definitions shadow the C copies for generated code
              helpers <-
                if "irhelpers" `elem` disabled config
                  then pure (Right ())
                  else timed "IR helpers" addHelpers >>= \case
                    Left e -> pure (Left ("could not be added: " ++ e))
                    Right _ ->
                      -- the module's definition must be the one generated code
                      -- gets, not the C copy the process also has
                      timed "IR helpers compiled" (lookupSymbol "unison_jit_murmur") <&> \case
                        Left e -> Left ("not found after adding: " ++ e)
                        Right p
                          | castFunPtrToPtr p == castFunPtrToPtr murmurCAddress -> Left "did not shadow the C copies"
                          | otherwise -> Right ()
              case helpers of
                Left e -> pure (Left ["the IR helpers (cbits/jit_helpers.ll) " ++ e ++ "; the JIT is off"])
                Right () -> do
                  writeIORef jitState (Just (JITState ls (CtxOffsets a b c d e f g h i j k l m n o p q r s t u v) rts))
                  triple <- targetTriple
                  pure (Right ("mode " ++ show (mode config) ++ ", LLVM " ++ version ++ " from " ++ path ++ ", target " ++ triple))
            _ -> pure (Left ["unexpected runtime facts; the JIT is off"])
        _ -> pure (Left ["unexpected Ctx layout; the JIT is off"])

-- | Adds the IR helpers' module, after defining the names of the runtime's
-- tables it refers to ("The IR helpers" in internals.md).
addHelpers :: IO (Either String ())
addHelpers = do
  tables <- tableAddresses
  defined <- sequence_ <$> traverse (uncurry defineSymbol) tables
  case defined of
    Left e -> pure (Left e)
    Right () -> void <$> addModule False (passes config) helpersIR

-- | The oldest LLVM the shim works with (why: cbits/jit_llvm.c).
minLLVM :: Int
minLLVM = 20

-- | Compiles a freshly loaded top-level definition, in eager mode.
jitCompileGroup :: Bool -> Reference -> Word64 -> MCombs -> IO ()
jitCompileGroup sandbox ref grp cmbs = case mode config of
  Eager ->
    readIORef jitState >>= \case
      Nothing -> pure ()
      Just st -> do
        types <- readIORef dataTypes
        (now, _) <- groupUnits sandbox False ref grp cmbs
        compileUnits st types ("unison_" ++ show grp) False now
  _ -> pure ()

-- ---------------------------------------------------------------------------
-- The @on@ mode: compiling what gets hot, on a thread of its own.
-- See design.md, "What gets compiled, and when", and internals.md, "The compile driver".

-- | What the interpreter asks the compile thread for.
data Request
  = -- | compile this definition: its group number, and how to read the
    -- code cache (every group's combinators, and their references)
    ReqGroup !Word64 !Bool !Word64 (IO (EC.EnumMap Word64 MCombs, EC.EnumMap Word64 Reference))
  | -- | generate a re-entry function that was left for later
    ReqUnit Unit

-- | Bounded, so a burst of requests costs nothing but the requests: one
-- that doesn't fit is dropped, and made again if the code stays hot.
requests :: TBQueue Request
requests = unsafePerformIO (newTBQueueIO 256)
{-# NOINLINE requests #-}

compileThreadStarted :: IORef Bool
compileThreadStarted = unsafePerformIO (newIORef False)
{-# NOINLINE compileThreadStarted #-}

-- | Queues a request if there is room; says whether there was.
offer :: Request -> IO Bool
offer r = atomically $ do
  full <- isFullTBQueue requests
  unless full (writeTBQueue requests r)
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
jitRequestGroup :: CCache p -> CombIx -> Ptr NativeCell -> IO ()
jitRequestGroup cc (CIx _ grp _) cell
  | cell == noNativeCell = pure () -- a builtin, or code not loaded through the cache
  | otherwise = do
      cache <- readTVarIO (combs cc)
      forM_ (EC.lookup grp cache >>= entryCell) $ \entry -> do
        mine <- claimNativeCell entry
        when mine $ do
          t0 <- getMonotonicTimeNSec
          queued <- offer (ReqGroup grp (sandboxed cc) t0 ((,) <$> readTVarIO (combs cc) <*> readTVarIO (combRefs cc)))
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
jitRequestCell :: Ptr NativeCell -> IO ()
jitRequestCell cell =
  lookupPending cell >>= \case
    Nothing -> pure ()
    Just u -> do
      mine <- claimNativeCell cell
      when mine $ do
        queued <- offer (ReqUnit u)
        -- addPending starts the count again
        unless queued (releaseNativeCell cell >> addPending u)
{-# NOINLINE jitRequestCell #-}

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
edgeCalls :: MCombs -> Word64 -> IO Double
edgeCalls cmbs d = fmap sum . forM (EC.mapToList cmbs) $ \(_, c) -> case c of
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
compileThread :: IO ()
compileThread = do
  initialized <- newIORef False
  callersRef <- newIORef (Callers Set.empty Map.empty)
  let loop held = do
        next <-
          if null held
            then Just <$> atomically (readTBQueue requests)
            else timeout (reentryWait config * 1000) (atomically (readTBQueue requests))
        case next of
          Nothing -> flush held >> loop []
          Just (ReqUnit u) | reentryWait config > 0 -> do
            let held' = u : held
            if length held' >= batch config then flush held' >> loop [] else loop held'
          Just req -> do
            r <- try (serve initialized callersRef req)
            case r of
              Left (e :: SomeException) -> jitLog ("compile thread: " ++ show e)
              Right () -> pure ()
            loop held
      flush [] = pure ()
      flush held = do
        r <- try (withState initialized $ \st types -> do
          let name = "unison_" ++ unitName (last held) ++ "_x" ++ show (length held)
          guarded name (compileUnits st types name True (reverse held))
          forM_ held dropPending)
        case r of
          Left (e :: SomeException) -> jitLog ("compile thread: " ++ show e)
          Right () -> pure ()
  loop []
  where
    withState initialized k = do
      ready <- readIORef initialized
      unless ready (void initJIT >> writeIORef initialized True)
      readIORef jitState >>= \case
        Nothing -> pure ()
        Just st -> do
          types <- readIORef dataTypes
          k st types
    serve initialized callersRef req =
      withState initialized $ \st types -> do
          case req of
            ReqGroup grp sandbox t0 find -> do
              when (logging config) $ do
                t1 <- getMonotonicTimeNSec
                jitLog ("request for " ++ show grp ++ " served after " ++ show ((t1 - t0) `div` 1000) ++ " us")
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
                      [ groupUnits sandbox True ref g cmbs
                        | g <- members,
                          Just cmbs <- [EC.lookup g cache],
                          Just ref <- [EC.lookup g refs]
                      ]
                -- pending before the groups' code can exit to them
                forM_ (concat later) addPending
                copies <- copyCandidates sandbox cache refs members
                guarded ("unison_" ++ show grp) (compileUnits st types ("unison_" ++ show grp) True (concat now ++ copies))
            ReqUnit u -> do
              guarded (unitName u) (compileUnits st types ("unison_" ++ unitName u) True [u])
              dropPending u
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
            Callee -> maybe (pure 0) (`edgeCalls` v) (EC.lookup g cache)
            Caller -> maybe (pure 0) (`edgeCalls` g) (EC.lookup v cache)
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
          best = Map.foldlWithKey' (\acc v (w, n) -> case acc of
                                      Just (_, (w', n')) | (w', negate n') >= (w, negate n) -> acc
                                      _ -> Just (v, (w, n))) Nothing
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
                        jitLog ("  " ++ show v ++ " joins the batch for " ++ show grp ++ ", weight " ++ show w)
                        expand v (seen', cands') >>= go (v : taken) seen'
                      else go taken seen' cands'
      cands0 <- expand grp (Set.singleton grp, Map.empty)
      (members, left) <- go [grp] (Set.singleton grp) cands0
      when (logging config && not (Map.null left)) $
        jitLog ("  left out of the batch for " ++ show grp ++ ": " ++ unwords [show v ++ " (weight " ++ show w ++ ")" | (v, (w, _)) <- Map.toList left])
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
                      (_, est) <- judge cell entry
                      case est of
                        Just (Estimate saving _ False) | saving <= copyBound config -> copyUnits sandbox ref d cmbs
                        _ -> pure []
            _ -> pure []
          let units = take (batch config) (concat picked)
          unless (null units) (jitLog ("private copies: " ++ unwords (map unitName units)))
          pure units
    guarded what act =
      try (act >>= evaluate) >>= \case
        Left (e :: SomeException) -> jitLog (what ++ ": " ++ show e)
        Right () -> pure ()

-- | Prints how often each exit was taken, when UNISON_JIT_STATS is set.
-- Called after each evaluation.
printJITStats :: IO ()
printJITStats = when (stats config) $ do
  t <- compileTotals
  jitDump
    ( "[jit] compiled " ++ show (ctModules t) ++ " modules: " ++ show (ctFunctions t) ++ " functions, "
        ++ show (ctAuxiliary t) ++ " auxiliary functions with them, " ++ show (ctOnDemand t)
        ++ " re-entry functions on demand (" ++ show (ctPending t) ++ " never asked for); "
        ++ show (ctNotWorthIt t) ++ " left interpreted by the exit rule; "
        ++ show (ctIRBytes t `div` 1024) ++ " KB of IR, " ++ show (fromIntegral (ctNanoseconds t) / 1e6 :: Double) ++ " ms"
        ++ " (generate " ++ show (fromIntegral (ctGenerate t) / 1e6 :: Double) ++ ", parse and optimize " ++ show (fromIntegral (ctOptimize t) / 1e6 :: Double) ++ ", code " ++ show (fromIntegral (ctCodegen t) / 1e6 :: Double) ++ ")"
    )
  printExits maxBound
  when (statsEach config) resetExitCounts

-- | With @UNISON_JIT_STATS=each@: the exits taken since the program last
-- wrote output (the busiest sites only), after which counting starts again.
printExitsSinceOutput :: IO ()
printExitsSinceOutput = do
  printExits 12
  resetExitCounts

printExits :: Int -> IO ()
printExits most = do
  entered <- entryCounts
  let enteredTotal = sum (map snd entered)
  when (enteredTotal > 0) $ do
    jitDump ("[jit] " ++ show enteredTotal ++ " entries into native code from the trampoline (" ++ show (length entered) ++ " functions):")
    forM_ (take most (sortOn (negate . snd) entered)) $ \(name, n) ->
      jitDump ("  " ++ show n ++ "\t" ++ name)
  counts <- exitCounts
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
