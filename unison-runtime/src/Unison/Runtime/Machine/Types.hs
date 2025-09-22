{-# LANGUAGE CPP #-}

module Unison.Runtime.Machine.Types where

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM as STM
import Control.Exception hiding (Handler)
#if !defined(mingw32_HOST_OS)
import Data.IORef
  (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef)
#else
import Data.IORef
  (IORef, newIORef, readIORef, writeIORef)
#endif
import Data.Kind (Type)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Word
#if !defined(mingw32_HOST_OS)
import GHC.Event (getSystemTimerManager, registerTimeout)
#else
import System.CPUTime
#endif
import Unison.Builtin.Decls (ioFailureRef)
import Unison.Prelude
import Unison.Reference (Reference, isBuiltin)
import Unison.Referent (Referent, pattern Ref)
import Unison.Runtime.ANF
  ( Cacheability (..),
    Code (..),
    SuperGroup (..),
    Value,
    foldGroupLinks,
    valueLinks,
  )
import Unison.Runtime.ANF.Optimize (OptInfos)
import Unison.Runtime.Builtin
import Unison.Runtime.Exception qualified as Exception
import Unison.Runtime.Foreign (Failure (..))
import Unison.Runtime.InternalError (CompileExn (CE))
import Unison.Runtime.MCode
import Unison.Runtime.Profiling
import Unison.Runtime.Referenced
import Unison.Runtime.Stack
import Unison.Symbol
import Unison.Util.EnumContainers as EC
import Unison.Util.Text as UText

-- | A ref storing every currently active thread.
-- This is helpful for cleaning up orphaned threads when the main process
-- completes.
--
-- We track threads when running in a host process like UCM,
-- otherwise, in one-off environments 'Nothing' is used and we don't bother tracking forked threads since they'll be
-- cleaned up automatically on process termination.
type ActiveThreads = Maybe (IORef (Set ThreadId))

type Tag = Word64

type MCombs = RCombs Val

type Combs = GCombs Void CombIx

type MSection = RSection Val

type MBranch = RBranch Val

type MInstr = RInstr Val

type MComb = RComb Val

type MRef = RRef Val

data Tracer
  = NoTrace
  | MsgTrace String String String
  | SimpleTrace String

refLookup :: String -> M.Map Reference Word64 -> Reference -> Word64
refLookup s m r
  | Just w <- M.lookup r m = w
  | otherwise =
      error $ "refLookup:" ++ s ++ ": unknown reference: " ++ show r

-- A class parameterizing profiling. The interpreter loop can be
-- specialized to a class, which allows the same code to be used for both
-- normal and profiling execution without sacrificing performance. If
-- desired, other aspects of the runtime could be configured this way.
class RuntimeProfiler prof where
  data Ticker prof :: Type

  -- starts a ticker for a profiler
  startTicker :: prof -> IO (Ticker prof, IO ())
  checkTicker :: Ticker prof -> CombIx -> K -> IO ()

instance RuntimeProfiler () where
  data Ticker () = NilTick
  startTicker () = pure (NilTick, pure ())
  checkTicker _ _ _ = pure ()
  {-# INLINE checkTicker #-}

type Tick = CombIx -> K -> IO ()
#if !defined(mingw32_HOST_OS)
-- GHC.Event, time-baed profiler
instance RuntimeProfiler ProfileComm where
  newtype Ticker ProfileComm = ProfTicker (IORef (Maybe Tick))

  startTicker (PC pf _ _) = do
    ticker <- newIORef Nothing
    cancel <- newIORef False
    tm <- getSystemTimerManager
    void . registerTimeout tm 100 $
      tickCallback 100 pf ticker cancel
    pure (ProfTicker ticker, writeIORef cancel True)

  checkTicker (ProfTicker ticker) cix k =
    atomicModifyIORef ticker (Nothing,) >>= \case
      Nothing -> pure ()
      Just pf -> pf cix k
  {-# INLINE checkTicker #-}

-- Callback for producing ticks via event manager timeouts. These happen
-- promptly, but probably should have short callbacks, since they're
-- running in the scheduler. here, we just write a tick to the MVar that
-- is checked periodically by runtime threads, then set a new timeout
-- unless we've been cancelled.
--
-- The callback doesn't block trying to write to the MVar, so if something
-- is already there, a second tick just won't happen.
tickCallback ::
  Int ->
  (Bool -> Tick) ->
  IORef (Maybe Tick) ->
  IORef Bool ->
  IO ()
tickCallback interval ptick ticker cancel = body
  where
    body = do
      _full <- atomicModifyIORef ticker \(isJust -> b) ->
        (Just $ ptick b, b)
      b <- readIORef cancel
      when (not b) do
        tm <- getSystemTimerManager
        () <$ registerTimeout tm interval body

#else

-- CPUTime based profiler for Windows
instance RuntimeProfiler ProfileComm where
  data Ticker ProfileComm = TPC !Tick !(IORef Word8)
  startTicker (PC pf _ _) = (, pure ()) . TPC (pf False)  <$> newIORef 1

  checkTicker (TPC tick r) cix k = do
    n <- readIORef r
    when (n `mod` 128 == 0) do
      n <- getCPUTime
      when (n `mod` 100000 == 0) $ tick cix k
    writeIORef r (n+1)

#endif

-- code caching environment
data CCache prof = CCache
  { sandboxed :: Bool,
    tracer :: Bool -> Val -> Tracer,
    profiler :: !prof,
    -- Combinators in their original form, where they're easier to serialize into SCache
    srcCombs :: TVar (EnumMap Word64 Combs),
    combs :: TVar (EnumMap Word64 MCombs),
    combRefs :: TVar (EnumMap Word64 Reference),
    -- Combs which we're allowed to cache after evaluating
    cacheableCombs :: TVar (EnumSet Word64),
    optInfos :: TVar (OptInfos Reference Symbol),
    tagRefs :: TVar (EnumMap Word64 Reference),
    freshTm :: TVar Word64,
    freshTy :: TVar Word64,
    intermed :: TVar (M.Map Reference (SuperGroup Reference Symbol)),
    refTm :: TVar (M.Map Reference Word64),
    refTy :: TVar (M.Map Reference Word64),
    sandbox :: TVar (M.Map Reference (Set Reference))
  }

refNumsTm :: CCache prof -> IO (M.Map Reference Word64)
refNumsTm cc = readTVarIO (refTm cc)

refNumsTy :: CCache prof -> IO (M.Map Reference Word64)
refNumsTy cc = readTVarIO (refTy cc)

refNumTm :: CCache prof -> Reference -> IO Word64
refNumTm cc r =
  refNumsTm cc >>= \case
    (M.lookup r -> Just w) -> pure w
    _ -> Exception.die [] $ "refNumTm: unknown reference: " ++ show r

baseCCache :: Bool -> IO (CCache ())
baseCCache sandboxed = do
  CCache sandboxed noTrace ()
    <$> newTVarIO srcCombs
    <*> newTVarIO combs
    <*> newTVarIO builtinTermBackref
    <*> newTVarIO cacheableCombs
    <*> newTVarIO builtinOptInfo
    <*> newTVarIO builtinTypeBackref
    <*> newTVarIO ftm
    <*> newTVarIO fty
    <*> newTVarIO mempty
    <*> newTVarIO builtinTermNumbering
    <*> newTVarIO builtinTypeNumbering
    <*> newTVarIO baseSandboxInfo
  where
    cacheableCombs = mempty
    noTrace _ _ = NoTrace
    ftm = 1 + maximum builtinTermNumbering
    fty = 1 + maximum builtinTypeNumbering

    rns = emptyRNs {dnum = refLookup "ty" builtinTypeNumbering}

    srcCombs :: EnumMap Word64 Combs
    srcCombs =
      numberedTermLookup
        & mapWithKey
          (\k v -> let r = builtinTermBackref ! k in emitComb @Symbol rns r k mempty (0, v))
    combs :: EnumMap Word64 MCombs
    combs =
      srcCombs
        & sanitizeCombsOfForeignFuncs sandboxed sandboxedForeignFuncs
        & absurdCombs
        & resolveCombs Nothing

lookupCode :: CCache prof -> Referent -> IO (Maybe (Referenced Code))
lookupCode env (Ref link) =
  resolveCode link
    <$> readTVarIO (intermed env)
    <*> readTVarIO (refTm env)
    <*> readTVarIO (cacheableCombs env)
    >>= traverse canonicalizeCodeRefs
lookupCode _ _ = Exception.die [] "lookupCode: Expected Ref"

-- Traverses a `Code`, calculating the used references within, and
-- canonicalizing them in memory.
canonicalizeCodeRefs ::
  Code Reference -> IO (Referenced Code)
canonicalizeCodeRefs = toReferenced . canonicalizeRefs

resolveCode ::
  Reference ->
  Map Reference (SuperGroup Reference Symbol) ->
  Map Reference Word64 ->
  EnumSet Word64 ->
  Maybe (Code Reference)
resolveCode link m rfn cach
  | Just sg <- M.lookup link m,
    ch <- cacheability rfn cach link =
      Just $ CodeRep sg ch
  | Just w <- M.lookup link builtinTermNumbering,
    Just sn <- EC.lookup w numberedTermLookup =
      Just $ CodeRep (Rec [] sn) Uncacheable
  | otherwise = Nothing

cacheability ::
  Map Reference Word64 ->
  EnumSet Word64 ->
  Reference ->
  Cacheability
cacheability rfn cach link
  | Just n <- M.lookup link rfn,
    EC.member n cach =
      Cacheable
  | otherwise = Uncacheable

checkSandboxing ::
  CCache prof ->
  [Reference] ->
  Closure ->
  IO Bool
checkSandboxing cc allowed0 c = do
  sands <- readTVarIO $ sandbox cc
  let f r
        | Just rs <- M.lookup r sands =
            rs `S.difference` allowed
        | otherwise = mempty
  pure $ S.null (closureTermRefs f c)
  where
    allowed = S.fromList allowed0

-- Checks a Value for sandboxing. A Left result indicates that some
-- dependencies of the Value are unknown. A Right result indicates
-- builtins transitively referenced by the Value that are disallowed.
checkValueSandboxing ::
  CCache prof ->
  [Reference] ->
  Value Reference ->
  IO (Either [Referent] [Referent])
checkValueSandboxing cc allowed0 v = do
  sands <- readTVarIO $ sandbox cc
  have <- readTVarIO $ intermed cc
  let f False r
        | Nothing <- M.lookup r have,
          not (isBuiltin r) =
            (S.singleton r, mempty)
        | Just rs <- M.lookup r sands =
            (mempty, rs `S.difference` allowed)
      f _ _ = (mempty, mempty)
  case valueLinks f v of
    (miss, sbx)
      | S.null miss -> pure . Right . map Ref $ S.toList sbx
      | otherwise -> pure . Left . map Ref $ S.toList miss
  where
    allowed = S.fromList allowed0

codeValidate ::
  CCache prof ->
  [(Reference, SuperGroup Reference Symbol)] ->
  IO (Maybe (Failure UText.Text))
codeValidate cc tml = do
  rty0 <- readTVarIO (refTy cc)
  fty <- readTVarIO (freshTy cc)
  let f b r
        | b, M.notMember r rty0 = S.singleton r
        | otherwise = mempty
      ntys0 = (foldMap . foldMap) (foldGroupLinks f) tml
      ntys = M.fromList $ zip (S.toList ntys0) [fty ..]
      rty = ntys <> rty0
  ftm <- readTVarIO (freshTm cc)
  rtm0 <- readTVarIO (refTm cc)
  let rs = fst <$> tml
      rtm = rtm0 `M.union` M.fromList (zip rs [ftm ..])
      rns = RN (refLookup "ty" rty) (refLookup "tm" rtm) (const Nothing)
      combinate (n, (r, g)) = evaluate $ emitCombs rns r n g
  (Nothing <$ traverse_ combinate (zip [ftm ..] tml))
    `catch` \(CE cs _issues perr) ->
      let msg = UText.pack perr
          extra = UText.pack $ show cs
       in pure . Just $ Failure ioFailureRef msg extra
