{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Runtime.Machine2 where

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM as STM
import Control.Exception
import Control.Lens
import Data.Bits
import Data.Map.Strict qualified as M
import Data.Ord (comparing)
import Data.Primitive.ByteArray qualified as BA
import Data.Sequence qualified as Sq
import Data.Set qualified as S
import Data.Set qualified as Set
import Data.Text qualified as DTx
import Data.Text.IO qualified as Tx
import Data.Traversable
import GHC.Conc as STM (unsafeIOToSTM)
import Unison.Builtin.Decls (exceptionRef, ioFailureRef)
import Unison.Builtin.Decls qualified as Rf
import Unison.ConstructorReference qualified as CR
import Unison.Debug qualified as Debug
import Unison.Prelude hiding (Text)
import Unison.Reference
  ( Reference,
    Reference' (Builtin),
    isBuiltin,
    toShortHash,
  )
import Unison.Referent (Referent, pattern Con, pattern Ref)
import Unison.Runtime.ANF as ANF
  ( CompileExn (..),
    SuperGroup,
    foldGroupLinks,
    maskTags,
    packTags,
    valueLinks,
  )
import Unison.Runtime.ANF qualified as ANF
import Unison.Runtime.Array as PA
import Unison.Runtime.Builtin2
import Unison.Runtime.Exception2
import Unison.Runtime.Foreign
import Unison.Runtime.Foreign.Function2
import Unison.Runtime.MCode2
import Unison.Runtime.Stack2
import Unison.ShortHash qualified as SH
import Unison.Symbol (Symbol)
import Unison.Type qualified as Rf
import Unison.Util.Bytes qualified as By
import Unison.Util.EnumContainers as EC
import Unison.Util.Pretty (toPlainUnbroken)
import Unison.Util.Text qualified as Util.Text
import UnliftIO (IORef)
import UnliftIO qualified
import UnliftIO.Concurrent qualified as UnliftIO

-- | A ref storing every currently active thread.
-- This is helpful for cleaning up orphaned threads when the main process
-- completes.
--
-- We track threads when running in a host process like UCM,
-- otherwise, in one-off environments 'Nothing' is used and we don't bother tracking forked threads since they'll be
-- cleaned up automatically on process termination.
type ActiveThreads = Maybe (IORef (Set ThreadId))

type Tag = Word64

-- dynamic environment
type DEnv = EnumMap Word64 Closure

type MCombs = RCombs Closure

type Combs = GCombs Void CombIx

type MSection = RSection Closure

type MBranch = RBranch Closure

type MInstr = RInstr Closure

type MComb = RComb Closure

type MRef = RRef Closure

data Tracer
  = NoTrace
  | MsgTrace String String String
  | SimpleTrace String

-- | Whether the evaluation of a given definition is cacheable or not.
-- i.e. it's a top-level pure value.
data Cacheability = Cacheable | Uncacheable
  deriving stock (Eq, Show)

-- code caching environment
data CCache = CCache
  { foreignFuncs :: EnumMap Word64 ForeignFunc,
    sandboxed :: Bool,
    tracer :: Bool -> Closure -> Tracer,
    -- Combinators in their original form, where they're easier to serialize into SCache
    srcCombs :: TVar (EnumMap Word64 Combs),
    combs :: TVar (EnumMap Word64 MCombs),
    combRefs :: TVar (EnumMap Word64 Reference),
    -- Combs which we're allowed to cache after evaluating
    cacheableCombs :: TVar (EnumSet Word64),
    tagRefs :: TVar (EnumMap Word64 Reference),
    freshTm :: TVar Word64,
    freshTy :: TVar Word64,
    intermed :: TVar (M.Map Reference (SuperGroup Symbol)),
    refTm :: TVar (M.Map Reference Word64),
    refTy :: TVar (M.Map Reference Word64),
    sandbox :: TVar (M.Map Reference (Set Reference))
  }

refNumsTm :: CCache -> IO (M.Map Reference Word64)
refNumsTm cc = readTVarIO (refTm cc)

refNumsTy :: CCache -> IO (M.Map Reference Word64)
refNumsTy cc = readTVarIO (refTy cc)

refNumTm :: CCache -> Reference -> IO Word64
refNumTm cc r =
  refNumsTm cc >>= \case
    (M.lookup r -> Just w) -> pure w
    _ -> die $ "refNumTm: unknown reference: " ++ show r

refNumTy :: CCache -> Reference -> IO Word64
refNumTy cc r =
  refNumsTy cc >>= \case
    (M.lookup r -> Just w) -> pure w
    _ -> die $ "refNumTy: unknown reference: " ++ show r

refNumTy' :: CCache -> Reference -> IO (Maybe Word64)
refNumTy' cc r = M.lookup r <$> refNumsTy cc

baseCCache :: Bool -> IO CCache
baseCCache sandboxed = do
  CCache ffuncs sandboxed noTrace
    <$> newTVarIO srcCombs
    <*> newTVarIO combs
    <*> newTVarIO builtinTermBackref
    <*> newTVarIO cacheableCombs
    <*> newTVarIO builtinTypeBackref
    <*> newTVarIO ftm
    <*> newTVarIO fty
    <*> newTVarIO mempty
    <*> newTVarIO builtinTermNumbering
    <*> newTVarIO builtinTypeNumbering
    <*> newTVarIO baseSandboxInfo
  where
    cacheableCombs = mempty
    ffuncs | sandboxed = sandboxedForeigns | otherwise = builtinForeigns
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
        & absurdCombs
        & resolveCombs Nothing

info :: (Show a) => String -> a -> IO ()
info ctx x = infos ctx (show x)

infos :: String -> String -> IO ()
infos ctx s = putStrLn $ ctx ++ ": " ++ s

stk'info :: Stack -> IO ()
stk'info s@(Stack _ _ sp _ _) = do
  let prn i
        | i < 0 = return ()
        | otherwise = bpeekOff s i >>= print >> prn (i - 1)
  prn sp

-- Entry point for evaluating a section
eval0 :: CCache -> ActiveThreads -> MSection -> IO ()
eval0 !env !activeThreads !co = do
  stk <- alloc
  cmbs <- readTVarIO $ combs env
  (denv, k) <-
    topDEnv cmbs <$> readTVarIO (refTy env) <*> readTVarIO (refTm env)
  eval env denv activeThreads stk (k KE) dummyRef co

topDEnv ::
  EnumMap Word64 MCombs ->
  M.Map Reference Word64 ->
  M.Map Reference Word64 ->
  (DEnv, K -> K)
topDEnv combs rfTy rfTm
  | Just n <- M.lookup exceptionRef rfTy,
    -- TODO: Should I special-case this raise ref and pass it down from the top rather than always looking it up?
    rcrf <- Builtin (DTx.pack "raise"),
    Just j <- M.lookup rcrf rfTm =
      let cix = (CIx rcrf j 0)
          comb = rCombSection combs cix
       in ( EC.mapSingleton n (PAp cix comb nullSeg),
            Mark 0 (EC.setSingleton n) mempty
          )
topDEnv _ _ _ = (mempty, id)

-- Entry point for evaluating a numbered combinator.
-- An optional callback for the base of the stack may be supplied.
--
-- This is the entry point actually used in the interactive
-- environment currently.
apply0 ::
  Maybe (Stack -> IO ()) ->
  CCache ->
  ActiveThreads ->
  Word64 ->
  IO ()
apply0 !callback !env !threadTracker !i = do
  stk <- alloc
  cmbrs <- readTVarIO $ combRefs env
  cmbs <- readTVarIO $ combs env
  (denv, kf) <-
    topDEnv cmbs <$> readTVarIO (refTy env) <*> readTVarIO (refTm env)
  r <- case EC.lookup i cmbrs of
    Just r -> pure r
    Nothing -> die "apply0: missing reference to entry point"
  let entryCix = (CIx r i 0)
  let entryComb = rCombSection cmbs entryCix
  apply env denv threadTracker stk (kf k0) True ZArgs $
    PAp entryCix entryComb nullSeg
  where
    k0 = maybe KE (CB . Hook) callback

-- Apply helper currently used for forking. Creates the new stacks
-- necessary to evaluate a closure with the provided information.
apply1 ::
  (Stack -> IO ()) ->
  CCache ->
  ActiveThreads ->
  Closure ->
  IO ()
apply1 callback env threadTracker clo = do
  stk <- alloc
  apply env mempty threadTracker stk k0 True ZArgs clo
  where
    k0 = CB $ Hook callback

-- Entry point for evaluating a saved continuation.
--
-- The continuation must be from an evaluation context expecting a
-- unit value.
jump0 ::
  (Stack -> IO ()) ->
  CCache ->
  ActiveThreads ->
  Closure ->
  IO ()
jump0 !callback !env !activeThreads !clo = do
  stk <- alloc
  cmbs <- readTVarIO $ combs env
  (denv, kf) <-
    topDEnv cmbs <$> readTVarIO (refTy env) <*> readTVarIO (refTm env)
  bstk <- bump stk
  bpoke bstk (Enum Rf.unitRef unitTag)
  jump env denv activeThreads stk (kf k0) (VArg1 0) clo
  where
    k0 = CB (Hook callback)

unitValue :: Closure
unitValue = Enum Rf.unitRef unitTag

lookupDenv :: Word64 -> DEnv -> Closure
lookupDenv p denv = fromMaybe BlackHole $ EC.lookup p denv

buildLit :: Reference -> Word64 -> MLit -> Closure
buildLit rf tt (MI i) = DataU1 rf tt i
buildLit _ _ (MT t) = Foreign (Wrap Rf.textRef t)
buildLit _ _ (MM r) = Foreign (Wrap Rf.termLinkRef r)
buildLit _ _ (MY r) = Foreign (Wrap Rf.typeLinkRef r)
buildLit _ _ (MD _) = error "buildLit: double"

-- | Execute an instruction
exec ::
  CCache ->
  DEnv ->
  ActiveThreads ->
  Stack ->
  K ->
  Reference ->
  MInstr ->
  IO (DEnv, Stack, K)
exec !_ !denv !_activeThreads !stk !k _ (Info tx) = do
  info tx stk
  info tx k
  pure (denv, stk, k)
exec !env !denv !_activeThreads !stk !k _ (Name r args) = do
  stk <- name stk args =<< resolve env denv stk r
  pure (denv, stk, k)
exec !_ !denv !_activeThreads !stk !k _ (SetDyn p i) = do
  clo <- bpeekOff stk i
  pure (EC.mapInsert p clo denv, stk, k)
exec !_ !denv !_activeThreads !stk !k _ (Capture p) = do
  (cap, denv, stk, k) <- splitCont denv stk k p
  stk <- bump stk
  bpoke stk cap
  pure (denv, stk, k)
exec !_ !denv !_activeThreads !stk !k _ (UPrim1 op i) = do
  stk <- uprim1 stk op i
  pure (denv, stk, k)
exec !_ !denv !_activeThreads !stk !k _ (UPrim2 op i j) = do
  stk <- uprim2 stk op i j
  pure (denv, stk, k)
exec !env !denv !_activeThreads !stk !k _ (BPrim1 MISS i)
  | sandboxed env = die "attempted to use sandboxed operation: isMissing"
  | otherwise = do
      clink <- bpeekOff stk i
      let link = case unwrapForeign $ marshalToForeign clink of
            Ref r -> r
            _ -> error "exec:BPrim1:MISS: Expected Ref"
      m <- readTVarIO (intermed env)
      stk <- bump stk
      if (link `M.member` m) then upoke stk 1 else upoke stk 0
      pure (denv, stk, k)
exec !env !denv !_activeThreads !stk !k _ (BPrim1 CACH i)
  | sandboxed env = die "attempted to use sandboxed operation: cache"
  | otherwise = do
      arg <- peekOffS stk i
      news <- decodeCacheArgument arg
      unknown <- cacheAdd news env
      stk <- bump stk
      pokeS
        stk
        (Sq.fromList $ Foreign . Wrap Rf.termLinkRef . Ref <$> unknown)
      pure (denv, stk, k)
exec !env !denv !_activeThreads !stk !k _ (BPrim1 CVLD i)
  | sandboxed env = die "attempted to use sandboxed operation: validate"
  | otherwise = do
      arg <- peekOffS stk i
      news <- decodeCacheArgument arg
      codeValidate news env >>= \case
        Nothing -> do
          stk <- bump stk
          upoke stk 0
          pure (denv, stk, k)
        Just (Failure ref msg clo) -> do
          stk <- bump stk
          upoke stk 1
          stk <- bumpn stk 3
          bpoke stk (Foreign $ Wrap Rf.typeLinkRef ref)
          pokeOffBi stk 1 msg
          bpokeOff stk 2 clo
          pure (denv, stk, k)
exec !env !denv !_activeThreads !stk !k _ (BPrim1 LKUP i)
  | sandboxed env = die "attempted to use sandboxed operation: lookup"
  | otherwise = do
      clink <- bpeekOff stk i
      let link = case unwrapForeign $ marshalToForeign clink of
            Ref r -> r
            _ -> error "exec:BPrim1:LKUP: Expected Ref"
      m <- readTVarIO (intermed env)
      stk <- bump stk
      stk <- case M.lookup link m of
        Nothing
          | Just w <- M.lookup link builtinTermNumbering,
            Just sn <- EC.lookup w numberedTermLookup -> do
              upoke stk 1
              stk <- bump stk
              stk <$ pokeBi stk (ANF.Rec [] sn)
          | otherwise -> stk <$ upoke stk 0
        Just sg -> do
          upoke stk 1
          stk <- bump stk
          stk <$ pokeBi stk sg
      pure (denv, stk, k)
exec !_ !denv !_activeThreads !stk !k _ (BPrim1 TLTT i) = do
  clink <- bpeekOff stk i
  let shortHash = case unwrapForeign $ marshalToForeign clink of
        Ref r -> toShortHash r
        Con r _ -> CR.toShortHash r
  let sh = Util.Text.fromText . SH.toText $ shortHash
  stk <- bump stk
  pokeBi stk sh
  pure (denv, stk, k)
exec !env !denv !_activeThreads !stk !k _ (BPrim1 LOAD i)
  | sandboxed env = die "attempted to use sandboxed operation: load"
  | otherwise = do
      v <- peekOffBi stk i
      stk <- bump stk
      stk <- bump stk
      reifyValue env v >>= \case
        Left miss -> do
          upokeOff stk 1 0
          pokeS stk $
            Sq.fromList $
              Foreign . Wrap Rf.termLinkRef . Ref <$> miss
        Right x -> do
          upokeOff stk 1 1
          bpoke stk x
      pure (denv, stk, k)
exec !env !denv !_activeThreads !stk !k _ (BPrim1 VALU i) = do
  m <- readTVarIO (tagRefs env)
  c <- bpeekOff stk i
  stk <- bump stk
  pokeBi stk =<< reflectValue m c
  pure (denv, stk, k)
exec !env !denv !_activeThreads !stk !k _ (BPrim1 DBTX i)
  | sandboxed env =
      die "attempted to use sandboxed operation: Debug.toText"
  | otherwise = do
      clo <- bpeekOff stk i
      stk <- bump stk
      stk <- case tracer env False clo of
        NoTrace -> stk <$ upoke stk 0
        MsgTrace _ _ tx -> do
          upoke stk 1
          stk <- bump stk
          stk <$ pokeBi stk (Util.Text.pack tx)
        SimpleTrace tx -> do
          upoke stk 2
          stk <- bump stk
          stk <$ pokeBi stk (Util.Text.pack tx)
      pure (denv, stk, k)
exec !env !denv !_activeThreads !stk !k _ (BPrim1 SDBL i)
  | sandboxed env =
      die "attempted to use sandboxed operation: sandboxLinks"
  | otherwise = do
      tl <- peekOffBi stk i
      stk <- bump stk
      pokeS stk . encodeSandboxListResult =<< sandboxList env tl
      pure (denv, stk, k)
exec !_ !denv !_activeThreads !stk !k _ (BPrim1 op i) = do
  stk <- bprim1 stk op i
  pure (denv, stk, k)
exec !env !denv !_activeThreads !stk !k _ (BPrim2 SDBX i j) = do
  s <- peekOffS stk i
  c <- bpeekOff stk j
  l <- decodeSandboxArgument s
  b <- checkSandboxing env l c
  stk <- bump stk
  upoke stk $ if b then 1 else 0
  pure (denv, stk, k)
exec !env !denv !_activeThreads !stk !k _ (BPrim2 SDBV i j)
  | sandboxed env =
      die "attempted to use sandboxed operation: Value.validateSandboxed"
  | otherwise = do
      s <- peekOffS stk i
      v <- peekOffBi stk j
      l <- decodeSandboxArgument s
      res <- checkValueSandboxing env l v
      stk <- bump stk
      bpoke stk $ encodeSandboxResult res
      pure (denv, stk, k)
exec !_ !denv !_activeThreads !stk !k _ (BPrim2 EQLU i j) = do
  x <- bpeekOff stk i
  y <- bpeekOff stk j
  stk <- bump stk
  upoke stk $ if universalEq (==) x y then 1 else 0
  pure (denv, stk, k)
exec !_ !denv !_activeThreads !stk !k _ (BPrim2 CMPU i j) = do
  x <- bpeekOff stk i
  y <- bpeekOff stk j
  stk <- bump stk
  upoke stk . fromEnum $ universalCompare compare x y
  pure (denv, stk, k)
exec !_ !_ !_activeThreads !stk !k r (BPrim2 THRO i j) = do
  name <- peekOffBi @Util.Text.Text stk i
  x <- bpeekOff stk j
  throwIO (BU (traceK r k) (Util.Text.toText name) x)
exec !env !denv !_activeThreads !stk !k _ (BPrim2 TRCE i j)
  | sandboxed env = die "attempted to use sandboxed operation: trace"
  | otherwise = do
      tx <- peekOffBi stk i
      clo <- bpeekOff stk j
      case tracer env True clo of
        NoTrace -> pure ()
        SimpleTrace str -> do
          putStrLn $ "trace: " ++ Util.Text.unpack tx
          putStrLn str
        MsgTrace msg ugl pre -> do
          putStrLn $ "trace: " ++ Util.Text.unpack tx
          putStrLn ""
          putStrLn msg
          putStrLn "\nraw structure:\n"
          putStrLn ugl
          putStrLn "partial decompilation:\n"
          putStrLn pre
      pure (denv, stk, k)
exec !_ !denv !_trackThreads !stk !k _ (BPrim2 op i j) = do
  stk <- bprim2 stk op i j
  pure (denv, stk, k)
exec !_ !denv !_activeThreads !stk !k _ (Pack r t args) = do
  clo <- buildData stk r t args
  stk <- bump stk
  bpoke stk clo
  pure (denv, stk, k)
exec !_ !denv !_activeThreads !stk !k _ (Unpack r i) = do
  stk <- dumpData r stk =<< bpeekOff stk i
  pure (denv, stk, k)
exec !_ !denv !_activeThreads !stk !k _ (Print i) = do
  t <- peekOffBi stk i
  Tx.putStrLn (Util.Text.toText t)
  pure (denv, stk, k)
exec !_ !denv !_activeThreads !stk !k _ (Lit (MI n)) = do
  ustk <- bump stk
  upoke ustk n
  pure (denv, stk, k)
exec !_ !denv !_activeThreads !stk !k _ (Lit (MD d)) = do
  stk <- bump stk
  pokeD stk d
  pure (denv, stk, k)
exec !_ !denv !_activeThreads !stk !k _ (Lit (MT t)) = do
  stk <- bump stk
  bpoke stk (Foreign (Wrap Rf.textRef t))
  pure (denv, stk, k)
exec !_ !denv !_activeThreads !stk !k _ (Lit (MM r)) = do
  stk <- bump stk
  bpoke stk (Foreign (Wrap Rf.termLinkRef r))
  pure (denv, stk, k)
exec !_ !denv !_activeThreads !stk !k _ (Lit (MY r)) = do
  stk <- bump stk
  bpoke stk (Foreign (Wrap Rf.typeLinkRef r))
  pure (denv, stk, k)
exec !_ !denv !_activeThreads !stk !k _ (BLit rf tt l) = do
  stk <- bump stk
  bpoke stk $ buildLit rf tt l
  pure (denv, stk, k)
exec !_ !denv !_activeThreads !stk !k _ (Reset ps) = do
  (stk, a) <- saveArgs stk
  pure (denv, stk, Mark a ps clos k)
  where
    clos = EC.restrictKeys denv ps
exec !_ !denv !_activeThreads !stk !k _ (Seq as) = do
  l <- closureArgs stk as
  stk <- bump stk
  pokeS stk $ Sq.fromList l
  pure (denv, stk, k)
exec !env !denv !_activeThreads !stk !k _ (ForeignCall _ w args)
  | Just (FF arg res ev) <- EC.lookup w (foreignFuncs env) =
      (denv,,k)
        <$> (arg stk args >>= ev >>= res stk)
  | otherwise =
      die $ "reference to unknown foreign function: " ++ show w
exec !env !denv !activeThreads !stk !k _ (Fork i)
  | sandboxed env = die "attempted to use sandboxed operation: fork"
  | otherwise = do
      tid <- forkEval env activeThreads =<< bpeekOff stk i
      stk <- bump stk
      bpoke stk . Foreign . Wrap Rf.threadIdRef $ tid
      pure (denv, stk, k)
exec !env !denv !activeThreads !stk !k _ (Atomically i)
  | sandboxed env = die $ "attempted to use sandboxed operation: atomically"
  | otherwise = do
      c <- bpeekOff stk i
      stk <- bump stk
      atomicEval env activeThreads (bpoke stk) c
      pure (denv, stk, k)
exec !env !denv !activeThreads !stk !k _ (TryForce i)
  | sandboxed env = die $ "attempted to use sandboxed operation: tryForce"
  | otherwise = do
      c <- bpeekOff stk i
      stk <- bump stk
      -- TODO: This one is a little tricky, double-check it.
      ev <- Control.Exception.try $ nestEval env activeThreads (bpoke stk) c
      -- TODO: Why don't we do this bump inside encode Exn itself?
      stk <- bump stk
      stk <- encodeExn stk ev
      pure (denv, stk, k)
{-# INLINE exec #-}

encodeExn ::
  Stack ->
  Either SomeException () ->
  IO Stack
encodeExn stk (Right _) = stk <$ poke stk 1
encodeExn stk (Left exn) = do
  upoke stk 0
  -- TODO: ALERT: something funky going on here,
  -- we seem to allocate only 2 slots, but write to 3
  stk <- bumpn stk 2
  bpoke stk $ Foreign (Wrap Rf.typeLinkRef link)
  pokeOffBi stk 1 msg
  stk <$ bpokeOff stk 2 extra
  where
    disp e = Util.Text.pack $ show e
    (link, msg, extra)
      | Just (ioe :: IOException) <- fromException exn =
          (Rf.ioFailureRef, disp ioe, unitValue)
      | Just re <- fromException exn = case re of
          PE _stk msg ->
            (Rf.runtimeFailureRef, Util.Text.pack $ toPlainUnbroken msg, unitValue)
          BU _ tx cl -> (Rf.runtimeFailureRef, Util.Text.fromText tx, cl)
      | Just (ae :: ArithException) <- fromException exn =
          (Rf.arithmeticFailureRef, disp ae, unitValue)
      | Just (nae :: NestedAtomically) <- fromException exn =
          (Rf.stmFailureRef, disp nae, unitValue)
      | Just (be :: BlockedIndefinitelyOnSTM) <- fromException exn =
          (Rf.stmFailureRef, disp be, unitValue)
      | Just (be :: BlockedIndefinitelyOnMVar) <- fromException exn =
          (Rf.ioFailureRef, disp be, unitValue)
      | Just (ie :: AsyncException) <- fromException exn =
          (Rf.threadKilledFailureRef, disp ie, unitValue)
      | otherwise = (Rf.miscFailureRef, disp exn, unitValue)

numValue :: Maybe Reference -> Closure -> IO Word64
numValue _ (DataU1 _ _ i) = pure (fromIntegral i)
numValue mr clo =
  die $
    "numValue: bad closure: "
      ++ show clo
      ++ maybe "" (\r -> "\nexpected type: " ++ show r) mr

-- | Evaluate a section
eval ::
  CCache ->
  DEnv ->
  ActiveThreads ->
  Stack ->
  K ->
  Reference ->
  MSection ->
  IO ()
eval !env !denv !activeThreads !stk !k r (Match i (TestT df cs)) = do
  t <- peekOffBi stk i
  eval env denv activeThreads stk k r $ selectTextBranch t df cs
eval !env !denv !activeThreads !stk !k r (Match i br) = do
  n <- peekOffN stk i
  eval env denv activeThreads stk k r $ selectBranch n br
eval !env !denv !activeThreads !stk !k r (DMatch mr i br) = do
  (t, stk) <- dumpDataNoTag mr stk =<< bpeekOff stk i
  eval env denv activeThreads stk k r $
    selectBranch (maskTags t) br
eval !env !denv !activeThreads !stk !k r (NMatch mr i br) = do
  n <- numValue mr =<< bpeekOff stk i
  eval env denv activeThreads stk k r $ selectBranch n br
eval !env !denv !activeThreads !stk !k r (RMatch i pu br) = do
  (t, stk) <- dumpDataNoTag Nothing stk =<< bpeekOff stk i
  if t == 0
    then eval env denv activeThreads stk k r pu
    else case ANF.unpackTags t of
      (ANF.rawTag -> e, ANF.rawTag -> t)
        | Just ebs <- EC.lookup e br ->
            eval env denv activeThreads stk k r $ selectBranch t ebs
        | otherwise -> unhandledErr "eval" env e
eval !env !denv !activeThreads !stk !k _ (Yield args)
  | asize stk > 0,
    VArg1 i <- args =
      bpeekOff stk i >>= apply env denv activeThreads stk k False ZArgs
  | otherwise = do
      stk <- moveArgs stk args
      stk <- frameArgs stk
      yield env denv activeThreads stk k
eval !env !denv !activeThreads !stk !k _ (App ck r args) =
  resolve env denv stk r
    >>= apply env denv activeThreads stk k ck args
eval !env !denv !activeThreads !stk !k _ (Call ck _combIx rcomb args) =
  enter env denv activeThreads stk k ck args rcomb
eval !env !denv !activeThreads !stk !k _ (Jump i args) =
  bpeekOff stk i >>= jump env denv activeThreads stk k args
eval !env !denv !activeThreads !stk !k r (Let nw cix f sect) = do
  (stk, fsz, asz) <- saveFrame stk
  eval
    env
    denv
    activeThreads
    ustk
    bstk
    (Push fsz asz cix f sect k)
    r
    nw
eval !env !denv !activeThreads !stk !k r (Ins i nx) = do
  (denv, stk, k) <- exec env denv activeThreads ustk bstk k r i
  eval env denv activeThreads ustk bstk k r nx
eval !_ !_ !_ !_activeThreads !_ _ Exit = pure ()
eval !_ !_ !_ !_activeThreads !_ _ (Die s) = die s
{-# NOINLINE eval #-}

forkEval :: CCache -> ActiveThreads -> Closure -> IO ThreadId
forkEval env activeThreads clo =
  do
    threadId <-
      UnliftIO.forkFinally
        (apply1 err env activeThreads clo)
        (const cleanupThread)
    trackThread threadId
    pure threadId
  where
    err :: Stack -> IO ()
    err _ = pure ()
    trackThread :: ThreadId -> IO ()
    trackThread threadID = do
      case activeThreads of
        Nothing -> pure ()
        Just activeThreads -> UnliftIO.atomicModifyIORef' activeThreads (\ids -> (Set.insert threadID ids, ()))
    cleanupThread :: IO ()
    cleanupThread = do
      case activeThreads of
        Nothing -> pure ()
        Just activeThreads -> do
          myThreadId <- UnliftIO.myThreadId
          UnliftIO.atomicModifyIORef' activeThreads (\ids -> (Set.delete myThreadId ids, ()))
{-# INLINE forkEval #-}

nestEval :: CCache -> ActiveThreads -> (Closure -> IO ()) -> Closure -> IO ()
nestEval env activeThreads write clo = apply1 readBack env activeThreads clo
  where
    readBack stk = bpeek stk >>= write
{-# INLINE nestEval #-}

atomicEval :: CCache -> ActiveThreads -> (Closure -> IO ()) -> Closure -> IO ()
atomicEval env activeThreads write clo =
  atomically . unsafeIOToSTM $ nestEval env activeThreads write clo
{-# INLINE atomicEval #-}

-- fast path application
enter ::
  CCache ->
  DEnv ->
  ActiveThreads ->
  Stack ->
  K ->
  Bool ->
  Args ->
  MComb ->
  IO ()
enter !env !denv !activeThreads !stk !k !ck !args = \case
  (RComb (Lam ua ba uf bf entry)) -> do
    ustk <- if ck then ensure ustk uf else pure ustk
    bstk <- if ck then ensure bstk bf else pure bstk
    (ustk, bstk) <- moveArgs ustk bstk args
    ustk <- acceptArgs ustk ua
    bstk <- acceptArgs bstk ba
    -- TODO: start putting references in `Call` if we ever start
    -- detecting saturated calls.
    eval env denv activeThreads ustk bstk k dummyRef entry
  (RComb (CachedClosure _cix clos)) -> do
    ustk <- discardFrame ustk
    bstk <- discardFrame bstk
    bstk <- bump bstk
    poke bstk clos
    yield env denv activeThreads ustk bstk k
{-# INLINE enter #-}

-- fast path by-name delaying
name :: Stack -> Args -> Closure -> IO Stack
name !stk !args clo = case clo of
  PAp cix comb seg -> do
    (useg, bseg) <- closeArgs I ustk bstk useg bseg args
    bstk <- bump bstk
    poke bstk $ PAp cix comb useg bseg
    pure bstk
  _ -> die $ "naming non-function: " ++ show clo
{-# INLINE name #-}

-- slow path application
apply ::
  CCache ->
  DEnv ->
  ActiveThreads ->
  Stack ->
  K ->
  Bool ->
  Args ->
  Closure ->
  IO ()
apply !env !denv !activeThreads !stk !k !ck !args = \case
  (PAp cix@(CIx combRef _ _) comb seg) ->
    case unRComb comb of
      CachedClosure _cix clos -> do
        zeroArgClosure clos
      Lam ua ba uf bf entry
        | ck || ua <= uac && ba <= bac -> do
            ustk <- ensure ustk uf
            bstk <- ensure bstk bf
            (ustk, bstk) <- moveArgs ustk bstk args
            ustk <- dumpSeg ustk useg A
            bstk <- dumpSeg bstk bseg A
            ustk <- acceptArgs ustk ua
            bstk <- acceptArgs bstk ba
            eval env denv activeThreads ustk bstk k combRef entry
        | otherwise -> do
            (useg, bseg) <- closeArgs C ustk bstk useg bseg args
            ustk <- discardFrame =<< frameArgs ustk
            bstk <- discardFrame =<< frameArgs bstk
            bstk <- bump bstk
            poke bstk $ PAp cix comb useg bseg
            yield env denv activeThreads ustk bstk k
    where
      uac = asize ustk + ucount args + uscount useg
      bac = asize bstk + bcount args + bscount bseg
  clo -> zeroArgClosure clo
  where
    zeroArgClosure clo
      | ZArgs <- args,
        asize ustk == 0,
        asize bstk == 0 = do
          ustk <- discardFrame ustk
          bstk <- discardFrame bstk
          bstk <- bump bstk
          poke bstk clo
          yield env denv activeThreads ustk bstk k
      | otherwise = die $ "applying non-function: " ++ show clo
{-# INLINE apply #-}

jump ::
  CCache ->
  DEnv ->
  ActiveThreads ->
  Stack ->
  K ->
  Args ->
  Closure ->
  IO ()
jump !env !denv !activeThreads !stk !k !args clo = case clo of
  Captured sk0 ua ba seg -> do
    let (up, bp, sk) = adjust sk0
    (useg, bseg) <- closeArgs K ustk bstk useg bseg args
    ustk <- discardFrame ustk
    bstk <- discardFrame bstk
    ustk <- dumpSeg ustk useg $ F (ucount args) ua
    bstk <- dumpSeg bstk bseg $ F (bcount args) ba
    ustk <- adjustArgs ustk up
    bstk <- adjustArgs bstk bp
    repush env activeThreads ustk bstk denv sk k
  _ -> die "jump: non-cont"
  where
    -- Adjusts a repushed continuation to account for pending arguments. If
    -- there are any frames in the pushed continuation, the nearest one needs to
    -- record the additional pending arguments.
    --
    -- If the repushed continuation has no frames, then the arguments are still
    -- pending, and the result stacks need to be adjusted. Hence the 3 results.
    adjust (Mark a rs denv k) =
      (0, Mark (a + asize stk) rs denv k)
    adjust (Push n a cix f rsect k) =
      (0, Push n (a + asize stk) cix f rsect k)
    adjust k = (asize stk, k)
{-# INLINE jump #-}

repush ::
  CCache ->
  ActiveThreads ->
  Stack ->
  DEnv ->
  K ->
  K ->
  IO ()
repush !env !activeThreads !stk = go
  where
    go !denv KE !k = yield env denv activeThreads ustk bstk k
    go !denv (Mark a ps cs sk) !k = go denv' sk $ Mark ua ba ps cs' k
      where
        denv' = cs <> EC.withoutKeys denv ps
        cs' = EC.restrictKeys denv ps
    go !denv (Push n a cix f rsect sk) !k =
      go denv sk $ Push n a cix f rsect k
    go !_ (CB _) !_ = die "repush: impossible"
{-# INLINE repush #-}

moveArgs ::
  Stack ->
  Args ->
  IO Stack
moveArgs !stk ZArgs = do
  ustk <- discardFrame ustk
  bstk <- discardFrame bstk
  pure (ustk, bstk)
moveArgs !stk (VArgV i j) = do
  ustk <-
    if ul > 0
      then prepareArgs ustk (ArgR 0 ul)
      else discardFrame ustk
  bstk <-
    if bl > 0
      then prepareArgs bstk (ArgR 0 bl)
      else discardFrame bstk
  pure (ustk, bstk)
  where
    ul = fsize ustk - i
    bl = fsize bstk - j
moveArgs !stk (VArg1 i) = do
  ustk <- prepareArgs ustk (Arg1 i)
  bstk <- discardFrame bstk
  pure (ustk, bstk)
moveArgs !stk (VArg2 i j) = do
  ustk <- prepareArgs ustk (Arg2 i j)
  bstk <- discardFrame bstk
  pure (ustk, bstk)
moveArgs !stk (VArgR i l) = do
  ustk <- prepareArgs ustk (ArgR i l)
  bstk <- discardFrame bstk
  pure (ustk, bstk)
moveArgs !stk (VArg1 i) = do
  ustk <- discardFrame ustk
  bstk <- prepareArgs bstk (Arg1 i)
  pure (ustk, bstk)
moveArgs !stk (VArg2 i j) = do
  ustk <- discardFrame ustk
  bstk <- prepareArgs bstk (Arg2 i j)
  pure (ustk, bstk)
moveArgs !stk (VArgR i l) = do
  ustk <- discardFrame ustk
  bstk <- prepareArgs bstk (ArgR i l)
  pure (ustk, bstk)
moveArgs !stk (VArg2 i j) = do
  ustk <- prepareArgs ustk (Arg1 i)
  bstk <- prepareArgs bstk (Arg1 j)
  pure (ustk, bstk)
moveArgs !stk (VArgN as) = do
  ustk <- prepareArgs ustk (ArgN as)
  bstk <- discardFrame bstk
  pure (ustk, bstk)
moveArgs !stk (VArgN as) = do
  ustk <- discardFrame ustk
  bstk <- prepareArgs bstk (ArgN as)
  pure (ustk, bstk)
moveArgs !stk (VArgN as) = do
  ustk <- prepareArgs ustk (ArgN us)
  bstk <- prepareArgs bstk (ArgN bs)
  pure (ustk, bstk)
{-# INLINE moveArgs #-}

closureArgs :: Stack -> Args -> IO [Closure]
closureArgs !_ ZArgs = pure []
closureArgs !stk (VArg1 i) = do
  x <- bpeekOff stk i
  pure [x]
closureArgs !stk (VArg2 i j) = do
  x <- bpeekOff stk i
  y <- bpeekOff stk j
  pure [x, y]
closureArgs !stk (VArgR i l) =
  for (take l [i ..]) (bpeekOff stk)
closureArgs !stk (VArgN bs) =
  for (PA.primArrayToList bs) (bpeekOff stk)
closureArgs !_ _ =
  error "closure arguments can only be boxed."
{-# INLINE closureArgs #-}

buildData ::
  Stack -> Reference -> Tag -> Args -> IO Closure
buildData !_ !r !t ZArgs = pure $ Enum r t
buildData !stk !r !t (VArg1 i) = do
  x <- peekOff ustk i
  pure $ DataU1 r t x
buildData !stk !r !t (VArg2 i j) = do
  x <- peekOff ustk i
  y <- peekOff ustk j
  pure $ DataU2 r t x y
buildData !stk !r !t (VArg1 i) = do
  x <- peekOff bstk i
  pure $ DataB1 r t x
buildData !stk !r !t (VArg2 i j) = do
  x <- peekOff bstk i
  y <- peekOff bstk j
  pure $ DataB2 r t x y
buildData !stk !r !t (VArg2 i j) = do
  x <- peekOff ustk i
  y <- peekOff bstk j
  pure $ DataUB r t x y
buildData !stk !r !t (VArgR i l) = do
  useg <- augSeg I ustk unull (Just $ ArgR i l)
  pure $ DataG r t useg bnull
buildData !stk !r !t (VArgR i l) = do
  bseg <- augSeg I bstk bnull (Just $ ArgR i l)
  pure $ DataG r t unull bseg
buildData !stk !r !t (VArgN as) = do
  useg <- augSeg I ustk unull (Just $ ArgN as)
  pure $ DataG r t useg bnull
buildData !stk !r !t (VArgN as) = do
  useg <- augSeg I ustk unull (Just $ ArgN us)
  bseg <- augSeg I bstk bnull (Just $ ArgN bs)
  pure $ DataG r t useg bseg
buildData !stk !r !t (VArgV ui bi) = do
  useg <-
    if ul > 0
      then augSeg I ustk unull (Just $ ArgR 0 ul)
      else pure unull
  bseg <-
    if bl > 0
      then augSeg I bstk bnull (Just $ ArgR 0 bl)
      else pure bnull
  pure $ DataG r t useg bseg
  where
    ul = fsize ustk - ui
    bl = fsize bstk - bi
{-# INLINE buildData #-}

-- Dumps a data type closure to the stack without writing its tag.
-- Instead, the tag is returned for direct case analysis.
dumpDataNoTag ::
  Maybe Reference ->
  Stack ->
  Closure ->
  IO (Word64, Stack)
dumpDataNoTag !_ !stk (Enum _ t) = pure (t, ustk, bstk)
dumpDataNoTag !_ !stk (DataU1 _ t x) = do
  ustk <- bump ustk
  poke ustk x
  pure (t, ustk, bstk)
dumpDataNoTag !_ !stk (DataU2 _ t x y) = do
  ustk <- bumpn ustk 2
  pokeOff ustk 1 y
  poke ustk x
  pure (t, ustk, bstk)
dumpDataNoTag !_ !stk (DataB1 _ t x) = do
  bstk <- bump bstk
  poke bstk x
  pure (t, ustk, bstk)
dumpDataNoTag !_ !stk (DataB2 _ t x y) = do
  bstk <- bumpn bstk 2
  pokeOff bstk 1 y
  poke bstk x
  pure (t, ustk, bstk)
dumpDataNoTag !_ !stk (DataUB _ t x y) = do
  ustk <- bump ustk
  bstk <- bump bstk
  poke ustk x
  poke bstk y
  pure (t, ustk, bstk)
dumpDataNoTag !_ !stk (DataG _ t seg) = do
  ustk <- dumpSeg ustk us S
  bstk <- dumpSeg bstk bs S
  pure (t, ustk, bstk)
dumpDataNoTag !mr !_ clo =
  die $
    "dumpDataNoTag: bad closure: "
      ++ show clo
      ++ maybe "" (\r -> "\nexpected type: " ++ show r) mr
{-# INLINE dumpDataNoTag #-}

dumpData ::
  Maybe Reference ->
  Stack ->
  Closure ->
  IO Stack
dumpData !_ !stk (Enum _ t) = do
  ustk <- bump ustk
  pokeN ustk $ maskTags t
  pure (ustk, bstk)
dumpData !_ !stk (DataU1 _ t x) = do
  ustk <- bumpn ustk 2
  pokeOff ustk 1 x
  pokeN ustk $ maskTags t
  pure (ustk, bstk)
dumpData !_ !stk (DataU2 _ t x y) = do
  ustk <- bumpn ustk 3
  pokeOff ustk 2 y
  pokeOff ustk 1 x
  pokeN ustk $ maskTags t
  pure (ustk, bstk)
dumpData !_ !stk (DataB1 _ t x) = do
  ustk <- bump ustk
  bstk <- bump bstk
  poke bstk x
  pokeN ustk $ maskTags t
  pure (ustk, bstk)
dumpData !_ !stk (DataB2 _ t x y) = do
  ustk <- bump ustk
  bstk <- bumpn bstk 2
  pokeOff bstk 1 y
  poke bstk x
  pokeN ustk $ maskTags t
  pure (ustk, bstk)
dumpData !_ !stk (DataUB _ t x y) = do
  ustk <- bumpn ustk 2
  bstk <- bump bstk
  pokeOff ustk 1 x
  poke bstk y
  pokeN ustk $ maskTags t
  pure (ustk, bstk)
dumpData !_ !stk (DataG _ t seg) = do
  ustk <- dumpSeg ustk us S
  bstk <- dumpSeg bstk bs S
  ustk <- bump ustk
  pokeN ustk $ maskTags t
  pure (ustk, bstk)
dumpData !mr !_ clo =
  die $
    "dumpData: bad closure: "
      ++ show clo
      ++ maybe "" (\r -> "\nexpected type: " ++ show r) mr
{-# INLINE dumpData #-}

-- Note: although the representation allows it, it is impossible
-- to under-apply one sort of argument while over-applying the
-- other. Thus, it is unnecessary to worry about doing tricks to
-- only grab a certain number of arguments.
closeArgs ::
  Augment ->
  Stack ->
  Seg ->
  Args ->
  IO Seg
closeArgs mode !stk !seg args = augSeg mode stk seg args
  where
    (uargs, bargs) = case args of
      -- TODO:
      ZArgs -> (Nothing, Nothing)
      VArg1 i -> (Just $ Arg1 i, Nothing)
      VArg1 i -> (Nothing, Just $ Arg1 i)
      VArg2 i j -> (Just $ Arg2 i j, Nothing)
      VArg2 i j -> (Nothing, Just $ Arg2 i j)
      VArgR i l -> (Just $ ArgR i l, Nothing)
      VArgR i l -> (Nothing, Just $ ArgR i l)
      VArg2 i j -> (Just $ Arg1 i, Just $ Arg1 j)
      VArgN as -> (Just $ ArgN as, Nothing)
      VArgN as -> (Nothing, Just $ ArgN as)
      VArgV ui bi -> (ua, ba)
        where
          ua
            | ul > 0 = Just $ ArgR 0 ul
            | otherwise = Nothing
          ba
            | bl > 0 = Just $ ArgR 0 bl
            | otherwise = Nothing
          ul = fsize ustk - ui
          bl = fsize bstk - bi

peekForeign :: Stack -> Int -> IO a
peekForeign bstk i =
  bpeekOff bstk i >>= \case
    Foreign x -> pure $ unwrapForeign x
    _ -> die "bad foreign argument"
{-# INLINE peekForeign #-}

uprim1 :: Stack -> UPrim1 -> Int -> IO Stack
uprim1 !ustk DECI !i = do
  m <- peekOff ustk i
  ustk <- bump ustk
  poke ustk (m - 1)
  pure ustk
uprim1 !ustk INCI !i = do
  m <- peekOff ustk i
  ustk <- bump ustk
  poke ustk (m + 1)
  pure ustk
uprim1 !ustk NEGI !i = do
  m <- peekOff ustk i
  ustk <- bump ustk
  poke ustk (-m)
  pure ustk
uprim1 !ustk SGNI !i = do
  m <- peekOff ustk i
  ustk <- bump ustk
  poke ustk (signum m)
  pure ustk
uprim1 !ustk ABSF !i = do
  d <- peekOffD ustk i
  ustk <- bump ustk
  pokeD ustk (abs d)
  pure ustk
uprim1 !ustk CEIL !i = do
  d <- peekOffD ustk i
  ustk <- bump ustk
  poke ustk (ceiling d)
  pure ustk
uprim1 !ustk FLOR !i = do
  d <- peekOffD ustk i
  ustk <- bump ustk
  poke ustk (floor d)
  pure ustk
uprim1 !ustk TRNF !i = do
  d <- peekOffD ustk i
  ustk <- bump ustk
  poke ustk (truncate d)
  pure ustk
uprim1 !ustk RNDF !i = do
  d <- peekOffD ustk i
  ustk <- bump ustk
  poke ustk (round d)
  pure ustk
uprim1 !ustk EXPF !i = do
  d <- peekOffD ustk i
  ustk <- bump ustk
  pokeD ustk (exp d)
  pure ustk
uprim1 !ustk LOGF !i = do
  d <- peekOffD ustk i
  ustk <- bump ustk
  pokeD ustk (log d)
  pure ustk
uprim1 !ustk SQRT !i = do
  d <- peekOffD ustk i
  ustk <- bump ustk
  pokeD ustk (sqrt d)
  pure ustk
uprim1 !ustk COSF !i = do
  d <- peekOffD ustk i
  ustk <- bump ustk
  pokeD ustk (cos d)
  pure ustk
uprim1 !ustk SINF !i = do
  d <- peekOffD ustk i
  ustk <- bump ustk
  pokeD ustk (sin d)
  pure ustk
uprim1 !ustk TANF !i = do
  d <- peekOffD ustk i
  ustk <- bump ustk
  pokeD ustk (tan d)
  pure ustk
uprim1 !ustk COSH !i = do
  d <- peekOffD ustk i
  ustk <- bump ustk
  pokeD ustk (cosh d)
  pure ustk
uprim1 !ustk SINH !i = do
  d <- peekOffD ustk i
  ustk <- bump ustk
  pokeD ustk (sinh d)
  pure ustk
uprim1 !ustk TANH !i = do
  d <- peekOffD ustk i
  ustk <- bump ustk
  pokeD ustk (tanh d)
  pure ustk
uprim1 !ustk ACOS !i = do
  d <- peekOffD ustk i
  ustk <- bump ustk
  pokeD ustk (acos d)
  pure ustk
uprim1 !ustk ASIN !i = do
  d <- peekOffD ustk i
  ustk <- bump ustk
  pokeD ustk (asin d)
  pure ustk
uprim1 !ustk ATAN !i = do
  d <- peekOffD ustk i
  ustk <- bump ustk
  pokeD ustk (atan d)
  pure ustk
uprim1 !ustk ASNH !i = do
  d <- peekOffD ustk i
  ustk <- bump ustk
  pokeD ustk (asinh d)
  pure ustk
uprim1 !ustk ACSH !i = do
  d <- peekOffD ustk i
  ustk <- bump ustk
  pokeD ustk (acosh d)
  pure ustk
uprim1 !ustk ATNH !i = do
  d <- peekOffD ustk i
  ustk <- bump ustk
  pokeD ustk (atanh d)
  pure ustk
uprim1 !ustk ITOF !i = do
  n <- peekOff ustk i
  ustk <- bump ustk
  pokeD ustk (fromIntegral n)
  pure ustk
uprim1 !ustk NTOF !i = do
  n <- peekOffN ustk i
  ustk <- bump ustk
  pokeD ustk (fromIntegral n)
  pure ustk
uprim1 !ustk LZRO !i = do
  n <- peekOffN ustk i
  ustk <- bump ustk
  poke ustk (countLeadingZeros n)
  pure ustk
uprim1 !ustk TZRO !i = do
  n <- peekOffN ustk i
  ustk <- bump ustk
  poke ustk (countTrailingZeros n)
  pure ustk
uprim1 !ustk POPC !i = do
  n <- peekOffN ustk i
  ustk <- bump ustk
  poke ustk (popCount n)
  pure ustk
uprim1 !ustk COMN !i = do
  n <- peekOffN ustk i
  ustk <- bump ustk
  pokeN ustk (complement n)
  pure ustk
{-# INLINE uprim1 #-}

uprim2 :: Stack -> UPrim2 -> Int -> Int -> IO Stack
uprim2 !ustk ADDI !i !j = do
  m <- peekOff ustk i
  n <- peekOff ustk j
  ustk <- bump ustk
  poke ustk (m + n)
  pure ustk
uprim2 !ustk SUBI !i !j = do
  m <- peekOff ustk i
  n <- peekOff ustk j
  ustk <- bump ustk
  poke ustk (m - n)
  pure ustk
uprim2 !ustk MULI !i !j = do
  m <- peekOff ustk i
  n <- peekOff ustk j
  ustk <- bump ustk
  poke ustk (m * n)
  pure ustk
uprim2 !ustk DIVI !i !j = do
  m <- peekOff ustk i
  n <- peekOff ustk j
  ustk <- bump ustk
  poke ustk (m `div` n)
  pure ustk
uprim2 !ustk MODI !i !j = do
  m <- peekOff ustk i
  n <- peekOff ustk j
  ustk <- bump ustk
  poke ustk (m `mod` n)
  pure ustk
uprim2 !ustk SHLI !i !j = do
  m <- peekOff ustk i
  n <- peekOff ustk j
  ustk <- bump ustk
  poke ustk (m `shiftL` n)
  pure ustk
uprim2 !ustk SHRI !i !j = do
  m <- peekOff ustk i
  n <- peekOff ustk j
  ustk <- bump ustk
  poke ustk (m `shiftR` n)
  pure ustk
uprim2 !ustk SHRN !i !j = do
  m <- peekOffN ustk i
  n <- peekOff ustk j
  ustk <- bump ustk
  pokeN ustk (m `shiftR` n)
  pure ustk
uprim2 !ustk POWI !i !j = do
  m <- peekOff ustk i
  n <- peekOffN ustk j
  ustk <- bump ustk
  poke ustk (m ^ n)
  pure ustk
uprim2 !ustk EQLI !i !j = do
  m <- peekOff ustk i
  n <- peekOff ustk j
  ustk <- bump ustk
  poke ustk $ if m == n then 1 else 0
  pure ustk
uprim2 !ustk LEQI !i !j = do
  m <- peekOff ustk i
  n <- peekOff ustk j
  ustk <- bump ustk
  poke ustk $ if m <= n then 1 else 0
  pure ustk
uprim2 !ustk LEQN !i !j = do
  m <- peekOffN ustk i
  n <- peekOffN ustk j
  ustk <- bump ustk
  poke ustk $ if m <= n then 1 else 0
  pure ustk
uprim2 !ustk DIVN !i !j = do
  m <- peekOffN ustk i
  n <- peekOffN ustk j
  ustk <- bump ustk
  pokeN ustk (m `div` n)
  pure ustk
uprim2 !ustk MODN !i !j = do
  m <- peekOffN ustk i
  n <- peekOffN ustk j
  ustk <- bump ustk
  pokeN ustk (m `mod` n)
  pure ustk
uprim2 !ustk ADDF !i !j = do
  x <- peekOffD ustk i
  y <- peekOffD ustk j
  ustk <- bump ustk
  pokeD ustk (x + y)
  pure ustk
uprim2 !ustk SUBF !i !j = do
  x <- peekOffD ustk i
  y <- peekOffD ustk j
  ustk <- bump ustk
  pokeD ustk (x - y)
  pure ustk
uprim2 !ustk MULF !i !j = do
  x <- peekOffD ustk i
  y <- peekOffD ustk j
  ustk <- bump ustk
  pokeD ustk (x * y)
  pure ustk
uprim2 !ustk DIVF !i !j = do
  x <- peekOffD ustk i
  y <- peekOffD ustk j
  ustk <- bump ustk
  pokeD ustk (x / y)
  pure ustk
uprim2 !ustk LOGB !i !j = do
  x <- peekOffD ustk i
  y <- peekOffD ustk j
  ustk <- bump ustk
  pokeD ustk (logBase x y)
  pure ustk
uprim2 !ustk POWF !i !j = do
  x <- peekOffD ustk i
  y <- peekOffD ustk j
  ustk <- bump ustk
  pokeD ustk (x ** y)
  pure ustk
uprim2 !ustk MAXF !i !j = do
  x <- peekOffD ustk i
  y <- peekOffD ustk j
  ustk <- bump ustk
  pokeD ustk (max x y)
  pure ustk
uprim2 !ustk MINF !i !j = do
  x <- peekOffD ustk i
  y <- peekOffD ustk j
  ustk <- bump ustk
  pokeD ustk (min x y)
  pure ustk
uprim2 !ustk EQLF !i !j = do
  x <- peekOffD ustk i
  y <- peekOffD ustk j
  ustk <- bump ustk
  poke ustk (if x == y then 1 else 0)
  pure ustk
uprim2 !ustk LEQF !i !j = do
  x <- peekOffD ustk i
  y <- peekOffD ustk j
  ustk <- bump ustk
  poke ustk (if x <= y then 1 else 0)
  pure ustk
uprim2 !ustk ATN2 !i !j = do
  x <- peekOffD ustk i
  y <- peekOffD ustk j
  ustk <- bump ustk
  pokeD ustk (atan2 x y)
  pure ustk
uprim2 !ustk ANDN !i !j = do
  x <- peekOffN ustk i
  y <- peekOffN ustk j
  ustk <- bump ustk
  pokeN ustk (x .&. y)
  pure ustk
uprim2 !ustk IORN !i !j = do
  x <- peekOffN ustk i
  y <- peekOffN ustk j
  ustk <- bump ustk
  pokeN ustk (x .|. y)
  pure ustk
uprim2 !ustk XORN !i !j = do
  x <- peekOffN ustk i
  y <- peekOffN ustk j
  ustk <- bump ustk
  pokeN ustk (xor x y)
  pure ustk
{-# INLINE uprim2 #-}

bprim1 ::
  Stack ->
  BPrim1 ->
  Int ->
  IO Stack
bprim1 !stk SIZT i = do
  t <- peekOffBi bstk i
  ustk <- bump ustk
  poke ustk $ Util.Text.size t
  pure (ustk, bstk)
bprim1 !stk SIZS i = do
  s <- peekOffS bstk i
  ustk <- bump ustk
  poke ustk $ Sq.length s
  pure (ustk, bstk)
bprim1 !stk ITOT i = do
  n <- peekOff ustk i
  bstk <- bump bstk
  pokeBi bstk . Util.Text.pack $ show n
  pure (ustk, bstk)
bprim1 !stk NTOT i = do
  n <- peekOffN ustk i
  bstk <- bump bstk
  pokeBi bstk . Util.Text.pack $ show n
  pure (ustk, bstk)
bprim1 !stk FTOT i = do
  f <- peekOffD ustk i
  bstk <- bump bstk
  pokeBi bstk . Util.Text.pack $ show f
  pure (ustk, bstk)
bprim1 !stk USNC i =
  peekOffBi bstk i >>= \t -> case Util.Text.unsnoc t of
    Nothing -> do
      ustk <- bump ustk
      poke ustk 0
      pure (ustk, bstk)
    Just (t, c) -> do
      ustk <- bumpn ustk 2
      bstk <- bump bstk
      pokeOff ustk 1 $ fromEnum c
      poke ustk 1
      pokeBi bstk t
      pure (ustk, bstk)
bprim1 !stk UCNS i =
  peekOffBi bstk i >>= \t -> case Util.Text.uncons t of
    Nothing -> do
      ustk <- bump ustk
      poke ustk 0
      pure (ustk, bstk)
    Just (c, t) -> do
      ustk <- bumpn ustk 2
      bstk <- bump bstk
      pokeOff ustk 1 $ fromEnum c
      poke ustk 1
      pokeBi bstk t
      pure (ustk, bstk)
bprim1 !stk TTOI i =
  peekOffBi bstk i >>= \t -> case readm $ Util.Text.unpack t of
    Just n
      | fromIntegral (minBound :: Int) <= n,
        n <= fromIntegral (maxBound :: Int) -> do
          ustk <- bumpn ustk 2
          poke ustk 1
          pokeOff ustk 1 (fromInteger n)
          pure (ustk, bstk)
    _ -> do
      ustk <- bump ustk
      poke ustk 0
      pure (ustk, bstk)
  where
    readm ('+' : s) = readMaybe s
    readm s = readMaybe s
bprim1 !stk TTON i =
  peekOffBi bstk i >>= \t -> case readMaybe $ Util.Text.unpack t of
    Just n
      | 0 <= n,
        n <= fromIntegral (maxBound :: Word) -> do
          ustk <- bumpn ustk 2
          poke ustk 1
          pokeOffN ustk 1 (fromInteger n)
          pure (ustk, bstk)
    _ -> do
      ustk <- bump ustk
      poke ustk 0
      pure (ustk, bstk)
bprim1 !stk TTOF i =
  peekOffBi bstk i >>= \t -> case readMaybe $ Util.Text.unpack t of
    Nothing -> do
      ustk <- bump ustk
      poke ustk 0
      pure (ustk, bstk)
    Just f -> do
      ustk <- bumpn ustk 2
      poke ustk 1
      pokeOffD ustk 1 f
      pure (ustk, bstk)
bprim1 !stk VWLS i =
  peekOffS bstk i >>= \case
    Sq.Empty -> do
      ustk <- bump ustk
      poke ustk 0
      pure (ustk, bstk)
    x Sq.:<| xs -> do
      ustk <- bump ustk
      poke ustk 1
      bstk <- bumpn bstk 2
      pokeOffS bstk 1 xs
      poke bstk x
      pure (ustk, bstk)
bprim1 !stk VWRS i =
  peekOffS bstk i >>= \case
    Sq.Empty -> do
      ustk <- bump ustk
      poke ustk 0
      pure (ustk, bstk)
    xs Sq.:|> x -> do
      ustk <- bump ustk
      poke ustk 1
      bstk <- bumpn bstk 2
      pokeOff bstk 1 x
      pokeS bstk xs
      pure (ustk, bstk)
bprim1 !stk PAKT i = do
  s <- peekOffS bstk i
  bstk <- bump bstk
  pokeBi bstk . Util.Text.pack . toList $ clo2char <$> s
  pure (ustk, bstk)
  where
    clo2char (DataU1 _ t i) | t == charTag = toEnum i
    clo2char c = error $ "pack text: non-character closure: " ++ show c
bprim1 !stk UPKT i = do
  t <- peekOffBi bstk i
  bstk <- bump bstk
  pokeS bstk
    . Sq.fromList
    . fmap (DataU1 Rf.charRef charTag . fromEnum)
    . Util.Text.unpack
    $ t
  pure (ustk, bstk)
bprim1 !stk PAKB i = do
  s <- peekOffS bstk i
  bstk <- bump bstk
  pokeBi bstk . By.fromWord8s . fmap clo2w8 $ toList s
  pure (ustk, bstk)
  where
    clo2w8 (DataU1 _ t n) | t == natTag = toEnum n
    clo2w8 c = error $ "pack bytes: non-natural closure: " ++ show c
bprim1 !stk UPKB i = do
  b <- peekOffBi bstk i
  bstk <- bump bstk
  pokeS bstk . Sq.fromList . fmap (DataU1 Rf.natRef natTag . fromEnum) $
    By.toWord8s b
  pure (ustk, bstk)
bprim1 !stk SIZB i = do
  b <- peekOffBi bstk i
  ustk <- bump ustk
  poke ustk $ By.size b
  pure (ustk, bstk)
bprim1 !stk FLTB i = do
  b <- peekOffBi bstk i
  bstk <- bump bstk
  pokeBi bstk $ By.flatten b
  pure (ustk, bstk)
-- impossible
bprim1 !stk MISS _ = pure (ustk, bstk)
bprim1 !stk CACH _ = pure (ustk, bstk)
bprim1 !stk LKUP _ = pure (ustk, bstk)
bprim1 !stk CVLD _ = pure (ustk, bstk)
bprim1 !stk TLTT _ = pure (ustk, bstk)
bprim1 !stk LOAD _ = pure (ustk, bstk)
bprim1 !stk VALU _ = pure (ustk, bstk)
bprim1 !stk DBTX _ = pure (ustk, bstk)
bprim1 !stk SDBL _ = pure (ustk, bstk)
{-# INLINE bprim1 #-}

bprim2 ::
  Stack ->
  BPrim2 ->
  Int ->
  Int ->
  IO Stack
bprim2 !stk EQLU i j = do
  x <- peekOff bstk i
  y <- peekOff bstk j
  ustk <- bump ustk
  poke ustk $ if universalEq (==) x y then 1 else 0
  pure (ustk, bstk)
bprim2 !stk IXOT i j = do
  x <- peekOffBi bstk i
  y <- peekOffBi bstk j
  case Util.Text.indexOf x y of
    Nothing -> do
      ustk <- bump ustk
      poke ustk 0
      pure (ustk, bstk)
    Just i -> do
      ustk <- bumpn ustk 2
      poke ustk 1
      pokeOffN ustk 1 i
      pure (ustk, bstk)
bprim2 !stk IXOB i j = do
  x <- peekOffBi bstk i
  y <- peekOffBi bstk j
  case By.indexOf x y of
    Nothing -> do
      ustk <- bump ustk
      poke ustk 0
      pure (ustk, bstk)
    Just i -> do
      ustk <- bumpn ustk 2
      poke ustk 1
      pokeOffN ustk 1 i
      pure (ustk, bstk)
bprim2 !stk DRPT i j = do
  n <- peekOff ustk i
  t <- peekOffBi bstk j
  bstk <- bump bstk
  -- Note; if n < 0, the Nat argument was greater than the maximum
  -- signed integer. As an approximation, just return the empty
  -- string, as a string larger than this would require an absurd
  -- amount of memory.
  pokeBi bstk $ if n < 0 then Util.Text.empty else Util.Text.drop n t
  pure (ustk, bstk)
bprim2 !stk CATT i j = do
  x <- peekOffBi bstk i
  y <- peekOffBi bstk j
  bstk <- bump bstk
  pokeBi bstk $ (x <> y :: Util.Text.Text)
  pure (ustk, bstk)
bprim2 !stk TAKT i j = do
  n <- peekOff ustk i
  t <- peekOffBi bstk j
  bstk <- bump bstk
  -- Note: if n < 0, the Nat argument was greater than the maximum
  -- signed integer. As an approximation, we just return the original
  -- string, because it's unlikely such a large string exists.
  pokeBi bstk $ if n < 0 then t else Util.Text.take n t
  pure (ustk, bstk)
bprim2 !stk EQLT i j = do
  x <- peekOffBi @Util.Text.Text bstk i
  y <- peekOffBi bstk j
  ustk <- bump ustk
  poke ustk $ if x == y then 1 else 0
  pure (ustk, bstk)
bprim2 !stk LEQT i j = do
  x <- peekOffBi @Util.Text.Text bstk i
  y <- peekOffBi bstk j
  ustk <- bump ustk
  poke ustk $ if x <= y then 1 else 0
  pure (ustk, bstk)
bprim2 !stk LEST i j = do
  x <- peekOffBi @Util.Text.Text bstk i
  y <- peekOffBi bstk j
  ustk <- bump ustk
  poke ustk $ if x < y then 1 else 0
  pure (ustk, bstk)
bprim2 !stk DRPS i j = do
  n <- peekOff ustk i
  s <- peekOffS bstk j
  bstk <- bump bstk
  -- Note: if n < 0, then the Nat argument was larger than the largest
  -- signed integer. Seq actually doesn't handle this well, despite it
  -- being possible to build (lazy) sequences this large. So,
  -- approximate by yielding the empty sequence.
  pokeS bstk $ if n < 0 then Sq.empty else Sq.drop n s
  pure (ustk, bstk)
bprim2 !stk TAKS i j = do
  n <- peekOff ustk i
  s <- peekOffS bstk j
  bstk <- bump bstk
  -- Note: if n < 0, then the Nat argument was greater than the
  -- largest signed integer. It is possible to build such large
  -- sequences, but the internal size will actually be wrong then. So,
  -- we just return the original sequence as an approximation.
  pokeS bstk $ if n < 0 then s else Sq.take n s
  pure (ustk, bstk)
bprim2 !stk CONS i j = do
  x <- peekOff bstk i
  s <- peekOffS bstk j
  bstk <- bump bstk
  pokeS bstk $ x Sq.<| s
  pure (ustk, bstk)
bprim2 !stk SNOC i j = do
  s <- peekOffS bstk i
  x <- peekOff bstk j
  bstk <- bump bstk
  pokeS bstk $ s Sq.|> x
  pure (ustk, bstk)
bprim2 !stk CATS i j = do
  x <- peekOffS bstk i
  y <- peekOffS bstk j
  bstk <- bump bstk
  pokeS bstk $ x Sq.>< y
  pure (ustk, bstk)
bprim2 !stk IDXS i j = do
  n <- peekOff ustk i
  s <- peekOffS bstk j
  case Sq.lookup n s of
    Nothing -> do
      ustk <- bump ustk
      poke ustk 0
      pure (ustk, bstk)
    Just x -> do
      ustk <- bump ustk
      poke ustk 1
      bstk <- bump bstk
      poke bstk x
      pure (ustk, bstk)
bprim2 !stk SPLL i j = do
  n <- peekOff ustk i
  s <- peekOffS bstk j
  if Sq.length s < n
    then do
      ustk <- bump ustk
      poke ustk 0
      pure (ustk, bstk)
    else do
      ustk <- bump ustk
      poke ustk 1
      bstk <- bumpn bstk 2
      let (l, r) = Sq.splitAt n s
      pokeOffS bstk 1 r
      pokeS bstk l
      pure (ustk, bstk)
bprim2 !stk SPLR i j = do
  n <- peekOff ustk i
  s <- peekOffS bstk j
  if Sq.length s < n
    then do
      ustk <- bump ustk
      poke ustk 0
      pure (ustk, bstk)
    else do
      ustk <- bump ustk
      poke ustk 1
      bstk <- bumpn bstk 2
      let (l, r) = Sq.splitAt (Sq.length s - n) s
      pokeOffS bstk 1 r
      pokeS bstk l
      pure (ustk, bstk)
bprim2 !stk TAKB i j = do
  n <- peekOff ustk i
  b <- peekOffBi bstk j
  bstk <- bump bstk
  -- If n < 0, the Nat argument was larger than the maximum signed
  -- integer. Building a value this large would reuire an absurd
  -- amount of memory, so just assume n is larger.
  pokeBi bstk $ if n < 0 then b else By.take n b
  pure (ustk, bstk)
bprim2 !stk DRPB i j = do
  n <- peekOff ustk i
  b <- peekOffBi bstk j
  bstk <- bump bstk
  -- See above for n < 0
  pokeBi bstk $ if n < 0 then By.empty else By.drop n b
  pure (ustk, bstk)
bprim2 !stk IDXB i j = do
  n <- peekOff ustk i
  b <- peekOffBi bstk j
  ustk <- bump ustk
  ustk <- case By.at n b of
    Nothing -> ustk <$ poke ustk 0
    Just x -> do
      poke ustk $ fromIntegral x
      ustk <- bump ustk
      ustk <$ poke ustk 1
  pure (ustk, bstk)
bprim2 !stk CATB i j = do
  l <- peekOffBi bstk i
  r <- peekOffBi bstk j
  bstk <- bump bstk
  pokeBi bstk (l <> r :: By.Bytes)
  pure (ustk, bstk)
bprim2 !stk THRO _ _ = pure (ustk, bstk) -- impossible
bprim2 !stk TRCE _ _ = pure (ustk, bstk) -- impossible
bprim2 !stk CMPU _ _ = pure (ustk, bstk) -- impossible
bprim2 !stk SDBX _ _ = pure (ustk, bstk) -- impossible
bprim2 !stk SDBV _ _ = pure (ustk, bstk) -- impossible
{-# INLINE bprim2 #-}

yield ::
  CCache ->
  DEnv ->
  ActiveThreads ->
  Stack ->
  K ->
  IO ()
yield !env !denv !activeThreads !stk !k = leap denv k
  where
    leap !denv0 (Mark a ps cs k) = do
      let denv = cs <> EC.withoutKeys denv0 ps
          clo = denv0 EC.! EC.findMin ps
      poke bstk . DataB1 Rf.effectRef 0 =<< peek bstk
      ustk <- adjustArgs ustk ua
      bstk <- adjustArgs bstk ba
      apply env denv activeThreads ustk bstk k False (BArg1 0) clo
    leap !denv (Push fsz asz (CIx ref _ _) f nx k) = do
      ustk <- restoreFrame ustk ufsz uasz
      bstk <- restoreFrame bstk bfsz basz
      ustk <- ensure ustk uf
      bstk <- ensure bstk bf
      eval env denv activeThreads ustk bstk k ref nx
    leap _ (CB (Hook f)) = f ustk bstk
    leap _ KE = pure ()
{-# INLINE yield #-}

selectTextBranch ::
  Util.Text.Text -> MSection -> M.Map Util.Text.Text MSection -> MSection
selectTextBranch t df cs = M.findWithDefault df t cs
{-# INLINE selectTextBranch #-}

selectBranch :: Tag -> MBranch -> MSection
selectBranch t (Test1 u y n)
  | t == u = y
  | otherwise = n
selectBranch t (Test2 u cu v cv e)
  | t == u = cu
  | t == v = cv
  | otherwise = e
selectBranch t (TestW df cs) = lookupWithDefault df t cs
selectBranch _ (TestT {}) = error "impossible"
{-# INLINE selectBranch #-}

-- Splits off a portion of the continuation up to a given prompt.
--
-- The main procedure walks along the 'code' stack `k`, keeping track of how
-- many cells of the data stacks need to be captured. Then the `finish` function
-- performs the actual splitting of the data stacks together with some tweaking.
--
-- Some special attention is required for pending arguments for over-applied
-- functions. They are part of the continuation, so how many there are at the
-- time of capture is recorded in the `Captured` closure, so that information
-- can be restored later. Also, the `Mark` frame that is popped off as part of
-- this operation potentially exposes pending arguments beyond the delimited
-- region, so those are restored in the `finish` function.
splitCont ::
  DEnv ->
  Stack ->
  K ->
  Word64 ->
  IO (Closure, DEnv, Stack, K)
splitCont !denv !stk !k !p =
  walk denv asz KE k
  where
    asz = asize stk
    walk !denv !sz !ck KE =
      die "fell off stack" >> finish denv sz 0 0 ck KE
    walk !denv !sz !ck (CB _) =
      die "fell off stack" >> finish denv sz 0 0 ck KE
    walk !denv !sz !ck (Mark a ps cs k)
      | EC.member p ps = finish denv' sz ua ba ck k
      | otherwise = walk denv' (usz + ua) (bsz + ba) (Mark ua ba ps cs' ck) k
      where
        denv' = cs <> EC.withoutKeys denv ps
        cs' = EC.restrictKeys denv ps
    walk !denv !bsz !ck (Push n a br p brSect k) =
      walk
        denv
        (usz + un + ua)
        (bsz + bn + ba)
        (Push un bn ua ba br up bp brSect ck)
        k

    finish !denv !usz !bsz !ua !ba !ck !k = do
      (useg, ustk) <- grab ustk usz
      (bseg, bstk) <- grab bstk bsz
      ustk <- adjustArgs ustk ua
      bstk <- adjustArgs bstk ba
      return (Captured ck uasz basz useg bseg, denv, stk, k)
{-# INLINE splitCont #-}

discardCont ::
  DEnv ->
  Stack ->
  K ->
  Word64 ->
  IO (DEnv, Stack, K)
discardCont denv ustk bstk k p =
  splitCont denv ustk bstk k p
    <&> \(_, denv, stk, k) -> (denv, stk, k)
{-# INLINE discardCont #-}

resolve :: CCache -> DEnv -> Stack -> MRef -> IO Closure
resolve _ _ _ (Env cix rComb) = pure $ PAp cix rComb nullSeg
resolve _ _ stk (Stk i) = bpeekOff stk i
resolve env denv _ (Dyn i) = case EC.lookup i denv of
  Just clo -> pure clo
  Nothing -> unhandledErr "resolve" env i

unhandledErr :: String -> CCache -> Word64 -> IO a
unhandledErr fname env i =
  readTVarIO (tagRefs env) >>= \rs -> case EC.lookup i rs of
    Just r -> bomb (show r)
    Nothing -> bomb (show i)
  where
    bomb sh = die $ fname ++ ": unhandled ability request: " ++ sh

rCombSection :: EnumMap Word64 MCombs -> CombIx -> MComb
rCombSection combs (CIx r n i) =
  case EC.lookup n combs of
    Just cmbs -> case EC.lookup i cmbs of
      Just cmb -> RComb cmb
      Nothing -> error $ "unknown section `" ++ show i ++ "` of combinator `" ++ show n ++ "`. Reference: " ++ show r
    Nothing -> error $ "unknown combinator `" ++ show n ++ "`. Reference: " ++ show r

resolveSection :: CCache -> Section -> IO MSection
resolveSection cc section = do
  rcombs <- readTVarIO (combs cc)
  pure $ rCombSection rcombs <$> section

dummyRef :: Reference
dummyRef = Builtin (DTx.pack "dummy")

reserveIds :: Word64 -> TVar Word64 -> IO Word64
reserveIds n free = atomically . stateTVar free $ \i -> (i, i + n)

updateMap :: (Semigroup s) => s -> TVar s -> STM s
updateMap new0 r = do
  new <- evaluateSTM new0
  stateTVar r $ \old ->
    let total = new <> old in (total, total)

refLookup :: String -> M.Map Reference Word64 -> Reference -> Word64
refLookup s m r
  | Just w <- M.lookup r m = w
  | otherwise =
      error $ "refLookup:" ++ s ++ ": unknown reference: " ++ show r

decodeCacheArgument ::
  Sq.Seq Closure -> IO [(Reference, SuperGroup Symbol)]
decodeCacheArgument s = for (toList s) $ \case
  DataB2 _ _ (Foreign x) (DataB2 _ _ (Foreign y) _) ->
    case unwrapForeign x of
      Ref r -> pure (r, unwrapForeign y)
      _ -> die "decodeCacheArgument: Con reference"
  _ -> die "decodeCacheArgument: unrecognized value"

decodeSandboxArgument :: Sq.Seq Closure -> IO [Reference]
decodeSandboxArgument s = fmap join . for (toList s) $ \case
  Foreign x -> case unwrapForeign x of
    Ref r -> pure [r]
    _ -> pure [] -- constructor
  _ -> die "decodeSandboxArgument: unrecognized value"

encodeSandboxListResult :: [Reference] -> Sq.Seq Closure
encodeSandboxListResult =
  Sq.fromList . fmap (Foreign . Wrap Rf.termLinkRef . Ref)

encodeSandboxResult :: Either [Reference] [Reference] -> Closure
encodeSandboxResult (Left rfs) =
  encodeLeft . Foreign . Wrap Rf.listRef $ encodeSandboxListResult rfs
encodeSandboxResult (Right rfs) =
  encodeRight . Foreign . Wrap Rf.listRef $ encodeSandboxListResult rfs

encodeLeft :: Closure -> Closure
encodeLeft = DataB1 Rf.eitherRef leftTag

encodeRight :: Closure -> Closure
encodeRight = DataB1 Rf.eitherRef rightTag

addRefs ::
  TVar Word64 ->
  TVar (M.Map Reference Word64) ->
  TVar (EnumMap Word64 Reference) ->
  S.Set Reference ->
  STM (M.Map Reference Word64)
addRefs vfrsh vfrom vto rs = do
  from0 <- readTVar vfrom
  let new = S.filter (`M.notMember` from0) rs
      sz = fromIntegral $ S.size new
  frsh <- stateTVar vfrsh $ \i -> (i, i + sz)
  let newl = S.toList new
      from = M.fromList (zip newl [frsh ..]) <> from0
      nto = mapFromList (zip [frsh ..] newl)
  writeTVar vfrom from
  modifyTVar vto (nto <>)
  pure from

codeValidate ::
  [(Reference, SuperGroup Symbol)] ->
  CCache ->
  IO (Maybe (Failure Closure))
codeValidate tml cc = do
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
      rtm = rtm0 `M.withoutKeys` S.fromList rs
      rns = RN (refLookup "ty" rty) (refLookup "tm" rtm)
      combinate (n, (r, g)) = evaluate $ emitCombs rns r n g
  (Nothing <$ traverse_ combinate (zip [ftm ..] tml))
    `catch` \(CE cs perr) ->
      let msg = Util.Text.pack $ toPlainUnbroken perr
          extra = Foreign . Wrap Rf.textRef . Util.Text.pack $ show cs
       in pure . Just $ Failure ioFailureRef msg extra

sandboxList :: CCache -> Referent -> IO [Reference]
sandboxList cc (Ref r) = do
  sands <- readTVarIO $ sandbox cc
  pure . maybe [] S.toList $ M.lookup r sands
sandboxList _ _ = pure []

checkSandboxing ::
  CCache ->
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
  CCache ->
  [Reference] ->
  ANF.Value ->
  IO (Either [Reference] [Reference])
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
      | S.null miss -> pure . Right $ S.toList sbx
      | otherwise -> pure . Left $ S.toList miss
  where
    allowed = S.fromList allowed0

-- Just evaluating to force exceptions. Shouldn't actually be that
-- unsafe.
evaluateSTM :: a -> STM a
evaluateSTM x = unsafeIOToSTM (evaluate x)

cacheAdd0 ::
  S.Set Reference ->
  [(Reference, SuperGroup Symbol, Cacheability)] ->
  [(Reference, Set Reference)] ->
  CCache ->
  IO ()
cacheAdd0 ntys0 termSuperGroups sands cc = do
  let toAdd = M.fromList (termSuperGroups <&> \(r, g, _) -> (r, g))
  (unresolvedCacheableCombs, unresolvedNonCacheableCombs) <- atomically $ do
    have <- readTVar (intermed cc)
    let new = M.difference toAdd have
    let sz = fromIntegral $ M.size new
    let rgs = M.toList new
    let rs = fst <$> rgs
    int <- writeTVar (intermed cc) (have <> new)
    rty <- addRefs (freshTy cc) (refTy cc) (tagRefs cc) ntys0
    ntm <- stateTVar (freshTm cc) $ \i -> (i, i + sz)
    rtm <- updateMap (M.fromList $ zip rs [ntm ..]) (refTm cc)
    -- check for missing references
    let rns = RN (refLookup "ty" rty) (refLookup "tm" rtm)
        combinate :: Word64 -> (Reference, SuperGroup Symbol) -> (Word64, EnumMap Word64 Comb)
        combinate n (r, g) = (n, emitCombs rns r n g)
    let combRefUpdates = (mapFromList $ zip [ntm ..] rs)
    let combIdFromRefMap = (M.fromList $ zip rs [ntm ..])
    let newCacheableCombs =
          termSuperGroups
            & mapMaybe
              ( \case
                  (ref, _, Cacheable) -> M.lookup ref combIdFromRefMap
                  _ -> Nothing
              )
            & EC.setFromList
    newCombRefs <- updateMap combRefUpdates (combRefs cc)
    (unresolvedNewCombs, unresolvedCacheableCombs, unresolvedNonCacheableCombs, updatedCombs) <- stateTVar (combs cc) \oldCombs ->
      let unresolvedNewCombs :: EnumMap Word64 (GCombs any CombIx)
          unresolvedNewCombs = absurdCombs . mapFromList $ zipWith combinate [ntm ..] rgs
          (unresolvedCacheableCombs, unresolvedNonCacheableCombs) =
            EC.mapToList unresolvedNewCombs & foldMap \(w, gcombs) ->
              if EC.member w newCacheableCombs
                then (EC.mapSingleton w gcombs, mempty)
                else (mempty, EC.mapSingleton w gcombs)
          newCombs :: EnumMap Word64 MCombs
          newCombs = resolveCombs (Just oldCombs) $ unresolvedNewCombs
          updatedCombs = newCombs <> oldCombs
       in ((unresolvedNewCombs, unresolvedCacheableCombs, unresolvedNonCacheableCombs, updatedCombs), updatedCombs)
    nsc <- updateMap unresolvedNewCombs (srcCombs cc)
    nsn <- updateMap (M.fromList sands) (sandbox cc)
    ncc <- updateMap newCacheableCombs (cacheableCombs cc)
    -- Now that the code cache is primed with everything we need,
    -- we can pre-evaluate the top-level constants.
    pure $ int `seq` rtm `seq` newCombRefs `seq` updatedCombs `seq` nsn `seq` ncc `seq` nsc `seq` (unresolvedCacheableCombs, unresolvedNonCacheableCombs)
  preEvalTopLevelConstants unresolvedCacheableCombs unresolvedNonCacheableCombs cc

preEvalTopLevelConstants :: (EnumMap Word64 (GCombs Closure CombIx)) -> (EnumMap Word64 (GCombs Closure CombIx)) -> CCache -> IO ()
preEvalTopLevelConstants cacheableCombs newCombs cc = do
  activeThreads <- Just <$> UnliftIO.newIORef mempty
  evaluatedCacheableCombsVar <- newTVarIO mempty
  for_ (EC.mapToList cacheableCombs) \(w, _) -> do
    Debug.debugM Debug.Temp "Evaluating " w
    let hook _ustk bstk = do
          clos <- peek bstk
          Debug.debugM Debug.Temp "Evaluated" ("Evaluated " ++ show w ++ " to " ++ show clos)
          atomically $ do
            modifyTVar evaluatedCacheableCombsVar $ EC.mapInsert w (EC.mapSingleton 0 $ CachedClosure w clos)
    apply0 (Just hook) cc activeThreads w

  evaluatedCacheableCombs <- readTVarIO evaluatedCacheableCombsVar
  Debug.debugLogM Debug.Temp "Done pre-caching"
  let allNew = evaluatedCacheableCombs <> newCombs
  -- Rewrite all the inlined combinator references to point to the
  -- new cached versions.
  atomically $ modifyTVar (combs cc) (\existingCombs -> (resolveCombs (Just $ EC.mapDifference existingCombs allNew) allNew) <> existingCombs)

expandSandbox ::
  Map Reference (Set Reference) ->
  [(Reference, SuperGroup Symbol)] ->
  [(Reference, Set Reference)]
expandSandbox sand0 groups = fixed mempty
  where
    f sand False r = fromMaybe mempty $ M.lookup r sand
    f _ True _ = mempty

    h sand (r, foldGroupLinks (f sand) -> s)
      | S.null s = Nothing
      | otherwise = Just (r, s)

    fixed extra
      | extra == extra' = new
      | otherwise = fixed extra'
      where
        new = mapMaybe (h $ extra <> sand0) groups
        extra' = M.fromList new

cacheAdd ::
  [(Reference, SuperGroup Symbol)] ->
  CCache ->
  IO [Reference]
cacheAdd l cc = do
  rtm <- readTVarIO (refTm cc)
  rty <- readTVarIO (refTy cc)
  sand <- readTVarIO (sandbox cc)
  let known = M.keysSet rtm <> S.fromList (fst <$> l)
      f b r
        | not b, S.notMember r known = Const (S.singleton r, mempty)
        | b, M.notMember r rty = Const (mempty, S.singleton r)
        | otherwise = Const (mempty, mempty)
      (missing, tys) = getConst $ (foldMap . foldMap) (foldGroupLinks f) l
      l' = filter (\(r, _) -> M.notMember r rtm) l
      -- Terms added via cacheAdd will have already been eval'd and cached if possible when
      -- they were originally loaded, so we
      -- don't need to re-check for cacheability here as part of a dynamic cache add.
      l'' = l' <&> (\(r, g) -> (r, g, Uncacheable))
  if S.null missing
    then [] <$ cacheAdd0 tys l'' (expandSandbox sand l') cc
    else pure $ S.toList missing

reflectValue :: EnumMap Word64 Reference -> Closure -> IO ANF.Value
reflectValue rty = goV
  where
    err s = "reflectValue: cannot prepare value for serialization: " ++ s
    refTy w
      | Just r <- EC.lookup w rty = pure r
      | otherwise =
          die $ err "unknown type reference"

    goIx (CIx r _ i) = ANF.GR r i

    goV (PApV cix _rComb ua ba) =
      ANF.Partial (goIx cix) (fromIntegral <$> ua) <$> traverse goV ba
    goV (DataC _ t [w] []) = ANF.BLit <$> reflectUData t w
    goV (DataC r t us bs) =
      ANF.Data r (maskTags t) (fromIntegral <$> us) <$> traverse goV bs
    goV (CapV k _ _ us bs) =
      ANF.Cont (fromIntegral <$> us) <$> traverse goV bs <*> goK k
    goV (Foreign f) = ANF.BLit <$> goF f
    goV BlackHole = die $ err "black hole"

    goK (CB _) = die $ err "callback continuation"
    goK KE = pure ANF.KE
    goK (Mark a ps de k) = do
      ps <- traverse refTy (EC.setToList ps)
      de <- traverse (\(k, v) -> (,) <$> refTy k <*> goV v) (mapToList de)
      ANF.Mark (fromIntegral ua) (fromIntegral ba) ps (M.fromList de) <$> goK k
    goK (Push f a cix _ _rsect k) =
      ANF.Push
        (fromIntegral uf)
        (fromIntegral bf)
        (fromIntegral ua)
        (fromIntegral ba)
        (goIx cix)
        <$> goK k

    goF f
      | Just t <- maybeUnwrapBuiltin f =
          pure (ANF.Text t)
      | Just b <- maybeUnwrapBuiltin f =
          pure (ANF.Bytes b)
      | Just s <- maybeUnwrapForeign Rf.listRef f =
          ANF.List <$> traverse goV s
      | Just l <- maybeUnwrapForeign Rf.termLinkRef f =
          pure (ANF.TmLink l)
      | Just l <- maybeUnwrapForeign Rf.typeLinkRef f =
          pure (ANF.TyLink l)
      | Just v <- maybeUnwrapForeign Rf.valueRef f =
          pure (ANF.Quote v)
      | Just g <- maybeUnwrapForeign Rf.codeRef f =
          pure (ANF.Code g)
      | Just a <- maybeUnwrapForeign Rf.ibytearrayRef f =
          pure (ANF.BArr a)
      | Just a <- maybeUnwrapForeign Rf.iarrayRef f =
          ANF.Arr <$> traverse goV a
      | otherwise = die $ err $ "foreign value: " <> (show f)

    reflectUData :: Word64 -> Int -> IO ANF.BLit
    reflectUData t v
      | t == natTag = pure $ ANF.Pos (fromIntegral v)
      | t == charTag = pure $ ANF.Char (toEnum v)
      | t == intTag, v >= 0 = pure $ ANF.Pos (fromIntegral v)
      | t == intTag, v < 0 = pure $ ANF.Neg (fromIntegral (-v))
      | t == floatTag = pure $ ANF.Float (intToDouble v)
      | otherwise = die . err $ "unboxed data: " <> show (t, v)

reifyValue :: CCache -> ANF.Value -> IO (Either [Reference] Closure)
reifyValue cc val = do
  erc <-
    atomically $ do
      combs <- readTVar (combs cc)
      rtm <- readTVar (refTm cc)
      case S.toList $ S.filter (`M.notMember` rtm) tmLinks of
        [] -> do
          newTy <- addRefs (freshTy cc) (refTy cc) (tagRefs cc) tyLinks
          pure . Right $ (combs, newTy, rtm)
        l -> pure (Left l)
  traverse (\rfs -> reifyValue0 rfs val) erc
  where
    f False r = (mempty, S.singleton r)
    f True r = (S.singleton r, mempty)
    (tyLinks, tmLinks) = valueLinks f val

reifyValue0 ::
  (EnumMap Word64 MCombs, M.Map Reference Word64, M.Map Reference Word64) ->
  ANF.Value ->
  IO Closure
reifyValue0 (combs, rty, rtm) = goV
  where
    err s = "reifyValue: cannot restore value: " ++ s
    refTy r
      | Just w <- M.lookup r rty = pure w
      | otherwise = die . err $ "unknown type reference: " ++ show r
    refTm r
      | Just w <- M.lookup r rtm = pure w
      | otherwise = die . err $ "unknown term reference: " ++ show r
    goIx :: ANF.GroupRef -> IO (CombIx, MComb)
    goIx (ANF.GR r i) =
      refTm r <&> \n ->
        let cix = (CIx r n i)
         in (cix, rCombSection combs cix)

    goV (ANF.Partial gr ua ba) = do
      (cix, rcomb) <- goIx gr
      clos <- traverse goV ba
      pure $ pap cix rcomb clos
      where
        pap cix i = PApV cix i (fromIntegral <$> ua)
    goV (ANF.Data r t0 us bs) = do
      t <- flip packTags (fromIntegral t0) . fromIntegral <$> refTy r
      DataC r t (fromIntegral <$> us) <$> traverse goV bs
    goV (ANF.Cont us bs k) = cv <$> goK k <*> traverse goV bs
      where
        cv k bs = CapV k ua ba (fromIntegral <$> us) bs
          where
            (uksz, bksz) = frameDataSize k
            ua = fromIntegral $ length us - uksz
            ba = fromIntegral $ length bs - bksz
    goV (ANF.BLit l) = goL l

    goK ANF.KE = pure KE
    goK (ANF.Mark ua ba ps de k) =
      mrk
        <$> traverse refTy ps
        <*> traverse (\(k, v) -> (,) <$> refTy k <*> goV v) (M.toList de)
        <*> goK k
      where
        mrk ps de k =
          Mark (fromIntegral ua) (fromIntegral ba) (setFromList ps) (mapFromList de) k
    goK (ANF.Push uf bf ua ba gr k) =
      goIx gr >>= \case
        (cix, RComb (Lam _ _ un bx sect)) ->
          Push
            (fromIntegral uf)
            (fromIntegral bf)
            (fromIntegral ua)
            (fromIntegral ba)
            cix
            un
            bx
            sect
            <$> goK k
        (CIx r _ _, _) ->
          die . err $
            "tried to reify a continuation with a cached value resumption"
              ++ show r

    goL (ANF.Text t) = pure . Foreign $ Wrap Rf.textRef t
    goL (ANF.List l) = Foreign . Wrap Rf.listRef <$> traverse goV l
    goL (ANF.TmLink r) = pure . Foreign $ Wrap Rf.termLinkRef r
    goL (ANF.TyLink r) = pure . Foreign $ Wrap Rf.typeLinkRef r
    goL (ANF.Bytes b) = pure . Foreign $ Wrap Rf.bytesRef b
    goL (ANF.Quote v) = pure . Foreign $ Wrap Rf.valueRef v
    goL (ANF.Code g) = pure . Foreign $ Wrap Rf.codeRef g
    goL (ANF.BArr a) = pure . Foreign $ Wrap Rf.ibytearrayRef a
    goL (ANF.Char c) = pure $ DataU1 Rf.charRef charTag (fromEnum c)
    goL (ANF.Pos w) =
      pure $ DataU1 Rf.natRef natTag (fromIntegral w)
    goL (ANF.Neg w) =
      pure $ DataU1 Rf.intRef intTag (-fromIntegral w)
    goL (ANF.Float d) =
      pure $ DataU1 Rf.floatRef floatTag (doubleToInt d)
    goL (ANF.Arr a) = Foreign . Wrap Rf.iarrayRef <$> traverse goV a

doubleToInt :: Double -> Int
doubleToInt d = indexByteArray (BA.byteArrayFromList [d]) 0

intToDouble :: Int -> Double
intToDouble w = indexByteArray (BA.byteArrayFromList [w]) 0

-- Universal comparison functions

closureNum :: Closure -> Int
closureNum PAp {} = 0
closureNum DataC {} = 1
closureNum Captured {} = 2
closureNum Foreign {} = 3
closureNum BlackHole {} = error "BlackHole"

universalEq ::
  (Foreign -> Foreign -> Bool) ->
  Closure ->
  Closure ->
  Bool
universalEq frn = eqc
  where
    eql cm l r = length l == length r && and (zipWith cm l r)
    eqc (DataC _ ct1 [w1] []) (DataC _ ct2 [w2] []) =
      matchTags ct1 ct2 && w1 == w2
    eqc (DataC _ ct1 us1 bs1) (DataC _ ct2 us2 bs2) =
      ct1 == ct2
        && eql (==) us1 us2
        && eql eqc bs1 bs2
    eqc (PApV cix1 _ us1 bs1) (PApV cix2 _ us2 bs2) =
      cix1 == cix2
        && eql (==) us1 us2
        && eql eqc bs1 bs2
    eqc (CapV k1 ua1 ba1 us1 bs1) (CapV k2 ua2 ba2 us2 bs2) =
      k1 == k2
        && ua1 == ua2
        && ba1 == ba2
        && eql (==) us1 us2
        && eql eqc bs1 bs2
    eqc (Foreign fl) (Foreign fr)
      | Just al <- maybeUnwrapForeign Rf.iarrayRef fl,
        Just ar <- maybeUnwrapForeign Rf.iarrayRef fr =
          arrayEq eqc al ar
      | Just sl <- maybeUnwrapForeign Rf.listRef fl,
        Just sr <- maybeUnwrapForeign Rf.listRef fr =
          length sl == length sr && and (Sq.zipWith eqc sl sr)
      | otherwise = frn fl fr
    eqc c d = closureNum c == closureNum d

    -- serialization doesn't necessarily preserve Int tags, so be
    -- more accepting for those.
    matchTags ct1 ct2 =
      ct1 == ct2
        || (ct1 == intTag && ct2 == natTag)
        || (ct1 == natTag && ct2 == intTag)

arrayEq :: (Closure -> Closure -> Bool) -> PA.Array Closure -> PA.Array Closure -> Bool
arrayEq eqc l r
  | PA.sizeofArray l /= PA.sizeofArray r = False
  | otherwise = go (PA.sizeofArray l - 1)
  where
    go i
      | i < 0 = True
      | otherwise = eqc (PA.indexArray l i) (PA.indexArray r i) && go (i - 1)

-- IEEE floating point layout is such that comparison as integers
-- somewhat works. Positive floating values map to positive integers
-- and negatives map to negatives. The corner cases are:
--
--   1. If both numbers are negative, ordering is flipped.
--   2. There is both +0 and -0, with -0 being represented as the
--      minimum signed integer.
--   3. NaN does weird things.
--
-- So, the strategy here is to compare normally if one argument is
-- positive, since positive numbers compare normally to others.
-- Otherwise, the sign bit is cleared and the numbers are compared
-- backwards. Clearing the sign bit maps -0 to +0 and maps a negative
-- number to its absolute value (including infinities). The multiple
-- NaN values are just handled according to bit patterns, rather than
-- IEEE specified behavior.
--
-- Transitivity is somewhat non-obvious for this implementation.
--
--   if i <= j and j <= k
--     if j > 0 then k > 0, so all 3 comparisons use `compare`
--     if k > 0 then k > i, since i <= j <= 0
--     if all 3 are <= 0, all 3 comparisons use the alternate
--       comparison, which is transitive via `compare`
compareAsFloat :: Int -> Int -> Ordering
compareAsFloat i j
  | i > 0 || j > 0 = compare i j
  | otherwise = compare (clear j) (clear i)
  where
    clear k = clearBit k 64

compareAsNat :: Int -> Int -> Ordering
compareAsNat i j = compare ni nj
  where
    ni, nj :: Word
    ni = fromIntegral i
    nj = fromIntegral j

floatTag :: Word64
floatTag
  | Just n <- M.lookup Rf.floatRef builtinTypeNumbering,
    rt <- toEnum (fromIntegral n) =
      packTags rt 0
  | otherwise = error "internal error: floatTag"

natTag :: Word64
natTag
  | Just n <- M.lookup Rf.natRef builtinTypeNumbering,
    rt <- toEnum (fromIntegral n) =
      packTags rt 0
  | otherwise = error "internal error: natTag"

intTag :: Word64
intTag
  | Just n <- M.lookup Rf.intRef builtinTypeNumbering,
    rt <- toEnum (fromIntegral n) =
      packTags rt 0
  | otherwise = error "internal error: intTag"

charTag :: Word64
charTag
  | Just n <- M.lookup Rf.charRef builtinTypeNumbering,
    rt <- toEnum (fromIntegral n) =
      packTags rt 0
  | otherwise = error "internal error: charTag"

unitTag :: Word64
unitTag
  | Just n <- M.lookup Rf.unitRef builtinTypeNumbering,
    rt <- toEnum (fromIntegral n) =
      packTags rt 0
  | otherwise = error "internal error: unitTag"

leftTag, rightTag :: Word64
(leftTag, rightTag)
  | Just n <- M.lookup Rf.eitherRef builtinTypeNumbering,
    et <- toEnum (fromIntegral n),
    lt <- toEnum (fromIntegral Rf.eitherLeftId),
    rt <- toEnum (fromIntegral Rf.eitherRightId) =
      (packTags et lt, packTags et rt)
  | otherwise = error "internal error: either tags"

universalCompare ::
  (Foreign -> Foreign -> Ordering) ->
  Closure ->
  Closure ->
  Ordering
universalCompare frn = cmpc False
  where
    cmpl cm l r =
      compare (length l) (length r) <> fold (zipWith cm l r)
    cmpc _ (DataC _ ct1 [i] []) (DataC _ ct2 [j] [])
      | ct1 == floatTag, ct2 == floatTag = compareAsFloat i j
      | ct1 == natTag, ct2 == natTag = compareAsNat i j
      | ct1 == intTag, ct2 == natTag = compare i j
      | ct1 == natTag, ct2 == intTag = compare i j
    cmpc tyEq (DataC rf1 ct1 us1 bs1) (DataC rf2 ct2 us2 bs2) =
      (if tyEq && ct1 /= ct2 then compare rf1 rf2 else EQ)
        <> compare (maskTags ct1) (maskTags ct2)
        <> cmpl compare us1 us2
        -- when comparing corresponding `Any` values, which have
        -- existentials inside check that type references match
        <> cmpl (cmpc $ tyEq || rf1 == Rf.anyRef) bs1 bs2
    cmpc tyEq (PApV cix1 _ us1 bs1) (PApV cix2 _ us2 bs2) =
      compare cix1 cix2
        <> cmpl compare us1 us2
        <> cmpl (cmpc tyEq) bs1 bs2
    cmpc _ (CapV k1 ua1 ba1 us1 bs1) (CapV k2 ua2 ba2 us2 bs2) =
      compare k1 k2
        <> compare ua1 ua2
        <> compare ba1 ba2
        <> cmpl compare us1 us2
        <> cmpl (cmpc True) bs1 bs2
    cmpc tyEq (Foreign fl) (Foreign fr)
      | Just sl <- maybeUnwrapForeign Rf.listRef fl,
        Just sr <- maybeUnwrapForeign Rf.listRef fr =
          fold (Sq.zipWith (cmpc tyEq) sl sr)
            <> compare (length sl) (length sr)
      | Just al <- maybeUnwrapForeign Rf.iarrayRef fl,
        Just ar <- maybeUnwrapForeign Rf.iarrayRef fr =
          arrayCmp (cmpc tyEq) al ar
      | otherwise = frn fl fr
    cmpc _ c d = comparing closureNum c d

arrayCmp ::
  (Closure -> Closure -> Ordering) ->
  PA.Array Closure ->
  PA.Array Closure ->
  Ordering
arrayCmp cmpc l r =
  comparing PA.sizeofArray l r <> go (PA.sizeofArray l - 1)
  where
    go i
      | i < 0 = EQ
      | otherwise = cmpc (PA.indexArray l i) (PA.indexArray r i) <> go (i - 1)
