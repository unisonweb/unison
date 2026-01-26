{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

module Unison.Runtime.Machine
  ( ActiveThreads,
    CCache (..),
    Combs,
    Tracer (..),
    apply0,
    baseCCache,
    cacheAdd,
    cacheAdd0,
    eval0,
    expandSandbox,
    preEvalTopLevelConstants,
    refLookup,
    refNumTm,
    refNumsTm,
    refNumsTy,
    reifyValue,
    resolveSection,
  )
where

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM as STM
import Control.Exception
import Control.Lens
import Control.Monad.State.Strict
import Data.Atomics qualified as Atomic
import Data.HashMap.Lazy qualified as HM
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as M
import Data.Map.Strict.Internal qualified as M
import Data.Sequence qualified as Sq
import Data.Set qualified as S
import Data.Set qualified as Set
import Data.Text qualified as DTx
import Data.Text.IO qualified as Tx
import Data.Traversable
import Foreign.LibFFI.Internal
import Foreign.Marshal (alloca)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr
import Foreign.Storable qualified as Store
import GHC.Conc as STM (unsafeIOToSTM)
import GHC.Float (double2Float, float2Double)
import GHC.Stack
import Unison.Builtin.Decls (exceptionRef)
import Unison.Builtin.Decls qualified as Rf
import Unison.Prelude hiding (Text)
import Unison.Reference
  ( Reference,
    Reference' (Builtin),
    showShort,
  )
import Unison.Referent (Referent, pattern Ref)
import Unison.ReferentPrime (Referent' (..))
import Unison.Runtime.ANF as ANF
  ( Cacheability (..),
    Code (..),
    PackedTag (..),
    SuperGroup,
    codeGroup,
    collectValueLinks,
    foldGroup,
    foldGroupLinks,
    maskTags,
    packTags,
  )
import Unison.Runtime.ANF qualified as ANF
import Unison.Runtime.ANF.Optimize qualified as ANF
#ifdef CODE_SERIAL_CHECK
import Unison.Runtime.ANF.Serialize (serializeCode, deserializeCode)
#endif
import Data.Text qualified as Text
import Unison.Runtime.Array as PA
import Unison.Runtime.Builtin hiding (unitValue)
import Unison.Runtime.Exception (RuntimeExn (BU, PE), die, exn)
import Unison.Runtime.Foreign.Dynamic qualified as DLL
import Unison.Runtime.Foreign.Function
  ( decodeVal,
    encodeVal,
    foreignCall,
    functionReplacements,
    functionUnreplacements,
    pseudoConstructors,
    writeBack,
  )
import Unison.Runtime.MCode
import Unison.Runtime.Machine.Primops
import Unison.Runtime.Machine.Types
import Unison.Runtime.Profiling
import Unison.Runtime.Referenced
import Unison.Runtime.Stack
import Unison.Runtime.TypeTags qualified as TT
import Unison.Symbol (Symbol)
import Unison.Type qualified as Rf
import Unison.Util.EnumContainers as EC
import Unison.Util.Pretty qualified as P
import Unison.Util.Text qualified as Util.Text
import UnliftIO qualified
import UnliftIO.Concurrent qualified as UnliftIO

#ifdef STACK_CHECK
import Unison.Debug qualified as Debug
import System.IO.Unsafe (unsafePerformIO)
#endif

#ifdef OPT_CHECK
import Test.Inspection qualified as TI
#endif

info :: (Show a) => String -> a -> IO ()
info ctx x = infos ctx (show x)

infos :: String -> String -> IO ()
infos ctx s = putStrLn $ ctx ++ ": " ++ s

-- Entry point for evaluating a section
eval0 ::
  (RuntimeProfiler p) => CCache p -> ActiveThreads -> MSection -> IO ()
eval0 env !activeThreads !co = do
  stk <- alloc
  cmbs <- readTVarIO $ combs env
  (henv, k) <- do
    rfTy <- readTVarIO (refTy env)
    rfTm <- readTVarIO (refTm env)
    topHEnv cmbs rfTy rfTm
  (tick, cancelTicks) <- startTicker $ profiler env
  eval tick env henv activeThreads stk (k KE) (CIx dummyRef 0 0) co
    `finally` cancelTicks
{-# SPECIALIZE eval0 ::
  CCache () -> ActiveThreads -> MSection -> IO ()
  #-}
{-# SPECIALIZE eval0 ::
  CCache ProfileComm -> ActiveThreads -> MSection -> IO ()
  #-}

mCombVal :: CombIx -> MComb -> Val
mCombVal cix (RComb (Comb comb)) =
  BoxedVal (PAp cix comb nullSeg)
mCombVal _ (RComb (CachedVal _ clo)) = clo

topAEnv ::
  EnumMap Word64 MCombs ->
  M.Map Reference Word64 ->
  M.Map Reference Word64 ->
  IO (AEnv, K -> K)
topAEnv combs rfTy rfTm
  | Just n <- M.lookup exceptionRef rfTy,
    rcrf <- Builtin (DTx.pack "raise"),
    Just j <- M.lookup rcrf rfTm,
    cix <- CIx rcrf j 0,
    clo <- mCombVal cix $ rCombSection combs cix = do
      r <- newIORef BlackHole
      let ar = ARef r
      ahv <- extendPAp clo . BoxedVal $ Affine (setSingleton n) mempty ar
      writeIORef r ahv
      pure (EC.mapSingleton n ar, AMark 0 mempty ar)
topAEnv _ _ _ = pure (mempty, id)

topHEnv ::
  EnumMap Word64 MCombs ->
  M.Map Reference Word64 ->
  M.Map Reference Word64 ->
  IO (HEnv, K -> K)
topHEnv combs rfTy rfTm =
  first (flip HEnv mempty) <$> topAEnv combs rfTy rfTm

-- Entry point for evaluating a numbered combinator.
-- An optional callback for the base of the stack may be supplied.
--
-- This is the entry point actually used in the interactive
-- environment currently.
apply0 ::
  (RuntimeProfiler p) =>
  Maybe (XStack -> IO ()) ->
  CCache p ->
  ActiveThreads ->
  Word64 ->
  IO ()
apply0 !callback env !threadTracker !i = do
  stk <- alloc
  cmbrs <- readTVarIO $ combRefs env
  cmbs <- readTVarIO $ combs env
  (henv, kf) <- do
    rfTy <- readTVarIO (refTy env)
    rfTm <- readTVarIO (refTm env)
    topHEnv cmbs rfTy rfTm
  r <- case EC.lookup i cmbrs of
    Just r -> pure r
    Nothing -> die [] "apply0: missing reference to entry point"
  let entryCix = (CIx r i 0)
  case unRComb $ rCombSection cmbs entryCix of
    Comb entryComb -> do
      (tick, cancelTicks) <- startTicker $ profiler env
      apply
        tick
        env
        henv
        threadTracker
        stk
        (kf k0)
        True
        ZArgs
        (BoxedVal $ PAp entryCix entryComb nullSeg)
        `finally` cancelTicks
    -- if it's cached, we can just finish
    CachedVal _ val -> bump stk >>= \stk -> poke stk val
  where
    k0 = fromMaybe KE (callback <&> \cb -> CB . Hook $ \stk -> cb stk)
{-# SPECIALIZE apply0 ::
  Maybe (XStack -> IO ()) ->
  CCache () ->
  ActiveThreads ->
  Word64 ->
  IO ()
  #-}
{-# SPECIALIZE apply0 ::
  Maybe (XStack -> IO ()) ->
  CCache ProfileComm ->
  ActiveThreads ->
  Word64 ->
  IO ()
  #-}

-- Apply helper currently used for forking. Creates the new stacks
-- necessary to evaluate a closure with the provided information.
apply1 ::
  (RuntimeProfiler p) =>
  (Stack -> IO ()) ->
  CCache p ->
  ActiveThreads ->
  Val ->
  IO ()
apply1 callback env threadTracker clo = do
  stk <- alloc
  (tick, cancelTicks) <- startTicker $ profiler env
  apply tick env mempty threadTracker stk k0 True ZArgs clo
    `finally` cancelTicks
  where
    k0 = CB $ Hook (\stk -> callback $ packXStack stk)
{-# INLINE apply1 #-}

unitValue :: Val
unitValue = BoxedVal $ unitClosure
{-# NOINLINE unitValue #-}

litToVal :: MLit -> Val
litToVal = \case
  MT t -> BoxedVal $ Foreign (WrapText t)
  MM r -> BoxedVal $ Foreign (WrapReferent r)
  MY r -> BoxedVal $ Foreign (WrapReference r)
  MI i -> IntVal i
  MN n -> NatVal n
  MC c -> CharVal c
  MD d -> DoubleVal d
{-# INLINE litToVal #-}

#ifdef STACK_CHECK
debugger :: (Show a) => Stack -> String -> a -> Bool
debugger stk msg a = unsafePerformIO $ do
  dumpStack stk
  Debug.debugLogM Debug.Interpreter (msg ++ ": " ++ show a)
  pure False

dumpStack :: Stack -> IO ()
dumpStack stk@(Stack ap fp sp _ustk _bstk)
  | sp - fp < 0 = Debug.debugLogM Debug.Interpreter "Stack before 👇: Empty"
  | otherwise = do
      stkLocals <- for [0 .. ((sp - fp) - 1)] $ \i -> do
        peekOff stk i
      Debug.debugM Debug.Interpreter "Stack frame locals 👇:" stkLocals
      stkArgs <- for [0 .. ((fp - ap) - 1)] $ \i -> do
        peekOff stk (i + (sp - fp))
      Debug.debugM Debug.Interpreter "Stack args 👇:" stkArgs
#endif

-- | Execute an instruction
--
-- Note: both `env` and `henv` are intentionally not strict arguments.
-- It seems to be slower to unpack them into many arguments. `env` is
-- never modified, so this is no worry. `henv` is modified, but it is
-- immediately evaluated when created to avoid thunks building up, so
-- that it doesn't need to be a strict argument.
exec ::
  (RuntimeProfiler prof) =>
  CCache prof ->
  HEnv ->
  ActiveThreads ->
  Stack ->
  K ->
  CombIx ->
  MInstr ->
  IO (Bool, HEnv, Stack, K)
#ifdef STACK_CHECK
exec _ _ !_ !stk !_ !_ instr
  | debugger stk "exec" instr = undefined
#endif
exec _ henv !_activeThreads !stk !k _ (Info tx) = do
  info tx stk
  info tx k
  pure (False, henv, stk, k)
exec env henv !_activeThreads !stk !k _ (Name r args) = do
  v <- resolve env henv stk r
  stk <- name stk args v
  pure (False, henv, stk, k)
exec _ henv !_activeThreads !stk !k _ (SetAff u i j) =
  bpeekOff stk i >>= \case
    Affine ps _ ar@(ARef r) -> do
      bpeekOff stk j >>= writeIORef r
      henv <-
        if u
          then do
            aenv <-
              evaluate $ EC.unionWith const (mapFromSet ps ar) (aenv henv)
            evaluate $ henv {aenv = aenv}
          else pure henv
      pure (False, henv, stk, k)
    _ -> die [] "SetAff called with bad handler reference"
exec _ henv !_activeThreads !stk !k _ (Capture p) = do
  (cap, denv, stk, k) <- splitCont (denv henv) stk k p
  stk <- bump stk
  poke stk cap
  henv <- evaluate $ henv {denv = denv}
  pure (False, henv, stk, k)
exec _ _henv !_activeThreads !stk !k _ (Discard i) = do
  bpeekOff stk i >>= \case
    Affine _ _ r -> do
      (aenv, stk, k) <- abortCont stk k r
      henv <- evaluate $ HEnv aenv mempty
      pure (False, henv, stk, k)
    _ -> die [] "Discard called with bad handler reference"
exec _env henv0 !_activeThreads !stk !k _ (InLocal i) = do
  bpeekOff stk i >>= \case
    Affine _ aenv _ -> do
      (stk, a) <- saveArgs stk
      henv <- evaluate $ HEnv aenv mempty
      pure (False, henv, stk, Local henv0 a k)
    v -> die [] $ "InLocal called with bad handler reference\n" ++ show v
exec env henv !_activeThreads !stk !k _ (Prim1 CACH i)
  | sandboxed env = die [] "attempted to use sandboxed operation: cache"
  | otherwise = do
      arg <- peekOffS stk i
      news <- decodeCacheArgument arg
      unknown <- cacheAdd news env
      stk <- bump stk
      pokeS
        stk
        (Sq.fromList $ encodeVal . Ref <$> unknown)
      pure (False, henv, stk, k)
exec env henv !_activeThreads !stk !k _ (Prim1 LOAD i)
  | sandboxed env = die [] "attempted to use sandboxed operation: load"
  | otherwise = do
      v <- peekOffBi stk i
      stk <- bumpn stk 2
      reifyValue env v >>= \case
        Left miss -> do
          pokeOffS stk 1 $
            Sq.fromList $
              encodeVal . Ref <$> miss
          pokeTag stk 0
        Right x -> do
          pokeOff stk 1 x
          pokeTag stk 1
      pure (False, henv, stk, k)
exec env henv !_activeThreads !stk !k _ (Prim1 VALU i) = do
  c <- peekOff stk i
  stk <- bump stk
  pokeBi stk =<< reflectValue env c
  pure (False, henv, stk, k)
exec env henv !_activeThreads !stk !k _ (Prim1 op i) = do
  stk <- prim1 env stk op i
  pure (False, henv, stk, k)
exec _ _ !_activeThreads !stk !k cix (Prim2 THRO i j) = do
  name <- peekOffBi @Util.Text.Text stk i
  x <- peekOff stk j
  () <- throwIO $ BU (traceK r k) (Util.Text.toText name) x
  error "throwIO should never return"
  where
    r = combRef cix
exec env henv !_activeThreads !stk !k _ (Prim2 TRCE i j)
  | sandboxed env = pure (False, henv, stk, k)
  | otherwise = do
      tx <- peekOffBi stk i
      clo <- peekOff stk j
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
      pure (False, henv, stk, k)
exec env henv !_trackThreads !stk !k _ (Prim2 op i j) = do
  stk <- primxx env stk op i j
  pure (False, henv, stk, k)
exec env henv !_activeThreads !stk !k _ (RefCAS refI ticketI valI)
  | sandboxed env = die [] "attempted to use sandboxed operation: Ref.cas"
  | otherwise = do
      (ref :: IORef Val) <- peekOffBi stk refI
      -- Note that the CAS machinery is extremely fussy w/r to whether things are forced because it
      -- uses unsafe pointer equality. The only way we've gotten it to work as expected is with liberal
      -- forcing of the values and tickets.
      !(ticket :: Atomic.Ticket Val) <- peekOffBi stk ticketI
      v <- peekOff stk valI
      (r, _) <- Atomic.casIORef ref ticket v
      stk <- bump stk
      pokeBool stk r
      pure (False, henv, stk, k)
exec _ henv !_activeThreads !stk !k _ (Pack r t args) = do
  clo <- buildData stk r t args
  stk <- bump stk
  bpoke stk clo
  pure (False, henv, stk, k)
exec _ henv !_activeThreads !stk !k _ (RecPack args) = do
  clo <- buildRec stk args
  stk <- bump stk
  bpoke stk clo
  pure (False, henv, stk, k)
exec _ henv !_activeThreads !stk !k _ (Print i) = do
  t <- peekOffBi stk i
  Tx.putStrLn (Util.Text.toText t)
  pure (False, henv, stk, k)
exec _ henv !_activeThreads !stk !k _ (Lit ml) = do
  stk <- bump stk
  poke stk $ litToVal ml
  pure (False, henv, stk, k)
exec _ henv !_activeThreads !stk !k _ (Reset ps nhi mah)
  -- if denv is null, and there's an affine handler, use it
  | HEnv aenv0 denv0 <- henv,
    null denv0,
    Just ahi <- mah = do
      (stk, a) <- saveArgs stk
      ahv0 <- peekOff stk ahi
      r <- newIORef BlackHole
      let ar = ARef r
      ahv <- extendPAp ahv0 . BoxedVal $ Affine ps aenv0 ar
      writeIORef r ahv
      aenv <- evaluate $ EC.unionWith const (mapFromSet ps ar) aenv0
      henv <- evaluate $ henv {aenv = aenv}
      pure (False, henv, stk, AMark a aenv0 ar k)
  | HEnv aenv0 denv0 <- henv = do
      (stk, a) <- saveArgs stk
      nh <- peekOff stk nhi
      denv <- evaluate $ EC.unionWith const (mapFromSet ps nh) denv0
      henv <- evaluate $ HEnv aenv0 denv
      clos <- evaluate $ EC.restrictKeys denv0 ps
      pure (False, henv, stk, Mark a ps clos k)
exec _ henv !_activeThreads !stk !k _ (Seq as) = do
  l <- closureArgs stk as
  stk <- bump stk
  pokeS stk $ Sq.fromList l
  pure (False, henv, stk, k)
exec _env henv !_activeThreads !stk !k _ (ForeignCall _ func args) = do
  (b, stk) <- exStackIOToIO $ foreignCall func args (unpackXStack stk)
  pure (b, henv, stk, k)
exec env henv !activeThreads !stk !k _ (Fork i)
  | sandboxed env = die [] "attempted to use sandboxed operation: fork"
  | otherwise = do
      tid <- forkEval env activeThreads =<< peekOff stk i
      stk <- bump stk
      bpoke stk . Foreign . WrapThreadId $ tid
      pure (False, henv, stk, k)
exec env henv !activeThreads !stk !k _ (Atomically i)
  | sandboxed env = die [] $ "attempted to use sandboxed operation: atomically"
  | otherwise = do
      v <- peekOff stk i
      stk <- bump stk
      atomicEval env activeThreads (poke stk) v
      pure (False, henv, stk, k)
exec env henv !activeThreads !stk !k _ (TryForce i)
  | sandboxed env = die [] $ "attempted to use sandboxed operation: tryForce"
  | otherwise = do
      v <- peekOff stk i
      stk <- bump stk -- Bump the boxed stack to make a slot for the result, which will be written in the callback if we succeed.
      ev <- Control.Exception.try $ nestEval env activeThreads (poke stk) v
      stk <- encodeExn stk ev
      pure (False, henv, stk, k)
exec _ henv !_activeThreads !stk !k _ DLLCall = do
  cf <- peekBi stk
  let n = DLL.numArgs $ DLL.cSpec cf
  -- Note: pre-bump, because you can't pass stk out of these blocks
  -- without boxing (or customizing allocaArray).
  stk <- bump stk
  allocaArray n \storage ->
    allocaArray n \cArgs ->
      alloca \(cRet :: Ptr Int) -> do
        copyArgs stk (DLL.cffArgs cf) storage cArgs do
          DLL.callForeign cf cArgs cRet
          case DLL.cffResult cf of
            DLL.I8 -> Store.peek (castPtr cRet) >>= pokeI stk . fi8
            DLL.I16 -> Store.peek (castPtr cRet) >>= pokeI stk . fi16
            DLL.I32 -> Store.peek (castPtr cRet) >>= pokeI stk . fi32
            DLL.I64 -> Store.peek cRet >>= pokeI stk
            DLL.U8 -> Store.peek (castPtr cRet) >>= pokeN stk . fu8
            DLL.U16 -> Store.peek (castPtr cRet) >>= pokeN stk . fu16
            DLL.U32 -> Store.peek (castPtr cRet) >>= pokeN stk . fu32
            DLL.U64 -> Store.peek (castPtr cRet) >>= pokeN stk
            DLL.F32 -> Store.peek (castPtr cRet) >>= pokeD stk . ff32
            DLL.D64 -> Store.peek (castPtr cRet) >>= pokeD stk
            DLL.Void -> poke stk unitValue
            DLL.Ptr ->
              Store.peek (castPtr cRet) >>= writeBack @(Ptr ()) stk
            DLL.MBArr ->
              die [] $ "unexpected array result from DLL function"
  pure (False, henv, stk, k)
exec _ henv !_activeThreads !stk !k _ (KeepAlive i) = do
  x <- bpeekOff stk i
  (stk, a) <- saveArgs stk
  pure (False, henv, stk, Keep x a k)
exec _ _ !_ !_ !_ _ (SandboxingFailure t) = do
  die [] $ "Attempted to use disallowed builtin in sandboxed environment: " <> DTx.unpack t
{-# INLINE exec #-}

fi8 :: Int8 -> Int
fi8 = fromIntegral

ti8 :: Int -> Int8
ti8 = fromIntegral

fu8 :: Word8 -> Word64
fu8 = fromIntegral

tu8 :: Word64 -> Word8
tu8 = fromIntegral

fi16 :: Int16 -> Int
fi16 = fromIntegral

ti16 :: Int -> Int16
ti16 = fromIntegral

fi32 :: Int32 -> Int
fi32 = fromIntegral

ti32 :: Int -> Int32
ti32 = fromIntegral

fu32 :: Word32 -> Word64
fu32 = fromIntegral

tu32 :: Word64 -> Word32
tu32 = fromIntegral

fu16 :: Word16 -> Word64
fu16 = fromIntegral

tu16 :: Word64 -> Word16
tu16 = fromIntegral

tf32 :: Double -> Float
tf32 = double2Float

ff32 :: Float -> Double
ff32 = float2Double

-- Copies unison stack values into temporary space appropriate for
-- calling libffi. The latter takes all arguments as pointers, so we
-- need to copy the arguments to pinned memory to have a stable
-- location. All our FFI arguments are 64-bit or smaller, though, so
-- we can just use a contiguous array with as many 8 byte slots as
-- there are arguments, possibly using only portions of some slots.
copyArgs ::
  Stack -> [DLL.FFType] -> Ptr Int -> Ptr (Ptr CValue) -> IO () -> IO ()
copyArgs !stk tys p0 h0 next = go 2 tys p0 h0
  where
    go !i (a : as) !p !h = store a i p do
      Store.poke h (castPtr p)
      go (i + 1) as (plusPtr p szp) (plusPtr h szh)
    go _ _ _ _ = next

    -- special case non-64-bit values for conversions, otherwise just
    -- copy bytes.
    store DLL.I32 i p nx =
      upeekOff stk i >>= Store.poke (castPtr p) . ti32 >> nx
    store DLL.U32 i p nx =
      peekOffN stk i >>= Store.poke (castPtr p) . tu32 >> nx
    store DLL.I16 i p nx =
      upeekOff stk i >>= Store.poke (castPtr p) . ti16 >> nx
    store DLL.U16 i p nx =
      peekOffN stk i >>= Store.poke (castPtr p) . tu16 >> nx
    store DLL.I8 i p nx =
      upeekOff stk i >>= Store.poke (castPtr p) . ti8 >> nx
    store DLL.U8 i p nx =
      peekOffN stk i >>= Store.poke (castPtr p) . tu8 >> nx
    store DLL.F32 i p nx =
      peekOffD stk i >>= Store.poke (castPtr p) . tf32 >> nx
    store DLL.MBArr i p nx = do
      mb <- peekOffBi stk i
      withMutableByteArrayContents mb \ptr ->
        Store.poke (castPtr p) ptr >> nx
    store DLL.Ptr i p nx = do
      peekOffBi @(Ptr ()) stk i >>= Store.poke (castPtr p) >> nx
    store _ i p nx =
      upeekOff stk i >>= Store.poke p >> nx
    {-# INLINE store #-}

    szp = Store.sizeOf (0 :: Int)
    szh = Store.sizeOf (undefined :: Ptr CValue)
{-# INLINE copyArgs #-}

encodeExn ::
  Stack ->
  Either SomeException () ->
  IO Stack
encodeExn stk exc = do
  case exc of
    Right () -> do
      stk <- bump stk
      stk <$ pokeTag stk 1
    Left exn -> do
      -- If we hit an exception, we have one unused slot on the stack
      -- from where the result _would_ have been placed.
      -- So here we bump one less than it looks like we should, and re-use
      -- that slot.
      stk <- bumpn stk 3
      pokeTag stk 0
      bpokeOff stk 1 $ Foreign (WrapReference link)
      pokeOffBi stk 2 msg
      stk <$ pokeOff stk 3 extra
      where
        disp :: (Exception e) => e -> Util.Text.Text
        disp = Util.Text.pack . show
        (link, msg, extra)
          | Just (ioe :: IOException) <- fromException exn =
              (Rf.ioFailureRef, disp ioe, unitValue)
          | Just re <- fromException exn = case re of
              PE _stk _issues msg ->
                (Rf.runtimeFailureRef, Util.Text.fromText $ P.toPlain 0 msg, unitValue)
              BU _ tx val -> (Rf.runtimeFailureRef, Util.Text.fromText tx, val)
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
          | Just (ie :: UnliftIO.AsyncCancelled) <- fromException exn =
              (Rf.asyncCancelledFailureRef, disp ie, unitValue)
          | Just (Panic msg v) <- fromException exn,
            msg <- Util.Text.pack $ "panic: " ++ msg =
              (Rf.miscFailureRef, msg, fromMaybe unitValue v)
          | otherwise = (Rf.miscFailureRef, disp exn, unitValue)

-- | Evaluate a section
--
-- Note: both `env` and `henv` are intentionally not strict arguments.
-- It seems to be slower to unpack them into many arguments. `env` is
-- never modified, so this is no worry. `henv` is modified, but it is
-- immediately evaluated when created to avoid thunks building up, so
-- that it doesn't need to be a strict argument.
eval' ::
  (RuntimeProfiler prof) =>
  Ticker prof ->
  CCache prof ->
  HEnv ->
  ActiveThreads ->
  Stack ->
  K ->
  CombIx ->
  MSection ->
  IO ()
#ifdef STACK_CHECK
eval' !_ _ _ !_ !stk !_ !_ section
  | debugger stk "eval" section = undefined
#endif
eval' !yld env henv !activeThreads !stk !k r (Match i (TestT df cs)) = do
  t <- peekOffBi stk i
  eval yld env henv activeThreads stk k r $ selectTextBranch t df cs
eval' !yld env henv !activeThreads !stk !k r (Match i br) = do
  n <- peekOffN stk i
  eval yld env henv activeThreads stk k r $ selectBranch n br
eval' !yld env henv !activeThreads !stk !k r (DMatch mr i br) = do
  (nx, stk) <- dataBranch mr stk br =<< bpeekOff stk i
  eval yld env henv activeThreads stk k r nx
eval' !yld env henv !activeThreads !stk !k r (NMatch _mr i br) = do
  n <- peekOffN stk i
  eval yld env henv activeThreads stk k r $ selectBranch n br
eval' !yld env henv !activeThreads !stk !k r (RMatch i pu br) = do
  (t, stk) <- dumpDataValNoTag stk =<< peekOff stk i
  if t == TT.pureEffectTag
    then eval yld env henv activeThreads stk k r pu
    else case ANF.unpackTags t of
      (ANF.rawTag -> e, ANF.rawTag -> t)
        | Just ebs <- EC.lookup e br ->
            eval yld env henv activeThreads stk k r $ selectBranch t ebs
        | otherwise -> unhandledAbilityRequest
eval' !yld env henv !activeThreads !stk !k here (Yield args)
  | asize stk > 0,
    VArg1 i <- args = do
      checkTicker yld here k
      peekOff stk i >>= apply yld env henv activeThreads stk k False ZArgs
  | otherwise = do
      checkTicker yld here k
      stk <- moveArgs stk args
      stk <- frameArgs stk
      yield yld env henv activeThreads stk k
eval' !yld env henv !activeThreads !stk !k here (App ck r args) = do
  checkTicker yld here k
  resolve env henv stk r
    >>= apply yld env henv activeThreads stk k ck args
eval' !yld env henv !activeThreads !stk !k here (Call ck combIx rcomb args) = do
  checkTicker yld here k
  enter yld env henv activeThreads stk k combIx ck args rcomb
eval' !yld env henv !activeThreads !stk !k _ (Jump i args) =
  bpeekOff stk i >>= jump yld env henv activeThreads stk k args
eval' !yld env henv !activeThreads !stk !k r (Let nw cix f sect) = do
  (stk, fsz, asz) <- saveFrame stk
  eval
    yld
    env
    henv
    activeThreads
    stk
    (Push fsz asz cix f sect k)
    r
    nw
eval' !yld env henv !activeThreads !stk !k r (Ins i nx) = do
  exec env henv activeThreads stk k r i >>= \case
    (exception, henv, !stk, !k)
      -- In this case, the instruction indicated an exception to
      -- be handled by the current {Exception} handler. The stack
      -- currently points to an appropriate `Failure` value, and
      -- we must handle the rest.
      | exception -> do
          eh <- resolveExceptionHandler henv
          fv <- peek stk
          bpoke stk $ Data1 exceptionRef TT.exceptionRaiseTag fv
          (stk, fsz, asz) <- saveFrame stk
          let kk = Push fsz asz fakeCix 10 nx k
          apply yld env henv activeThreads stk kk False (VArg1 0) eh
      | otherwise -> eval yld env henv activeThreads stk k r nx
eval' !_ _ _ !_ !_activeThreads !_ _ Exit = pure ()
eval' !_ _ _ !_ !_activeThreads !_ _ (Die s) = die [] s
{-# INLINE eval' #-}

eval ::
  (RuntimeProfiler prof) =>
  Ticker prof ->
  CCache prof ->
  HEnv ->
  ActiveThreads ->
  Stack ->
  K ->
  CombIx ->
  MSection ->
  IO ()
eval !yld env henv !activeThreads !stk !k here sect = do
  checkTicker yld here k
  eval' yld env henv activeThreads stk k here sect
{-# SPECIALIZE eval ::
  Ticker () ->
  CCache () ->
  HEnv ->
  ActiveThreads ->
  Stack ->
  K ->
  CombIx ->
  MSection ->
  IO ()
  #-}
{-# SPECIALIZE eval ::
  Ticker ProfileComm ->
  CCache ProfileComm ->
  HEnv ->
  ActiveThreads ->
  Stack ->
  K ->
  CombIx ->
  MSection ->
  IO ()
  #-}

-- Note: denv shadows aenv always
resolveExceptionHandler :: HEnv -> IO Val
resolveExceptionHandler (HEnv aenv denv)
  | Just eh <- EC.lookup TT.exceptionTag denv = pure eh
  | Just (ARef r) <- EC.lookup TT.exceptionTag aenv =
      BoxedVal <$> readIORef r
  -- should be impossible
  | otherwise = unhandledAbilityRequest
{-# INLINE resolveExceptionHandler #-}

fakeCix :: CombIx
fakeCix = CIx exceptionRef maxBound maxBound

unhandledAbilityRequest :: (HasCallStack) => IO a
unhandledAbilityRequest = exn [2922, 5400] "eval: unhandled ability request"

forkEval ::
  (RuntimeProfiler prof) =>
  CCache prof ->
  ActiveThreads ->
  Val ->
  IO ThreadId
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

nestEval ::
  (RuntimeProfiler prof) =>
  CCache prof ->
  ActiveThreads ->
  (Val -> IO ()) ->
  Val ->
  IO ()
nestEval env activeThreads write val = apply1 readBack env activeThreads val
  where
    readBack stk = peek stk >>= write
{-# INLINE nestEval #-}

atomicEval ::
  (RuntimeProfiler prof) =>
  CCache prof ->
  ActiveThreads ->
  (Val -> IO ()) ->
  Val ->
  IO ()
atomicEval env activeThreads write val =
  atomically . unsafeIOToSTM $ nestEval env activeThreads write val
{-# INLINE atomicEval #-}

-- fast path application
enter ::
  (RuntimeProfiler prof) =>
  Ticker prof ->
  CCache prof ->
  HEnv ->
  ActiveThreads ->
  Stack ->
  K ->
  CombIx ->
  Bool ->
  Args ->
  MComb ->
  IO ()
enter !yld env henv !activeThreads !stk !k !cref !sck !args = \case
  (RComb (Lam a f entry)) -> do
    -- check for stack check _skip_
    stk <- if sck then pure stk else ensure stk f
    stk <- moveArgs stk args
    stk <- acceptArgs stk a
    eval yld env henv activeThreads stk k cref entry
  (RComb (CachedVal _ val)) -> do
    stk <- discardFrame stk
    stk <- bump stk
    poke stk val
    yield yld env henv activeThreads stk k
{-# INLINE enter #-}

-- fast path by-name delaying
name :: Stack -> Args -> Val -> IO Stack
name !stk !args = \case
  BoxedVal (PAp cix comb seg) -> do
    seg <- closeArgs I stk seg args
    stk <- bump stk
    bpoke stk $ PAp cix comb seg
    pure stk
  v -> die [] $ "naming non-function: " ++ show v
{-# INLINE name #-}

extendPAp :: Val -> Val -> IO Closure
extendPAp (BoxedVal (PAp cix comb (useg0, bseg0))) new = do
  ucop <- newByteArray $ ussz + 8
  copyByteArray ucop 8 useg0 0 ussz
  writeByteArray ucop 0 $ getUnboxedVal new
  useg <- unsafeFreezeByteArray ucop

  bcop <- newArray (bssz + 1) BlackHole
  copyArray bcop 1 bseg0 0 bssz
  writeArray bcop 0 $ getBoxedVal new
  bseg <- unsafeFreezeArray bcop

  pure $ PAp cix comb (useg, bseg)
  where
    ussz = sizeofByteArray useg0
    bssz = sizeofArray bseg0
extendPAp v _ =
  die [] $ "extendPAp: non partial application" ++ show v
{-# INLINE extendPAp #-}

-- slow path application
apply ::
  (RuntimeProfiler prof) =>
  Ticker prof ->
  CCache prof ->
  HEnv ->
  ActiveThreads ->
  Stack ->
  K ->
  Bool ->
  Args ->
  Val ->
  IO ()
#ifdef STACK_CHECK
apply !yld _env _henv !_activeThreads !stk !_k !_ck !args !val
  | debugger stk "apply" (args, val) = undefined
#endif
apply !yld env henv !activeThreads !stk !k !ck !args !val =
  case val of
    BoxedVal (PAp cix comb seg) ->
      case comb of
        LamI a f entry
          | ck || a <= ac -> do
              stk <- ensure stk f
              stk <- moveArgs stk args
              stk <- dumpSeg stk seg A
              stk <- acceptArgs stk a
              eval yld env henv activeThreads stk k cix entry
          | otherwise -> do
              seg <- closeArgs C stk seg args
              stk <- discardFrame =<< frameArgs stk
              stk <- bump stk
              bpoke stk $ PAp cix comb seg
              yield yld env henv activeThreads stk k
      where
        ac = asize stk + countArgs args + scount seg
    v -> zeroArgClosure v
  where
    zeroArgClosure :: Val -> IO ()
    zeroArgClosure v
      | ZArgs <- args,
        asize stk == 0 = do
          stk <- discardFrame stk
          stk <- bump stk
          poke stk v
          yield yld env henv activeThreads stk k
      | otherwise = die [] $ "applying non-function: " ++ show v
{-# INLINE apply #-}

jump ::
  (RuntimeProfiler prof) =>
  Ticker prof ->
  CCache prof ->
  HEnv ->
  ActiveThreads ->
  Stack ->
  K ->
  Args ->
  Closure ->
  IO ()
jump !yld env henv !activeThreads !stk !k !args clo = case clo of
  Captured sk0 a seg -> do
    let (p, sk) = adjust sk0
    seg <- closeArgs K stk seg args
    stk <- discardFrame stk
    stk <- dumpSeg stk seg $ F (countArgs args) a
    stk <- adjustArgs stk p
    repush yld env activeThreads stk henv sk k
  _ -> die [] "jump: non-cont"
  where
    -- Adjusts a repushed continuation to account for pending arguments. If
    -- there are any frames in the pushed continuation, the nearest one needs to
    -- record the additional pending arguments.
    --
    -- If the repushed continuation has no frames, then the arguments are still
    -- pending, and the result stacks need to be adjusted.
    adjust :: K -> (SZ, K)
    adjust (Mark a rs denv k) =
      (0, Mark (a + asize stk) rs denv k)
    adjust (Push n a cix f rsect k) =
      (0, Push n (a + asize stk) cix f rsect k)
    adjust k = (asize stk, k)
{-# INLINE jump #-}

repush ::
  (RuntimeProfiler prof) =>
  Ticker prof ->
  CCache prof ->
  ActiveThreads ->
  Stack ->
  HEnv ->
  K ->
  K ->
  IO ()
repush !yld env !activeThreads !stk (HEnv aenv denv0) = go denv0
  where
    go !denv KE !k
      -- Pending arguments. The continuation argument must be a function
      -- to be applied to them.
      | asize stk > 0 =
          peek stk
            >>= apply yld env henv activeThreads stk k False ZArgs
      | otherwise = yield yld env henv activeThreads stk k
      where
        henv = HEnv aenv denv
    go !denv (Mark a ps cs sk) !k = go denv' sk $ Mark a ps cs' k
      where
        denv' = cs <> EC.withoutKeys denv ps
        cs' = EC.restrictKeys denv ps
    go !denv (Push n a cix f rsect sk) !k =
      go denv sk $ Push n a cix f rsect k
    go !_ (Local {}) !_ = die [] "repush: captured Local frame"
    go !_ (AMark {}) !_ = die [] "repush: captured AMark frame"
    go !_ (Keep {}) !_ = die [] "repush: captured Keep frame"
    go !_ (CB _) !_ = die [] "repush: impossible"
{-# INLINE repush #-}

moveArgs ::
  Stack ->
  Args ->
  IO Stack
moveArgs !stk ZArgs = do
  stk <- discardFrame stk
  pure stk
moveArgs !stk (VArg1 i) = do
  stk <- prepareArgs stk (Arg1 i)
  pure stk
moveArgs !stk (VArg2 i j) = do
  stk <- prepareArgs stk (Arg2 i j)
  pure stk
moveArgs !stk (VArgR i l) = do
  stk <- prepareArgs stk (ArgR i l)
  pure stk
moveArgs !stk (VArgN as) = do
  stk <- prepareArgs stk (ArgN as)
  pure stk
moveArgs !stk (VArgV i) = do
  stk <-
    if l > 0
      then prepareArgs stk (ArgR 0 l)
      else discardFrame stk
  pure stk
  where
    l = fsize stk - i
{-# INLINE moveArgs #-}

closureArgs :: Stack -> Args -> IO [Val]
closureArgs !_ ZArgs = pure []
closureArgs !stk (VArg1 i) = do
  x <- peekOff stk i
  pure [x]
closureArgs !stk (VArg2 i j) = do
  x <- peekOff stk i
  y <- peekOff stk j
  pure [x, y]
closureArgs !stk (VArgR i l) =
  for (take l [i ..]) (peekOff stk)
closureArgs !stk (VArgN bs) =
  for (PA.primArrayToList bs) (peekOff stk)
closureArgs !_ _ =
  error "closure arguments can only be boxed."
{-# INLINE closureArgs #-}

-- | Pack some number of args into a data type of the provided ref/tag type.
buildData ::
  Stack -> Reference -> PackedTag -> Args -> IO Closure
buildData !_ !r !t ZArgs = pure $ Enum r t
buildData !stk !r !t (VArg1 i) = do
  v <- peekOff stk i
  pure $ Data1 r t v
buildData !stk !r !t (VArg2 i j) = do
  v1 <- peekOff stk i
  v2 <- peekOff stk j
  pure $ Data2 r t v1 v2
buildData !stk !r !t (VArgR i l) = do
  seg <- augSeg I stk nullSeg (Just $ ArgR i l)
  pure $ DataG r t seg
buildData !stk !r !t (VArgN as) = do
  seg <- augSeg I stk nullSeg (Just $ ArgN as)
  pure $ DataG r t seg
buildData !stk !r !t (VArgV i) = do
  seg <-
    if l > 0
      then augSeg I stk nullSeg (Just $ ArgR 0 l)
      else pure nullSeg
  pure $ DataG r t seg
  where
    l = fsize stk - i
{-# INLINE buildData #-}

-- | Pack some number of args into a record data type of the provided ref/tag type.
buildRec :: Stack -> Args -> IO Closure
buildRec !stk args = do
  -- TODO: Add more cases like buildData for efficiency
  seg <- augSeg I stk nullSeg (Just $ argsToArgs' args)
  pure $ RecordG seg
{-# INLINE buildRec #-}

dumpDataValNoTag ::
  Stack ->
  Val ->
  IO (PackedTag, Stack)
dumpDataValNoTag stk (BoxedVal c) =
  (closureTag c,) <$> dumpDataNoTag Nothing stk c
dumpDataValNoTag _ v =
  die [] $ "dumpDataValNoTag: unboxed val: " ++ show v
{-# INLINE dumpDataValNoTag #-}

-- Dumps a data type closure to the stack without writing its tag.
-- Instead, the tag is returned for direct case analysis.
dumpDataNoTag ::
  Maybe Reference ->
  Stack ->
  Closure ->
  IO Stack
dumpDataNoTag !mr !stk = \case
  -- Normally we want to avoid dumping unboxed values since it's unnecessary, but sometimes we don't know the type of
  -- the incoming value and end up dumping unboxed values, so we just push them back to the stack as-is. e.g. in type-casts/coercions
  Enum _ _ -> pure stk
  Data1 _ _ x -> do
    stk <- bump stk
    poke stk x
    pure stk
  Data2 _ _ x y -> do
    stk <- bumpn stk 2
    pokeOff stk 1 y
    stk <$ poke stk x
  DataG _ _ seg -> dumpSeg stk seg S
  clo ->
    die [3320] $
      "dumpDataNoTag: bad closure: "
        ++ show clo
        ++ maybe "" (\r -> "\nexpected type: " ++ show r) mr
{-# INLINE dumpDataNoTag #-}

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
closeArgs mode !stk !seg args = augSeg mode stk seg as
  where
    as = case args of
      ZArgs -> Nothing
      VArg1 i -> Just $ Arg1 i
      VArg2 i j -> Just $ Arg2 i j
      VArgR i l -> Just $ ArgR i l
      VArgN as -> Just $ ArgN as
      VArgV i -> a
        where
          a
            | l > 0 = Just $ ArgR 0 l
            | otherwise = Nothing
          l = fsize stk - i

yield ::
  (RuntimeProfiler prof) =>
  Ticker prof ->
  CCache prof ->
  HEnv ->
  ActiveThreads ->
  Stack ->
  K ->
  IO ()
yield !yld env henv0 !activeThreads !stk = leap
  where
    leap (Mark a ps cs k) | HEnv aenv0 denv0 <- henv0 = do
      denv <- evaluate $ cs <> EC.withoutKeys denv0 ps
      let h = denv0 EC.! EC.findMin ps
      v <- peek stk
      stk <- bump stk
      bpoke stk $ Data1 Rf.effectRef (PackedTag 0) v
      stk <- adjustArgs stk a
      henv <- evaluate $ HEnv aenv0 denv
      apply yld env henv activeThreads stk k False (VArg1 0) h
    leap (AMark a aenv (ARef r) k) = do
      v <- peek stk
      h <- BoxedVal <$> readIORef r
      stk <- bump stk
      bpoke stk $ Data1 Rf.effectRef (PackedTag 0) v
      stk <- adjustArgs stk a
      henv <- evaluate $ HEnv aenv mempty
      apply yld env henv activeThreads stk k False (VArg1 0) h
    leap (Push fsz asz cix f nx k) = do
      stk <- restoreFrame stk fsz asz
      stk <- ensure stk f
      eval yld env henv0 activeThreads stk k cix nx
    leap (Local henv asz k) = do
      stk <- restoreFrame stk 0 asz
      yield yld env henv activeThreads stk k
    leap (CB (Hook f)) = f (unpackXStack stk)
    leap (Keep _ asz k) = do
      stk <- restoreFrame stk 0 asz
      yield yld env henv0 activeThreads stk k
    leap KE = pure ()
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

-- Combined branch selection and field dumping function for data types.
-- Fields should only be dumped on _matches_, not default cases, because
-- default cases potentially cover many constructors which could result
-- in a variable number of values being put on the stack. Default cases
-- uniformly expect _no_ values to be added to the stack.
dataBranch ::
  Maybe Reference -> Stack -> MBranch -> Closure -> IO (MSection, Stack)
dataBranch mrf stk (Test1 u cu df) = \case
  Enum _ t
    | maskTags t == u -> pure (cu, stk)
    | otherwise -> pure (df, stk)
  Data1 _ t x
    | maskTags t == u -> do
        stk <- bump stk
        (cu, stk) <$ poke stk x
    | otherwise -> pure (df, stk)
  Data2 _ t x y
    | maskTags t == u -> do
        stk <- bumpn stk 2
        pokeOff stk 1 y
        (cu, stk) <$ poke stk x
    | otherwise -> pure (df, stk)
  DataG _ t seg
    | maskTags t == u -> (cu,) <$> dumpSeg stk seg S
    | otherwise -> pure (df, stk)
  Foreign (WrapMap m) -> case m of
    M.Bin sz k e l r
      | u == Rf.mapBin -> (cu,) <$> dumpBin sz k e l r stk
    M.Tip
      | u == Rf.mapTip -> pure (cu, stk)
    _ -> pure (df, stk)
  clo -> (df, stk) <$ dataBranchClosureError mrf clo
dataBranch mrf stk (Test2 u cu v cv df) = \case
  Enum _ t
    | maskTags t == u -> pure (cu, stk)
    | maskTags t == v -> pure (cv, stk)
    | otherwise -> pure (df, stk)
  Data1 _ t x
    | maskTags t == u -> do
        stk <- bump stk
        (cu, stk) <$ poke stk x
    | maskTags t == v -> do
        stk <- bump stk
        (cv, stk) <$ poke stk x
    | otherwise -> pure (df, stk)
  Data2 _ t x y
    | maskTags t == u -> do
        stk <- bumpn stk 2
        pokeOff stk 1 y
        (cu, stk) <$ poke stk x
    | maskTags t == v -> do
        stk <- bumpn stk 2
        pokeOff stk 1 y
        (cv, stk) <$ poke stk x
    | otherwise -> pure (df, stk)
  DataG _ t seg
    | maskTags t == u -> (cu,) <$> dumpSeg stk seg S
    | maskTags t == v -> (cv,) <$> dumpSeg stk seg S
    | otherwise -> pure (df, stk)
  Foreign (WrapMap m) -> case m of
    M.Bin sz k e l r
      | u == Rf.mapBin -> (cu,) <$> dumpBin sz k e l r stk
      | v == Rf.mapBin -> (cv,) <$> dumpBin sz k e l r stk
    M.Tip
      | u == Rf.mapTip -> pure (cu, stk)
      | v == Rf.mapTip -> pure (cv, stk)
    _ -> pure (df, stk)
  clo -> (df, stk) <$ dataBranchClosureError mrf clo
dataBranch mrf stk (TestW df bs) = \case
  Enum _ t
    | Just ca <- EC.lookup (maskTags t) bs -> pure (ca, stk)
    | otherwise -> pure (df, stk)
  Data1 _ t x
    | Just ca <- EC.lookup (maskTags t) bs -> do
        stk <- bump stk
        (ca, stk) <$ poke stk x
    | otherwise -> pure (df, stk)
  Data2 _ t x y
    | Just ca <- EC.lookup (maskTags t) bs -> do
        stk <- bumpn stk 2
        pokeOff stk 1 y
        (ca, stk) <$ poke stk x
    | otherwise -> pure (df, stk)
  DataG _ t seg
    | Just ca <- EC.lookup (maskTags t) bs ->
        (ca,) <$> dumpSeg stk seg S
    | otherwise -> pure (df, stk)
  Foreign (WrapMap m) -> case m of
    M.Bin sz k e l r
      | Just ca <- EC.lookup Rf.mapBin bs ->
          (ca,) <$> dumpBin sz k e l r stk
    M.Tip
      | Just ca <- EC.lookup Rf.mapTip bs ->
          pure (ca, stk)
    _ -> pure (df, stk)
  clo -> (df, stk) <$ dataBranchClosureError mrf clo
dataBranch _ _ br = \_ ->
  dataBranchBranchError br
{-# INLINE dataBranch #-}

dumpBin :: Int -> Val -> Val -> Map Val Val -> Map Val Val -> Stack -> IO Stack
dumpBin sz k e l r stk = do
  stk <- bumpn stk 5
  unsafePokeIasN stk sz
  pokeOff stk 1 k
  pokeOff stk 2 e
  pokeOffBi stk 3 l
  pokeOffBi stk 4 r
  pure stk
{-# INLINE dumpBin #-}

prettyRef :: Reference -> String
prettyRef = Text.unpack . showShort 10

dataBranchClosureError ::
  Maybe Reference -> Closure -> IO ()
dataBranchClosureError (Just rftgt) (DataC rf _ _)
  | rftgt /= rf =
      die [] $
        "dataBranch: type mismatch detected\n"
          <> "    expected: "
          <> prettyRef rftgt
          <> "\n"
          <> "    received: "
          <> prettyRef rf
dataBranchClosureError _ (DataC rf t _) =
  die [] $
    "dataBranch: unexpected tag for data type\n"
      <> "    type: "
      <> prettyRef rf
      <> "\n"
      <> "    data tag: "
      <> show (maskTags t)
dataBranchClosureError mrf clo =
  die [] $
    "dataBranch: unexpected closure type\n"
      <> expected
      <> "but instead I received "
      <> description
  where
    expected = case mrf of
      Just rftgt ->
        "    expected type: " <> prettyRef rftgt <> "\n  "
      Nothing -> "I expected a data type, "
    description = case clo of
      PAp {} -> "a partially applied function"
      Captured {} -> "a continuation"
      Affine {} -> "an affine handler info"
      BlackHole -> "a black hole"
      UnboxedTypeTag CharTag -> "a character"
      UnboxedTypeTag FloatTag -> "a floating point number"
      UnboxedTypeTag IntTag -> "an integer"
      UnboxedTypeTag NatTag -> "a natural number"
      Foreign (foreignRef -> rf) ->
        "a builtin value of type `" <> prettyRef rf <> "`"
      RecordG {} -> "a record"

dataBranchBranchError :: MBranch -> IO a
dataBranchBranchError br =
  die [] $ "dataBranch: unexpected branch: " <> show br

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
  IO (Val, DEnv, Stack, K)
splitCont !denv !stk !k !p =
  walk denv asz KE k
  where
    asz = asize stk
    walk :: DEnv -> SZ -> K -> K -> IO (Val, DEnv, Stack, K)
    walk !denv !sz !ck KE =
      die [] "fell off stack" >> finish denv sz 0 ck KE
    walk !denv !sz !ck (CB _) =
      die [] "fell off stack" >> finish denv sz 0 ck KE
    walk !denv !sz !ck (Local {}) =
      die [] "splitCont: Local frame" >> finish denv sz 0 ck KE
    walk !denv !sz !ck (AMark {}) =
      die [] "splitCont: AMark frame" >> finish denv sz 0 ck KE
    walk !denv !sz !ck (Keep {}) =
      die [] "splitCont: Keep frame" >> finish denv sz 0 ck KE
    walk !denv !sz !ck (Mark a ps cs k)
      | EC.member p ps = finish denv' sz a ck k
      | otherwise = walk denv' (sz + a) (Mark a ps cs' ck) k
      where
        denv' = cs <> EC.withoutKeys denv ps
        cs' = EC.restrictKeys denv ps
    walk !denv !sz !ck (Push n a br p brSect k) =
      walk
        denv
        (sz + n + a)
        (Push n a br p brSect ck)
        k

    finish :: DEnv -> SZ -> SZ -> K -> K -> IO (Val, DEnv, Stack, K)
    finish !denv !sz !a !ck !k = do
      (seg, stk) <- grabSeg stk sz
      stk <- adjustArgs stk a
      return (BoxedVal $ Captured ck asz seg, denv, stk, k)
{-# INLINE splitCont #-}

abortCont ::
  Stack ->
  K ->
  AffineRef ->
  IO (AEnv, Stack, K)
abortCont !stk !k !r = walk (asize stk) k
  where
    walk :: SZ -> K -> IO (AEnv, Stack, K)
    walk !sz = \case
      KE -> die [] "abortCont: fell off stack"
      (CB _) -> die [] "abortCont: fell off stack"
      (Local _ a k) -> walk (sz + a) k
      (Push n a _ _ _ k) -> walk (sz + n + a) k
      (Keep _ a k) -> walk (sz + a) k
      -- dynamic mark cannot match
      (Mark a _ _ k) -> walk (sz + a) k
      (AMark a aenv s k)
        | r == s -> finish aenv sz a k
        | otherwise -> walk (sz + a) k

    finish :: AEnv -> SZ -> SZ -> K -> IO (AEnv, Stack, K)
    finish !aenv !sz !a !k = do
      stk <- truncateSeg stk sz
      stk <- adjustArgs stk a
      pure (aenv, stk, k)
{-# INLINE abortCont #-}

resolve :: CCache p -> HEnv -> Stack -> MRef -> IO Val
resolve _ _ _ (Env cix mcomb) = pure (mCombVal cix mcomb)
resolve _ _ stk (Stk i) = peekOff stk i
resolve env (HEnv aenv denv) _ (Dyn i)
  | Just v <- EC.lookup i denv = pure v
  | Just (ARef r) <- EC.lookup i aenv = BoxedVal <$> readIORef r
  | otherwise = unhandledErr "resolve" env i
{-# INLINE resolve #-}

unhandledErr :: String -> CCache p -> Word64 -> IO a
unhandledErr fname env i =
  readTVarIO (tagRefs env) >>= \rs -> case EC.lookup i rs of
    Just r -> bomb (show r)
    Nothing -> bomb (show i)
  where
    bomb sh = die [] $ fname ++ ": unhandled ability request: " ++ sh

rCombSection :: EnumMap Word64 MCombs -> CombIx -> MComb
rCombSection combs (CIx r n i) =
  case EC.lookup n combs of
    Just cmbs -> case EC.lookup i cmbs of
      Just cmb -> RComb cmb
      Nothing -> error $ "unknown section `" ++ show i ++ "` of combinator `" ++ show n ++ "`. Reference: " ++ show r
    Nothing -> error $ "unknown combinator `" ++ show n ++ "`. Reference: " ++ show r

resolveSection :: CCache p -> Section -> IO MSection
resolveSection cc section = do
  rcombs <- readTVarIO (combs cc)
  pure $ rCombSection rcombs <$> section

dummyRef :: Reference
dummyRef = Builtin (DTx.pack "dummy")

updateMap :: (Semigroup s) => s -> TVar s -> STM s
updateMap new0 r = do
  new <- evaluateSTM new0
  stateTVar r $ \old ->
    let total = new <> old in (total, total)

decodeCacheArgument :: USeq -> IO [(Reference, Code Reference)]
decodeCacheArgument s = traverse (f <=< decodeVal) $ toList s
  where
    f (Ref r, rco) = pure (r, dereference rco)
    f _ = die [] "decodeCacheArgument: Con reference"

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

-- Just evaluating to force exceptions. Shouldn't actually be that
-- unsafe.
evaluateSTM :: a -> STM a
evaluateSTM x = unsafeIOToSTM (evaluate x)

-- If this flag is set, all code is run through serialization before
-- loading. This renames variables, and it's possible a problem would
-- only be visible with the renamed variables. This allows testing
-- these cases just by rebuilding ucm, rather than actually concocting
-- a test that involves remote code loading.
#if defined(CODE_SERIAL_CHECK)

normalizeCode :: Code Reference -> Code Reference
normalizeCode co = case deserializeCode (serializeCode False co) of
  Left _ -> error "normalizeCode: impossible"
  Right co -> co

normalizeCodes ::
  [(Reference, Code Reference)] -> [(Reference, Code Reference)]
normalizeCodes = fmap $ second normalizeCode

#else

normalizeCodes ::
  [(Reference, Code Reference)] -> [(Reference, Code Reference)]
normalizeCodes = id

#endif

cacheAdd0 ::
  (RuntimeProfiler p) =>
  S.Set Reference ->
  [(Reference, Code Reference)] ->
  [(Reference, Set Reference)] ->
  CCache p ->
  IO ()
cacheAdd0 ntys0 (normalizeCodes -> termSuperGroups) sands cc = do
  let toAdd = M.fromList (termSuperGroups <&> second codeGroup)
  (unresolvedCacheableCombs, unresolvedNonCacheableCombs) <- atomically $ do
    have <- readTVar (intermed cc)
    let new = M.difference toAdd have
    let sz = fromIntegral $ M.size new
    let rs = M.keys new
    int <- updateMap new (intermed cc)
    let replace =
          ANF.replaceConstructors pseudoConstructors
            . ANF.replaceFunctions functionReplacements
        haff (cmbs, opts) =
          (M.mapWithKey (ANF.optimizeHandler Builtin opts) cmbs, opts)
    opt <-
      stateTVar (optInfos cc) $ haff . ANF.optimize (fmap replace new)
    rty <- addRefs (freshTy cc) (refTy cc) (tagRefs cc) ntys0
    ntm <- stateTVar (freshTm cc) $ \i -> (i, i + sz)
    rtm <- updateMap (M.fromList $ zip rs [ntm ..]) (refTm cc)
    -- TODO: Need to populate with new field values
    fieldtm <- readTVar (fieldNums cc)
    -- check for missing references
    let arities = fmap (head . ANF.arities) int <> builtinArities
        rns = RN (refLookup "ty" rty) (refLookup "tm" rtm) (flip M.lookup arities) (fieldNameLookup fieldtm)
        combinate :: Word64 -> (Reference, SuperGroup Reference Symbol) -> (Word64, EnumMap Word64 Comb)
        combinate n (r, g) = (n, emitCombs rns r n g)
    let combRefUpdates = (mapFromList $ zip [ntm ..] rs)
    let combIdFromRefMap = (M.fromList $ zip rs [ntm ..])
    let newCacheableCombs =
          termSuperGroups
            & mapMaybe
              ( \case
                  (ref, CodeRep _ Cacheable) ->
                    M.lookup ref combIdFromRefMap
                  _ -> Nothing
              )
            & EC.setFromList
    newCombRefs <- updateMap combRefUpdates (combRefs cc)
    (unresolvedNewCombs, unresolvedCacheableCombs, unresolvedNonCacheableCombs, updatedCombs) <- stateTVar (combs cc) \oldCombs ->
      let unresolvedNewCombs :: EnumMap Word64 (GCombs any CombIx)
          unresolvedNewCombs =
            absurdCombs
              . sanitizeCombsOfForeignFuncs (sandboxed cc) sandboxedForeignFuncs
              . mapFromList
              $ zipWith combinate [ntm ..] (M.toList opt)
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

preEvalTopLevelConstants ::
  (RuntimeProfiler p) =>
  (EnumMap Word64 (GCombs Val CombIx)) ->
  (EnumMap Word64 (GCombs Val CombIx)) ->
  CCache p ->
  IO ()
preEvalTopLevelConstants cacheableCombs newCombs cc = do
  activeThreads <- Just <$> UnliftIO.newIORef mempty
  evaluatedCacheableCombsVar <- newTVarIO mempty
  for_ (EC.mapToList cacheableCombs) \(w, _) -> do
    let hook xstk = do
          val <- peek (packXStack xstk)
          atomically $ do
            modifyTVar evaluatedCacheableCombsVar $ EC.mapInsert w (EC.mapSingleton 0 $ CachedVal w val)
    apply0 (Just hook) cc activeThreads w
      `catch` \e ->
        -- ignore sandboxing exceptions during pre-eval, in case they
        -- don't matter for the final result.
        if isSandboxingException e
          then pure ()
          else throwIO e

  evaluatedCacheableCombs <- readTVarIO evaluatedCacheableCombsVar
  let allNew = evaluatedCacheableCombs <> newCombs
  -- Rewrite all the inlined combinator references to point to the
  -- new cached versions.
  atomically $ modifyTVar (combs cc) (\existingCombs -> (resolveCombs (Just $ EC.mapDifference existingCombs allNew) allNew) <> existingCombs)

-- Checks if a runtime exception is due to sandboxing.
--
-- This is used above during pre-evaluation, to ignore sandboxing
-- exceptions for top-level constant dependencies of docs and such, in
-- case the docs don't actually evaluate them.
isSandboxingException :: RuntimeExn -> Bool
isSandboxingException (PE _ _ (P.toPlain 0 -> msg)) =
  Text.isPrefixOf sdbx1 msg || Text.isPrefixOf sdbx2 msg
  where
    sdbx1 = "attempted to use sandboxed operation"
    sdbx2 = "Attempted to use disallowed builtin in sandboxed"
isSandboxingException _ = False

expandSandbox ::
  Map Reference (Set Reference) ->
  [(Reference, SuperGroup Reference Symbol)] ->
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
  (RuntimeProfiler p) =>
  [(Reference, Code Reference)] ->
  CCache p ->
  IO [Reference]
cacheAdd l cc = do
  rtm <- readTVarIO (refTm cc)
  rty <- readTVarIO (refTy cc)
  sand <- readTVarIO (sandbox cc)
  let known = M.keysSet rtm <> S.fromList (view _1 <$> l)
      f b r
        | not b, S.notMember r known = Const (S.singleton r, mempty)
        | b, M.notMember r rty = Const (mempty, S.singleton r)
        | otherwise = Const (mempty, mempty)
      (missing, tys) =
        getConst $ (foldMap . foldMap . foldGroup) (foldGroupLinks f) l
      l'' = filter (\(r, _) -> M.notMember r rtm) l
      l' = map (second codeGroup) l''
  if S.null missing
    then [] <$ cacheAdd0 tys l'' (expandSandbox sand l') cc
    else pure $ S.toList missing

data ReflectionState = RS
  { _tyNums :: HM.HashMap Word64 RefNum,
    _tmNums :: HM.HashMap Word64 RefNum,
    _canonST :: {-# UNPACK #-} !CanonST
  }

type Reflect = StateT ReflectionState IO

emptyRS :: ReflectionState
emptyRS = RS HM.empty HM.empty emptyCST

mediate :: Canonize a -> Reflect a
mediate act = StateT \(RS sty stm cst) ->
  second (RS sty stm) <$> runStateT act cst
{-# INLINE mediate #-}

canonicalizeReference :: Bool -> Reference -> Reflect RefNum
canonicalizeReference isTy = mediate . resolveRef isTy

canonicalizeReferent :: Referent -> Reflect (Referent' RefNum)
canonicalizeReferent = mediate . canonicalizeRefs

canonicalizeReferenced ::
  (Referential t) => Referenced t -> Reflect (t RefNum)
canonicalizeReferenced x = mediate $ recanonicalizeRefs x
{-# INLINE canonicalizeReferenced #-}

reflectValue :: CCache p -> Val -> IO (Referenced ANF.Value)
reflectValue env val = do
  tyr <- readTVarIO (tagRefs env)
  tmr <- readTVarIO (combRefs env)
  reflectValue0 tyr tmr val
    `catch` \(ReflectExn problem) ->
      die [] $ err problem rendered
  where
    err s v =
      "reflectValue: cannot prepare value for serialization: "
        ++ s
        ++ "\n\nSerialized value:\n\n"
        ++ v

    rendered = case tracer env False val of
      NoTrace -> show val
      MsgTrace _ _ pre -> pre
      SimpleTrace ugl -> ugl

reflExn :: String -> Reflect a
reflExn msg = lift . throwIO $ ReflectExn msg

-- Converts the numbered reference map to a renumbering operation for
-- reflection.
resolveTy :: EnumMap Word64 Reference -> Word64 -> Reflect RefNum
resolveTy rty w =
  StateT \st@(RS seenty seentm cst) ->
    case HM.lookup w seenty of
      Just r -> pure (r, st)
      -- if we haven't seen the number before, we need to consult the
      -- reference map and canonicalizers.
      Nothing
        | Just r <- EC.lookup w rty -> do
            (rn, cst) <- runStateT (resolveRef True r) cst
            seenty <- pure $ HM.insert w rn seenty
            st <- pure $ RS seenty seentm cst
            pure (rn, st)
        | otherwise -> throwIO $ ReflectExn "unknown type reference"

-- Converts the numbered refence map to a renumbering operation for
-- reflection, with function unreplacements considered.
resolveTm :: EnumMap Word64 Reference -> Word64 -> Reflect RefNum
resolveTm rtm w =
  StateT \st@(RS seenty seentm cst) ->
    case HM.lookup w seentm of
      Just r -> pure (r, st)
      -- if we haven't seen the number before, we need to consult the
      -- reference map and canonicalizers.
      Nothing
        | Just r <- EC.lookup w rtm,
          r <- M.findWithDefault r r functionUnreplacements -> do
            (rn, cst) <- runStateT (resolveRef False r) cst
            seentm <- pure $ HM.insert w rn seentm
            st <- pure $ RS seenty seentm cst
            pure (rn, st)
        | otherwise -> throwIO $ ReflectExn "unknown term reference"

-- Reflects a runtime value into an interchange value, given a mapping
-- from numberings to references.
--
-- Note
-- ----
--
-- There is some difficulty with reflecting a value that has already
-- had its references resolved. It is possible to reflect a value that
-- contains a reflected value, and the latter _might_ not have been
-- produced with the same in-memory references as the numbering. This
-- would be the case if the value has been produced by
-- deserialization.
--
-- So, there is an extra canonicalization step that takes place to
-- choose unique `Reference` values over the entire value. Cost for
-- numberings is avoided because we locally remember (in a hash map)
-- the canonical value the first time we see each number. Making the
-- value overall canonical might require some substitution in the
-- embedded values (or code), which could be costly. To avoid that
-- cost, avoid having lots of nested reflected values.
reflectValue0 ::
  EnumMap Word64 Reference ->
  EnumMap Word64 Reference ->
  Val ->
  IO (Referenced ANF.Value)
reflectValue0 rty rtm = goV0
  where
    goIx (CIx _ top i) = flip ANF.GR i <$> resolveTm rtm top

    finish (val, RS _ _ (CST _ _ _ tys tms)) =
      WithRefs (toList tys) (toList tms) val

    goV0 :: Val -> IO (Referenced ANF.Value)
    goV0 v = finish <$> runStateT (goV v) emptyRS

    goVs :: Seg -> Reflect [ANF.Value RefNum]
    goVs sg = traverseAccumSegToList goV sg

    goV :: Val -> Reflect (ANF.Value RefNum)
    goV = \case
      -- For back-compatibility we reflect all Unboxed values into boxed literals, we could change this in the future,
      -- but there's not much of a big reason to.

      NatVal n -> pure . ANF.BLit $ ANF.Pos n
      IntVal n
        | n >= 0 -> pure . ANF.BLit $ ANF.Pos (fromIntegral n)
        | otherwise -> pure . ANF.BLit $ ANF.Neg (fromIntegral (abs n))
      DoubleVal f -> pure . ANF.BLit $ ANF.Float f
      CharVal c -> pure . ANF.BLit $ ANF.Char c
      Val _ clos ->
        case clos of
          PAp cix _rComb args ->
            ANF.Partial <$> goIx cix <*> goVs args
          Enum _ t -> do
            r <- resolveTy rty $ TT.typeTag t
            pure $ ANF.Data r (maskTags t) []
          Data1 _ t u -> do
            r <- resolveTy rty $ TT.typeTag t
            u <- goV u
            pure $ ANF.Data r (maskTags t) [u]
          Data2 _ t u v -> do
            r <- resolveTy rty $ TT.typeTag t
            u <- goV u
            v <- goV v
            pure $ ANF.Data r (maskTags t) [u, v]
          DataG _ t seg -> do
            r <- resolveTy rty $ TT.typeTag t
            ANF.Data r (maskTags t) <$> goVs seg
          RecordG _args -> error "reflectValue: Record reflection not yet implemented"
          Captured k _ segs ->
            ANF.Cont <$> goVs segs <*> goK k
          Foreign f -> ANF.BLit <$> goF f
          BlackHole -> reflExn "black hole"
          UnboxedTypeTag {} ->
            reflExn "unknown unboxed value"
          Affine {} -> reflExn "affine info"

    goK (CB _) = reflExn "callback continuation"
    goK (Local {}) = reflExn "captured Local frame"
    goK (AMark {}) = reflExn "captured AMark frame"
    goK (Keep {}) = reflExn "captured Keep frame"
    goK KE = pure ANF.KE
    goK (Mark a ps de k) = do
      ps <- traverse (resolveTy rty) (EC.setToList ps)
      de <- traverse (\(k, v) -> (,) <$> resolveTy rty k <*> goV v) (mapToList de)
      ANF.Mark (fromIntegral a) ps de <$> goK k
    goK (Push f a cix _ _rsect k) =
      ANF.Push
        (fromIntegral f)
        (fromIntegral a)
        <$> goIx cix
        <*> goK k

    goF = \case
      WrapText t -> pure (ANF.Text t)
      WrapBytes b -> pure (ANF.Bytes b)
      WrapSeq s -> ANF.List <$> traverse goV s
      WrapReferent l -> ANF.TmLink <$> canonicalizeReferent l
      WrapReference l -> ANF.TyLink <$> canonicalizeReference True l
      WrapValue v -> ANF.Quote <$> canonicalizeReferenced v
      WrapCode g -> ANF.Code <$> canonicalizeReferenced g
      WrapByteArray a -> pure (ANF.BArr a)
      WrapArray a -> ANF.Arr <$> traverse goV a
      WrapMap m ->
        ANF.Map
          <$> traverse (\(k, v) -> (,) <$> goV k <*> goV v) (M.toList m)
      WrapInteger i -> pure (ANF.BigInt i)
      WrapNatural n -> pure (ANF.BigNat n)
      _ -> reflExn "foreign value"

data ReflectExn = ReflectExn String deriving (Show)

instance Exception ReflectExn

ixArr :: String -> Array a -> RefNum -> IO a
ixArr pfx arr (RefNum i)
  | 0 <= i, i < sizeofArray arr = indexArrayM arr i
  | otherwise = die [] . (pfx ++) $ " index out of bounds: " ++ show i
{-# INLINE ixArr #-}

reifyValue ::
  CCache p -> Referenced ANF.Value -> IO (Either [Reference] Val)
reifyValue cc val = do
  (tyLinks, tmLinks) <- case val of
    Plain v -> pure $ collectValueLinks v
    WithRefs tys tms v -> do
      let tya = arrayFromList tys
          tma = arrayFromList tms
          (tyns, tmns) = collectValueLinks v
          travSet f = fmap S.fromList . traverse f . S.toList
      (,)
        <$> travSet (ixArr "reifyValue: type" tya) tyns
        <*> travSet (ixArr "reifyValue: term" tma) tmns
  erc <-
    atomically $ do
      combs <- readTVar (combs cc)
      rtm <- readTVar (refTm cc)
      case S.toList $ S.filter (`M.notMember` rtm) tmLinks of
        [] -> do
          newTy <- addRefs (freshTy cc) (refTy cc) (tagRefs cc) tyLinks
          pure . Right $ (combs, newTy, rtm)
        l -> pure (Left l)
  traverse (\rfs -> reifyValue1 rfs val) erc

reifyValue1 ::
  (EnumMap Word64 MCombs, M.Map Reference Word64, M.Map Reference Word64) ->
  Referenced ANF.Value ->
  IO Val
reifyValue1 tup (Plain v) = reifyValue0 tup v
reifyValue1 (combs, rty0, rtm0) (WithRefs tys tms v) = do
  let rty = HM.fromList . mapMaybe procTypeRefs $ zip [0 ..] tys
      rtm = HM.fromList . mapMaybe procTermRefs $ zip [0 ..] tms
  reifyValue0Canon combs tys tms rty rtm v
  where
    procTypeRefs (i, r) = (RefNum i,) <$> M.lookup r rty0
    procTermRefs (i, r) =
      (RefNum i,)
        <$> M.lookup (M.findWithDefault r r functionReplacements) rtm0

reifyValue0Canon ::
  EnumMap Word64 MCombs ->
  [Reference] ->
  [Reference] ->
  HM.HashMap RefNum Word64 ->
  HM.HashMap RefNum Word64 ->
  ANF.Value RefNum ->
  IO Val
reifyValue0Canon combs tys tms rty rtm = goV
  where
    err s = "reifyValue: cannot restore value: " ++ s

    !tya = arrayFromList tys
    !tma = arrayFromList tms

    ixTy (RefNum i)
      | 0 <= i, i < sizeofArray tya = pure $ indexArray tya i
      | otherwise = die [] . err $ "type ref index out of bounds: " ++ show i

    ixTm (RefNum i)
      | 0 <= i, i < sizeofArray tma = pure $ indexArray tma i
      | otherwise = die [] . err $ "term ref index out of bounds: " ++ show i

    numToRef :: Bool -> RefNum -> IO Reference
    numToRef True = ixTy
    numToRef False = ixTm

    refTy r = case HM.lookup r rty of
      Just w -> pure w
      _ -> die [] . err $ "unknown type reference: " ++ show r

    refTm r = case HM.lookup r rtm of
      Just w -> pure w
      _ -> die [] . err $ "unknown term reference: " ++ show r

    goIx :: ANF.GroupRef RefNum -> IO (CombIx, MComb)
    goIx (ANF.GR rn i) = do
      n <- refTm rn
      rf <- ixTm rn
      let cix = (CIx rf n i)
      pure (cix, rCombSection combs cix)

    goVs :: [ANF.Value RefNum] -> IO Seg
    goVs vs = traverseListToSeg goV vs

    goVArr :: Array (ANF.Value RefNum) -> IO (Array Val)
    goVArr vs = traverseArrayIO goV vs

    goVSeq :: Seq (ANF.Value RefNum) -> IO (Seq Val)
    goVSeq vs = traverse goV vs

    goV :: ANF.Value RefNum -> IO Val
    goV (ANF.Partial gr vs) =
      goIx gr >>= \case
        (cix, RComb (Comb rcomb)) ->
          boxedVal . PAp cix rcomb <$> goVs vs
        (_, RComb (CachedVal _ val))
          | [] <- vs -> pure val
          | otherwise -> die [] . err $ msg
          where
            msg = "reifyValue0: non-trivial partial application to cached value"
    goV (ANF.Data rn t0 vs) = do
      t <- flip packTags (fromIntegral t0) . fromIntegral <$> refTy rn
      rf <- ixTy rn
      boxedVal . formDataReplaced rf t <$> goVs vs
    goV (ANF.Cont vs k) = do
      k' <- goK k
      vs' <- goVs vs
      pure . boxedVal $ cv k' vs'
      where
        cv k s = Captured k a s
          where
            ksz = frameDataSize k
            a = fromIntegral $ length s - ksz
    goV (ANF.BLit l) = goL l

    goK ANF.KE = pure KE
    goK (ANF.Mark a ps de k) =
      mrk
        <$> traverse refTy ps
        <*> traverse (\(k, v) -> (,) <$> refTy k <*> (goV v)) de
        <*> goK k
      where
        mrk ps de k =
          Mark (fromIntegral a) (setFromList ps) (mapFromList de) k
    goK (ANF.Push f a gr k) =
      goIx gr >>= \case
        (cix, RComb (Lam _ fr sect)) ->
          Push
            (fromIntegral f)
            (fromIntegral a)
            cix
            fr
            sect
            <$> goK k
        (CIx r _ _, _) ->
          die [] . err $
            "tried to reify a continuation with a cached value resumption"
              ++ show r

    goL :: ANF.BLit RefNum -> IO Val
    goL (ANF.Text t) = pure $ encodeVal t
    goL (ANF.List l) = encodeVal <$> goVSeq l
    goL (ANF.TmLink r) = encodeVal <$> traverseRefs numToRef r
    goL (ANF.TyLink r) = encodeVal <$> ixTy r
    goL (ANF.Bytes b) = pure $ encodeVal b
    goL (ANF.Quote v) = pure $ encodeVal (WithRefs tys tms v)
    goL (ANF.Code g) = pure $ encodeVal (WithRefs tys tms g)
    goL (ANF.BArr a) = pure $ encodeVal a
    goL (ANF.Char c) = pure $ CharVal c
    goL (ANF.Pos w) =
      -- TODO: Should this be a Nat or an Int?
      pure $ NatVal w
    goL (ANF.Neg w) = pure $ IntVal (negate (fromIntegral w :: Int))
    goL (ANF.Float d) = pure $ DoubleVal d
    goL (ANF.Arr a) = encodeVal <$> goVArr a
    goL (ANF.Map l) = encodeVal . M.fromList <$> traverse goP l
      where
        goP (x, y) = (,) <$> goV x <*> goV y
    goL (ANF.BigInt i) = pure $ encodeVal i
    goL (ANF.BigNat n) = pure $ encodeVal n

reifyValue0 ::
  (EnumMap Word64 MCombs, M.Map Reference Word64, M.Map Reference Word64) ->
  ANF.Value Reference ->
  IO Val
reifyValue0 (combs, rty, rtm) = goV
  where
    err s = "reifyValue: cannot restore value: " ++ s
    refTy r
      | Just w <- M.lookup r rty = pure w
      | otherwise = die [] . err $ "unknown type reference: " ++ show r
    refTm r
      | Just w <- M.lookup r rtm = pure w
      | otherwise = die [] . err $ "unknown term reference: " ++ show r
    goIx :: ANF.GroupRef Reference -> IO (CombIx, MComb)
    goIx (ANF.GR r0 i) =
      refTm r <&> \n ->
        let cix = (CIx r n i)
         in (cix, rCombSection combs cix)
      where
        r = M.findWithDefault r0 r0 functionReplacements

    goVs :: [ANF.Value Reference] -> IO Seg
    goVs vs = traverseListToSeg goV vs

    goVArr :: Array (ANF.Value Reference) -> IO (Array Val)
    goVArr vs = traverseArrayIO goV vs

    goV :: ANF.Value Reference -> IO Val
    goV (ANF.Partial gr vs) =
      goIx gr >>= \case
        (cix, RComb (Comb rcomb)) ->
          boxedVal . PAp cix rcomb <$> goVs vs
        (_, RComb (CachedVal _ val))
          | [] <- vs -> pure val
          | otherwise -> die [] . err $ msg
          where
            msg = "reifyValue0: non-trivial partial application to cached value"
    goV (ANF.Data r t0 vs) = do
      t <- flip packTags (fromIntegral t0) . fromIntegral <$> refTy r
      boxedVal . formDataReplaced r t <$> goVs vs
    goV (ANF.Cont vs k) = do
      k' <- goK k
      vs' <- goVs vs
      pure . boxedVal $ cv k' vs'
      where
        cv k s = Captured k a s
          where
            ksz = frameDataSize k
            a = fromIntegral $ length s - ksz
    goV (ANF.BLit l) = goL l

    goK ANF.KE = pure KE
    goK (ANF.Mark a ps de k) =
      mrk
        <$> traverse refTy ps
        <*> traverse (\(k, v) -> (,) <$> refTy k <*> (goV v)) de
        <*> goK k
      where
        mrk ps de k =
          Mark (fromIntegral a) (setFromList ps) (mapFromList de) k
    goK (ANF.Push f a gr k) =
      goIx gr >>= \case
        (cix, RComb (Lam _ fr sect)) ->
          Push
            (fromIntegral f)
            (fromIntegral a)
            cix
            fr
            sect
            <$> goK k
        (CIx r _ _, _) ->
          die [] . err $
            "tried to reify a continuation with a cached value resumption"
              ++ show r

    goL :: ANF.BLit Reference -> IO Val
    goL (ANF.Text t) = pure $ encodeVal t
    goL (ANF.List l) = boxedVal . Foreign . WrapSeq <$> traverse goV l
    goL (ANF.TmLink r) = pure $ encodeVal r
    goL (ANF.TyLink r) = pure $ encodeVal r
    goL (ANF.Bytes b) = pure $ encodeVal b
    goL (ANF.Quote v) = pure $ encodeVal (Plain v)
    goL (ANF.Code g) = pure $ encodeVal (Plain g)
    goL (ANF.BArr a) = pure $ encodeVal a
    goL (ANF.Char c) = pure $ CharVal c
    goL (ANF.Pos w) =
      -- TODO: Should this be a Nat or an Int?
      pure $ NatVal w
    goL (ANF.Neg w) = pure $ IntVal (negate (fromIntegral w :: Int))
    goL (ANF.Float d) = pure $ DoubleVal d
    goL (ANF.Arr a) = encodeVal <$> goVArr a
    goL (ANF.Map l) = encodeVal . M.fromList <$> traverse goP l
      where
        goP (x, y) = (,) <$> goV x <*> goV y
    goL (ANF.BigInt i) = pure $ encodeVal i
    goL (ANF.BigNat n) = pure $ encodeVal n

#ifdef OPT_CHECK
-- Assert that we don't allocate any 'Stack' objects in 'eval', since we expect GHC to always
-- trigger the worker/wrapper optimization and unbox it fully, and if it fails to do so, we want to
-- know about it.
--
-- Note: this must remain in this module, it can't be moved to a testing module, this is a requirement of the inspection
-- testing library.
--
-- Note: We _must_ check 'eval0' instead of 'eval' here because if you simply check 'eval', you'll be
-- testing the 'wrapper' part of the worker/wrapper, which will always mention the 'Stack' object as part of its
-- unwrapping, and since there's  no way to refer to the generated wrapper directly, we instead refer to 'eval0'
-- which allocates its own stack to pass in, meaning it's one level above the wrapper, and GHC should always detect that
-- it can call the worker directly without using the wrapper.
-- See: https://github.com/nomeata/inspection-testing/issues/50 for more information.
--
-- If this test starts failing, here are some things you can check.
--
-- 1. Are 'Stack's being passed to dynamic functions? If so, try changing those functions to take an 'XStack' instead,
--    and manually unpack/pack the 'Stack' where necessary.
-- 2. Are there calls to 'die' or 'throwIO' or something similar in which a fully polymorphic type variable is being
--    specialized to 'Stack'? Sometimes this trips up the optimization, you can try using an 'error' instead, or even
--    following the 'throwIO' with a useless call to @error "unreachable"@, this seems to help for some reason.
--    See this page for more info on precise exceptions: https://gitlab.haskell.org/ghc/ghc/-/wikis/exceptions/precise-exceptions
--
-- Best of luck!
TI.inspect $ 'eval0 `TI.hasNoType` ''Stack
#endif
