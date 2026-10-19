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
    initialNativeCells,
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
import Control.Concurrent qualified as Conc
import Control.Concurrent.STM as STM
import Control.Exception
import Control.Lens
import Control.Monad.State.Strict
import Data.Atomics qualified as Atomic
import Data.HashMap.Lazy qualified as HM
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as M
import Data.Map.Strict.Internal qualified as M
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Set qualified as Set
import Data.Text qualified as DTx
import Data.Text.IO qualified as Tx
import Data.Traversable
import Foreign.Concurrent qualified as CFPtr
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
import Unison.Runtime.JIT (initialNativeCount, jitCompileGroup, jitConfig, jitExits, jitFrames, jitPool, jitRequestCell, jitRequestGroup, jitStats, jitTracing, noteOutput, printJITStats, registerTypeNumbers, stackSlots)
import Unison.Runtime.JIT.Config qualified as JIT
import Unison.Runtime.JIT.Exits
import Unison.Runtime.JIT.Frames
import Unison.Runtime.JIT.Native
import Unison.Runtime.JIT.Pool (currentPool)
import Unison.Util.Deque qualified as Sq
import Unsafe.Coerce (unsafeCoerce)
#ifdef CODE_SERIAL_CHECK
import Unison.Runtime.ANF.Serialize (serializeCode, deserializeCode)
#endif
import Data.Text qualified as Text
import Data.Vector qualified as V
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
import Unison.Util.BiMap qualified as BM
import Unison.Util.Bytes qualified as Bytes
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
  stk <- alloc (stackSlots (jit env))
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
topAEnv _ _ _ = pure (emptyAEnv, id)

topHEnv ::
  EnumMap Word64 MCombs ->
  M.Map Reference Word64 ->
  M.Map Reference Word64 ->
  IO (HEnv, K -> K)
topHEnv combs rfTy rfTm =
  first (flip HEnv emptyDEnv) <$> topAEnv combs rfTy rfTm

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
  stk <- alloc (stackSlots (jit env))
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
  stk <- alloc (stackSlots (jit env))
  (tick, cancelTicks) <- startTicker $ profiler env
  apply tick env (HEnv emptyAEnv emptyDEnv) threadTracker stk k0 True ZArgs clo
    `finally` cancelTicks
  where
    k0 = CB $ Hook (\stk -> callback $ packXStack stk)
{-# INLINE apply1 #-}

unitValue :: Val
unitValue = BoxedVal $ unitClosure
{-# NOINLINE unitValue #-}

litToVal :: MLit -> Val
litToVal = \case
  MT t -> BoxedVal $ TextC t
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
      henv <- evaluate $ HEnv aenv emptyDEnv
      pure (False, henv, stk, k)
    _ -> die [] "Discard called with bad handler reference"
exec _env henv0 !_activeThreads !stk !k _ (InLocal i) = do
  bpeekOff stk i >>= \case
    Affine _ aenv _ -> do
      (stk, a) <- saveArgs stk
      henv <- evaluate $ HEnv aenv emptyDEnv
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
exec _ henv !_activeThreads !stk !k _ (RecPack shape args) = do
  clo <- buildRec stk shape args
  stk <- bump stk
  bpoke stk clo
  pure (False, henv, stk, k)
exec _ henv !_activeThreads !stk !k _ (RecUnpack desiredFields recIndex) = do
  bpeekOff stk recIndex >>= \case
    RecordG shape vals -> do
      -- The slot a field sits in depends on the *value's* shape, not the
      -- pattern's: one pattern matches records of several shapes, in which a
      -- given field may sit at a different slot.
      let seg =
            V.toList desiredFields
              <&> (\f -> vals V.! shapeSlot shape f)
              & segFromList
      stk' <- dumpSeg stk seg S
      pure (False, henv, stk', k)
    _ -> die [] "RecUnpack called on non-record value"
exec _ henv !_activeThreads !stk !k _ (RecSet field args) = do
  case args of
    VArg2 recI valI -> do
      rv <- bpeekOff stk recI
      nv <- peekOff stk valI
      case rv of
        RecordG shape vals -> do
          -- The value's own shape says which slot the field sits in.
          let pos = shapeSlot shape field
          stk <- bump stk
          bpoke stk . RecordG shape $ vals V.// [(pos, nv)]
          pure (False, henv, stk, k)
        _ -> die [] "RecSet called on non-record value"
    _ -> die [] "RecSet called with unexpected arguments"
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
          -- evaluated: the JIT's handler code reads it from the stack
          !aff = Affine ps aenv0 ar
      ahv <- extendPAp ahv0 (BoxedVal aff)
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
exec env henv !_activeThreads !stk !k _ (ForeignCall _ func args) = do
  (b, stk) <- exStackIOToIO $ foreignCall func args (unpackXStack stk)
  -- for the JIT's per-output statistics; nothing unless they are on
  noteOutput (jit env) func
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
exec env henv !activeThreads !stk !k _ (NewForeignPtr i j)
  | sandboxed env =
      die [] "attempted to use sandboxed operation: ForeignPtr.new"
  | otherwise = do
      fin <- peekOff stk i
      p <- peekOffBi stk j
      let thunk = nestEval env activeThreads (const $ pure ()) fin
      fp <- CFPtr.newForeignPtr p thunk
      stk <- bump stk
      pokeBi stk fp
      pure (False, henv, stk, k)
exec env henv !activeThreads !stk !k _ (AddFinalizer i j)
  | sandboxed env =
      die [] "attempted to use sandboxed operation: ForeignPtr.addFinalizer"
  | otherwise = do
      fp <- peekOffBi stk i
      fin <- peekOff stk j
      CFPtr.addForeignPtrFinalizer fp $
        nestEval env activeThreads (const $ pure ()) fin
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
eval' !yld env henv !activeThreads !stk !k r (Match i (TestY df cs)) = do
  b <- peekOffBi stk i
  eval yld env henv activeThreads stk k r $ selectBytesBranch b df cs
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
eval' !yld env henv !activeThreads !stk !k r (Let nw cix f sect cell) = do
  (stk, fsz, asz) <- saveFrame stk
  eval
    yld
    env
    henv
    activeThreads
    stk
    (Push fsz asz cix f sect cell k)
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
          let kk = Push fsz asz fakeCix 10 nx noNativeCell k
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
  (RComb (Comb (LamI a f entry cell))) -> do
    native <- readNativeEntry cell
    if native == nullPtr
      then do
        countInterpreted env cref cell
        -- check for stack check _skip_
        stk <- if sck then pure stk else ensure stk f
        stk <- moveArgs stk args
        stk <- acceptArgs stk a
        eval yld env henv activeThreads stk k cref entry
      else do
        when (jitTracing (jit env)) $
          JIT.jitDump ("enter native " ++ show cref ++ " arity " ++ show a ++ " args " ++ show args ++ " cell " ++ show cell ++ " code " ++ show native ++ " stack before " ++ show stk)
        stk <- if sck then pure stk else ensure stk f
        stk <- moveArgs stk args
        stk <- acceptArgs stk a
        when (jitTracing (jit env)) $ JIT.jitDump ("  stack after " ++ show stk)
        runNative yld env henv activeThreads stk k (castPtrToFunPtr native)
  (RComb (CachedVal _ val)) -> do
    stk <- discardFrame stk
    stk <- bump stk
    poke stk val
    yield yld env henv activeThreads stk k
{-# INLINE enter #-}

-- Counts a call the interpreter is about to run itself because the
-- combinator has no native code, and asks for the combinator's definition
-- to be compiled when the count says it is hot (in the JIT's `on` mode).
countInterpreted :: CCache prof -> CombIx -> Ptr NativeCell -> IO ()
countInterpreted env cix cell = do
  hot <- bumpNativeCount cell
  when hot (jitRequestGroup (jit env) (sandboxed env) (combs env) (combRefs env) cix cell)
{-# INLINE countInterpreted #-}

-- Likewise for a re-entry point: counts a time the interpreter continued
-- at a place where native code would have, had it been generated, and asks
-- for it to be once that has happened often enough.
countReentry :: CCache prof -> Ptr NativeCell -> IO ()
countReentry env cell = do
  hot <- bumpNativeCount cell
  when hot (jitRequestCell (jit env) cell)
{-# INLINE countReentry #-}

-- The trampoline: runs compiled code for a combinator whose frame has
-- been set up as `enter` sets it up, then acts on the status it returns.
-- See unison-runtime/src/Unison/Runtime/JIT/design.md, "The trampoline".
runNative ::
  (RuntimeProfiler prof) =>
  Ticker prof ->
  CCache prof ->
  HEnv ->
  ActiveThreads ->
  Stack ->
  K ->
  FunPtr NativeFn ->
  IO ()
runNative !yld env henv0 !activeThreads !stk0 !k0 fn0 = go fn0 henv0 stk0 k0
  where
    -- read once: these are consulted several times per round trip, and
    -- the tables on every one
    !tracing = jitTracing (jit env)
    !counting = jitStats (jit env)
    !exits = jitExits (jit env)
    !frames = jitFrames (jit env)
    !pool = jitPool (jit env)
    go fn henv (Stack ap fp sp ustk bstk) k = do
      poolArr <- currentPool pool
      let !size = bstkSlots bstk
      when counting $ countEntry exits (castFunPtrToPtr fn)
      -- the handler environment goes in the boxed stack's out-area, where
      -- the collector sees it; forced here, since native code can read a
      -- constructor but not a thunk
      let !henvC = unsafeCoerce henv :: Closure
      writeArray bstk size henvC
      status <- enterNative fn ustk bstk poolArr size ap fp sp
      ap' <- outAp ustk size
      fp' <- outFp ustk size
      sp' <- outSp ustk size
      let !stk = Stack ap' fp' sp' ustk bstk
      if status == statusOK
        then do
          when tracing $ JIT.jitDump ("native returned OK with ap/fp/sp " ++ show (ap', fp', sp') ++ " (entered with " ++ show (ap, fp, sp) ++ ")")
          yield yld env henv activeThreads stk k
        else
          if status == statusError
            then die [] "native code reported an error"
            else do
              -- the frame records of the native callers above the exit;
              -- they can be read once
              n <- outFrameCount ustk size
              records <- if n == 0 then pure [] else outFrames ustk size
              when tracing $
                JIT.jitDump ("native returned " ++ show status ++ " with ap/fp/sp " ++ show (ap', fp', sp') ++ " (entered with " ++ show (ap, fp, sp) ++ ")" ++ concatMap (("\n  frame record " ++) . show) records)
              when counting $ do
                total <- countExit exits status
                let every = JIT.statsEvery (jitConfig (jit env))
                when (every > 0 && total `mod` every == 0) (printJITStats (jit env))
              -- Records are written innermost caller first; K has the
              -- innermost frame on top, so build from the last record.
              k <- if n == 0 then pure k else foldM (pushRecord bstk size) k (reverse records)
              -- the environment native code was running with: switched,
              -- when it exited from inside a handler case
              henv' <- unsafeCoerce <$> readArray bstk size
              act henv' stk k =<< lookupExit exits status
    pushRecord bstk size k (FrameRecord i fsz asz)
      | i < 0 = do
          -- an affine handler case's environment switch, in force at the
          -- exit: the frame that switches back (see exec's InLocal)
          henv0 <- unsafeCoerce <$> readArray bstk (size + 1 + (-1 - i))
          pure (Local henv0 asz k)
      | otherwise = do
          Frame cix f sect cell <- lookupFrame frames i
          pure (Push fsz asz cix f sect cell k)
    act henv stk k = \case
      Named _ e -> act henv stk k e
      Resume cix sect -> do
        when tracing $ JIT.jitDump ("resume " ++ show cix ++ " at\n" ++ prettySection 4 sect "")
        eval yld env henv activeThreads stk k cix sect
      -- Grow by at least the stack's current size, ensuring push is amortized O(1)
      GrowStack n cell -> do
        when tracing $ JIT.jitDump ("grow stack by " ++ show n)
        stk <- ensureGenerously stk n
        again cell henv stk k
      Reenter cell -> do
        when tracing $ JIT.jitDump "reenter"
        Conc.yield
        again cell henv stk k
      -- The interpreter runs one instruction, then native code continues
      -- after it, with whatever the instruction did to the handler
      -- environment and K. The exception case is eval's, except that the
      -- handler's continuation can be native too.
      CallOut cix instr rest n cell -> do
        when tracing $ JIT.jitDump ("call out " ++ show cix ++ " for " ++ show instr)
        code <- readNativeEntry cell
        exec env henv activeThreads stk k cix instr >>= \case
          (exception, henv, !stk', !k')
            | exception -> do
                eh <- resolveExceptionHandler henv
                fv <- peek stk'
                bpoke stk' $ Data1 exceptionRef TT.exceptionRaiseTag fv
                (stk', fsz, asz) <- saveFrame stk'
                let cell' = if n == 1 then cell else noNativeCell
                    kk = Push fsz asz fakeCix 10 rest cell' k'
                apply yld env henv activeThreads stk' kk False (VArg1 0) eh
            | code /= nullPtr,
              sp stk' == sp stk + n,
              fp stk' == fp stk ->
                go (castPtrToFunPtr code) henv stk' k'
            | otherwise -> do
                when (code == nullPtr) (countReentry env cell)
                when tracing $ JIT.jitDump ("  no re-entry: pushed " ++ show (sp stk' - sp stk) ++ ", expected " ++ show n ++ "; fp " ++ show (fp stk, fp stk') ++ ", code " ++ show code)
                eval yld env henv activeThreads stk' k' cix rest
    -- The exit says which function to call again. It is the one that took
    -- the exit, which after tail calls need not be the one entered here.
    --
    -- Its cell can still be empty: the functions of a module call each
    -- other directly, and one can be running before the compile thread
    -- has written the cell of another that it called. The cell is about
    -- to be filled, so wait for it.
    again cell henv stk k = wait (10000 :: Int)
      where
        wait n = do
          code <- readNativeCode cell
          if code /= nullPtr
            then go (castPtrToFunPtr code) henv stk k
            else
              if n == 0
                then die [] "native code disappeared from its cell"
                else Conc.threadDelay 1000 >> wait (n - 1)

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
  !useg <- unsafeFreezeByteArray ucop

  bcop <- newArray (bssz + 1) BlackHole
  copyArray bcop 1 bseg0 0 bssz
  writeArray bcop 0 $! getBoxedVal new
  !bseg <- unsafeFreezeArray bcop

  -- evaluated: the JIT's request code reads the ref this goes into
  pure $! PAp cix comb (useg, bseg)
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
        LamI a f entry cell
          | ck || a <= ac -> do
              stk <- ensure stk f
              stk <- moveArgs stk args
              stk <- dumpSeg stk seg A
              stk <- acceptArgs stk a
              native <- readNativeEntry cell
              if native == nullPtr || "apply" `elem` JIT.disabled (jitConfig (jit env))
                then do
                  countInterpreted env cix cell
                  eval yld env henv activeThreads stk k cix entry
                else do
                  when (jitTracing (jit env)) $
                    JIT.jitDump ("apply native " ++ show cix ++ " arity " ++ show a ++ " stack " ++ show stk)
                  runNative yld env henv activeThreads stk k (castPtrToFunPtr native)
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
    adjust (Push n a cix f rsect c k) =
      (0, Push n (a + asize stk) cix f rsect c k)
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
    go !denv (Push n a cix f rsect c sk) !k =
      go denv sk $ Push n a cix f rsect c k
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
buildData !stk !r !t (VArgR i l)
  | l <= 6 = buildDataAt stk r t l (i +)
  | otherwise = do
      seg <- augSeg I stk nullSeg (Just $ ArgR i l)
      pure $ DataG r t seg
buildData !stk !r !t (VArgN as)
  | n <= 6 = buildDataAt stk r t n (PA.indexPrimArray as)
  | otherwise = do
      seg <- augSeg I stk nullSeg (Just $ ArgN as)
      pure $ DataG r t seg
  where
    n = PA.sizeofPrimArray as
buildData !stk !r !t (VArgV i)
  | l <= 6 = buildDataAt stk r t l id
  | otherwise = do
      seg <- augSeg I stk nullSeg (Just $ ArgR 0 l)
      pure $ DataG r t seg
  where
    l = fsize stk - i
{-# INLINE buildData #-}

-- | A constructor of @n@ (up to six) fields, field @k@ read from slot
-- @slot k@.
buildDataAt :: Stack -> Reference -> PackedTag -> Int -> (Int -> Int) -> IO Closure
buildDataAt !stk !r !t !n slot = case n of
  0 -> pure $ Enum r t
  1 -> Data1 r t <$> at 0
  2 -> Data2 r t <$> at 0 <*> at 1
  3 -> Data3 r t <$> at 0 <*> at 1 <*> at 2
  4 -> Data4 r t <$> at 0 <*> at 1 <*> at 2 <*> at 3
  5 -> Data5 r t <$> at 0 <*> at 1 <*> at 2 <*> at 3 <*> at 4
  6 -> Data6 r t <$> at 0 <*> at 1 <*> at 2 <*> at 3 <*> at 4 <*> at 5
  _ -> die [] "buildDataAt: more than six fields"
  where
    at k = peekOff stk (slot k)
{-# INLINE buildDataAt #-}

-- | Pack some number of args into a record value of the given shape. The args
-- are emitted in ascending field-name order, which is the shape's slot order,
-- so this is a straight copy.
buildRec :: Stack -> RecordShape -> Args -> IO Closure
buildRec !stk shape args = do
  seg <- augSeg I stk nullSeg (Just $ argsToArgs' args)
  pure . RecordG shape $ recordVals seg
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
  Data1 _ _ a -> do
    stk <- bump stk
    stk <$ poke stk a
  Data2 _ _ a b -> do
    stk <- bumpn stk 2
    pokeOff stk 1 b
    stk <$ poke stk a
  clo@(Data3 {}) -> dumpDataWide stk clo
  clo@(Data4 {}) -> dumpDataWide stk clo
  clo@(Data5 {}) -> dumpDataWide stk clo
  clo@(Data6 {}) -> dumpDataWide stk clo
  DataG _ _ seg -> dumpSeg stk seg S
  clo ->
    die [3320] $
      "dumpDataNoTag: bad closure: "
        ++ show clo
        ++ maybe "" (\r -> "\nexpected type: " ++ show r) mr
{-# INLINE dumpDataNoTag #-}

-- | The fields of a constructor with three to six fields, out of line so
-- that the interpreter's loop only inlines the common shapes.
dumpDataWide :: Stack -> Closure -> IO Stack
dumpDataWide !stk = \case
  Data3 _ _ a b c -> do
    stk <- bumpn stk 3
    pokeOff stk 2 c
    pokeOff stk 1 b
    stk <$ poke stk a
  Data4 _ _ a b c d -> do
    stk <- bumpn stk 4
    pokeOff stk 3 d
    pokeOff stk 2 c
    pokeOff stk 1 b
    stk <$ poke stk a
  Data5 _ _ a b c d e -> do
    stk <- bumpn stk 5
    pokeOff stk 4 e
    pokeOff stk 3 d
    pokeOff stk 2 c
    pokeOff stk 1 b
    stk <$ poke stk a
  Data6 _ _ a b c d e f -> do
    stk <- bumpn stk 6
    pokeOff stk 5 f
    pokeOff stk 4 e
    pokeOff stk 3 d
    pokeOff stk 2 c
    pokeOff stk 1 b
    stk <$ poke stk a
  clo -> die [3321] $ "dumpDataWide: bad closure: " ++ show clo
{-# NOINLINE dumpDataWide #-}

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
      henv <- evaluate $ HEnv aenv emptyDEnv
      apply yld env henv activeThreads stk k False (VArg1 0) h
    leap (Push fsz asz cix f nx cell k) = do
      stk <- restoreFrame stk fsz asz
      stk <- ensure stk f
      -- The Let body may have native code (the re-entry point).
      native <- readNativeEntry cell
      if native == nullPtr
        then do
          countReentry env cell
          eval yld env henv0 activeThreads stk k cix nx
        else runNative yld env henv0 activeThreads stk k (castPtrToFunPtr native)
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

selectBytesBranch ::
  Bytes.Bytes -> MSection -> M.Map Bytes.Bytes MSection -> MSection
selectBytesBranch b df cs = M.findWithDefault df b cs
{-# INLINE selectBytesBranch #-}

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
selectBranch _ (TestY {}) = error "impossible"
{-# INLINE selectBranch #-}

-- Combined branch selection and field dumping function for data types.
-- Fields should only be dumped on _matches_, not default cases, because
-- default cases potentially cover many constructors which could result
-- in a variable number of values being put on the stack. Default cases
-- uniformly expect _no_ values to be added to the stack.
dataBranch ::
  Maybe Reference -> Stack -> MBranch -> Closure -> IO (MSection, Stack)
dataBranch mrf stk br = \case
  -- one case on the closure: each shape looks its arm up and pushes its
  -- fields in the same alternative (a second dispatch to push them cost a
  -- few nanoseconds per match, 2026-10-15)
  Enum _ t -> pick t (pure stk)
  Data1 _ t a -> pick t do
    stk <- bump stk
    stk <$ poke stk a
  Data2 _ t a b -> pick t do
    stk <- bumpn stk 2
    pokeOff stk 1 b
    stk <$ poke stk a
  clo@(Data3 _ t _ _ _) -> pick t (dumpDataWide stk clo)
  clo@(Data4 _ t _ _ _ _) -> pick t (dumpDataWide stk clo)
  clo@(Data5 _ t _ _ _ _ _) -> pick t (dumpDataWide stk clo)
  clo@(Data6 _ t _ _ _ _ _ _) -> pick t (dumpDataWide stk clo)
  DataG _ t seg -> pick t (dumpSeg stk seg S)
  Foreign (WrapMap m) -> case m of
    M.Bin sz k e l r
      | Just ca <- branchArm br Rf.mapBin -> (ca,) <$> dumpBin sz k e l r stk
    M.Tip
      | Just ca <- branchArm br Rf.mapTip -> pure (ca, stk)
    _ -> pure (branchDefault br, stk)
  clo -> (branchDefault br, stk) <$ dataBranchClosureError mrf clo
  where
    pick :: PackedTag -> IO Stack -> IO (MSection, Stack)
    pick t push = case branchArm br (maskTags t) of
      Just ca -> (ca,) <$> push
      Nothing -> pure (branchDefault br, stk)
    {-# INLINE pick #-}
{-# INLINE dataBranch #-}

-- | The section of a data branch for a constructor number, if it has one.
-- Inlined into a case, so the Maybe costs nothing.
branchArm :: MBranch -> Word64 -> Maybe MSection
branchArm br ct = case br of
  Test1 u cu _ -> if ct == u then Just cu else Nothing
  Test2 u cu v cv _ -> if ct == u then Just cu else if ct == v then Just cv else Nothing
  TestW _ bs -> EC.lookup ct bs
  _ -> Nothing
{-# INLINE branchArm #-}

-- | The default section of a data branch.
branchDefault :: MBranch -> MSection
branchDefault = \case
  Test1 _ _ d -> d
  Test2 _ _ _ _ d -> d
  TestW d _ -> d
  br -> throw (Panic ("dataBranch: unexpected branch: " ++ show br) Nothing)
{-# INLINE branchDefault #-}

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
      TextC {} -> "a builtin value of type `" <> prettyRef Rf.textRef <> "`"
      ListC {} -> "a builtin value of type `" <> prettyRef Rf.listRef <> "`"
      BytesC {} -> "a builtin value of type `" <> prettyRef Rf.bytesRef <> "`"
      Foreign (foreignRef -> rf) ->
        "a builtin value of type `" <> prettyRef rf <> "`"
      RecordC {} -> "a record"

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
    walk !denv !sz !ck (Push n a br p brSect c k) =
      walk
        denv
        (sz + n + a)
        (Push n a br p brSect c ck)
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
      (Push n a _ _ _ _ k) -> walk (sz + n + a) k
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
cacheAdd0 ntys0 (normalizeCodes -> termSuperGroups) sands cc = go
  where
    toAdd = M.fromList (termSuperGroups <&> second codeGroup)
    -- Which record schemas this code needs is a property of the code, so read it
    -- off the groups rather than making every caller work it out.
    recSchemas = foldMap (ANF.groupRecordSchemas . codeGroup . snd) termSuperGroups

    -- Every new combinator gets a native code cell (see unison-runtime/src/Unison/Runtime/JIT/design.md).
    -- Cells come from the cache's pool, so the transaction never allocates
    -- memory, which a retried transaction would leak. If the pool is too
    -- small, the transaction makes no changes to the code and reports how
    -- many cells it needs; the pool is grown outside the transaction and
    -- the whole thing is tried again.
    go = do
      result <- atomically transaction
      case result of
        Left needed -> do
          pool <- readTVarIO (nativeCells cc)
          pool' <- growNativeCellPool (initialNativeCount (jit cc)) needed pool
          atomically $ writeTVar (nativeCells cc) pool'
          go
        Right (unresolvedCacheableCombs, unresolvedNonCacheableCombs, newCombs, newCombRefs) -> do
          readTVarIO (tagRefs cc) >>= registerTypeNumbers (jit cc)
          when (JIT.dumpMCode (jitConfig (jit cc))) $
            for_ (EC.mapToList (unresolvedCacheableCombs <> unresolvedNonCacheableCombs)) \(w, cmbs) ->
              for_ (EC.mapToList cmbs) \(i, c) -> JIT.jitDump (prettyComb w i c "")
          for_ (EC.mapToList newCombs) \(w, cmbs) ->
            for_ (EC.lookup w newCombRefs) \r -> jitCompileGroup (jit cc) (sandboxed cc) r w cmbs
          preEvalTopLevelConstants unresolvedCacheableCombs unresolvedNonCacheableCombs cc

    transaction = do
      have <- readTVar (intermed cc)
      haveRecSchemas <- readTVar (recordRefs cc)
      let new = M.difference toAdd have
      let sz = fromIntegral $ M.size new
      let rs = M.keys new
      let int = new <> have
      let replace =
            ANF.replaceConstructors pseudoConstructors
              . ANF.replaceFunctions functionReplacements
          haff (cmbs, opts) =
            (M.mapWithKey (ANF.optimizeHandler Builtin opts) cmbs, opts)
      (opt, optInfos') <- haff . ANF.optimize (fmap replace new) <$> readTVar (optInfos cc)
      -- These three number things. Numbering is harmless to commit even if the
      -- pool turns out to be too small: the numbers are simply used or skipped.
      rty <- addRefs (freshTy cc) (refTy cc) (tagRefs cc) ntys0
      ntm <- stateTVar (freshTm cc) $ \i -> (i, i + sz)
      let newRefTm = M.fromList $ zip rs [ntm ..]
      rtm <- (newRefTm <>) <$> readTVar (refTm cc)
      let newRecSchemas = recSchemas `Set.difference` (BM.keysSetL haveRecSchemas)
      let numNewRecSchemas = fromIntegral $ Set.size newRecSchemas
      nrs <- stateTVar (freshRecSchema cc) $ \i -> (i, i + numNewRecSchemas)
      let newRecSchemaMap = BM.fromList $ zip (Set.toList newRecSchemas) (ANF.RecordRef <$> [nrs ..])
          rrLookup = newRecSchemaMap <> haveRecSchemas
      -- Field names no longer need pre-registering here: building a record's
      -- shape during emit interns any name it hasn't seen.
      oldRfms <- readTVar (recordFieldMappings cc)
      -- check for missing references
      let arities = fmap (head . ANF.arities) int <> builtinArities
          rns = RN (refLookup "ty" rty) (refLookup "tm" rtm) (flip M.lookup arities) (recordRefLookup rrLookup)
          combinate :: Word64 -> (Reference, SuperGroup Reference Symbol) -> State RecordFieldMappings (Word64, EnumMap Word64 Comb)
          combinate n (r, g) = (n,) <$> emitCombs rns r n g
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
      let (emittedCombs, newRFMs) =
            zipWith combinate [ntm ..] (M.toList opt)
              & sequenceA
              & flip runState oldRfms
          uncelled =
            sanitizeCombsOfForeignFuncs (sandboxed cc) sandboxedForeignFuncs $
              mapFromList emittedCombs
          (cellCount, _) = attachNativeCells nullPtr uncelled
      pool <- readTVar (nativeCells cc)
      case takeNativeCells cellCount pool of
        Nothing -> pure (Left cellCount)
        Just (cells, pool') -> do
          -- Everything is in hand. From here on the transaction only writes.
          writeTVar (nativeCells cc) pool'
          int' <- updateMap new (intermed cc)
          writeTVar (optInfos cc) optInfos'
          rtm' <- updateMap newRefTm (refTm cc)
          rrs <- updateMap newRecSchemaMap (recordRefs cc)
          newCombRefs <- updateMap combRefUpdates (combRefs cc)
          (unresolvedNewCombs, unresolvedCacheableCombs, unresolvedNonCacheableCombs, newCombs, updatedCombs) <- stateTVar (combs cc) \oldCombs ->
            let unresolvedNewCombs :: EnumMap Word64 (GCombs any CombIx)
                unresolvedNewCombs = absurdCombs (snd (attachNativeCells cells uncelled))
                (unresolvedCacheableCombs, unresolvedNonCacheableCombs) =
                  EC.mapToList unresolvedNewCombs & foldMap \(w, gcombs) ->
                    if EC.member w newCacheableCombs
                      then (EC.mapSingleton w gcombs, mempty)
                      else (mempty, EC.mapSingleton w gcombs)
                newCombs :: EnumMap Word64 MCombs
                newCombs = resolveCombs (Just oldCombs) $ unresolvedNewCombs
                updatedCombs = newCombs <> oldCombs
             in ((unresolvedNewCombs, unresolvedCacheableCombs, unresolvedNonCacheableCombs, newCombs, updatedCombs), updatedCombs)
          nsc <- updateMap unresolvedNewCombs (srcCombs cc)
          nsn <- updateMap (M.fromList sands) (sandbox cc)
          ncc <- updateMap newCacheableCombs (cacheableCombs cc)
          writeTVar (recordFieldMappings cc) newRFMs
          -- Now that the code cache is primed with everything we need,
          -- we can pre-evaluate the top-level constants.
          pure $ int' `seq` rtm' `seq` rrs `seq` newCombRefs `seq` updatedCombs `seq` nsn `seq` ncc `seq` nsc `seq` Right (unresolvedCacheableCombs, unresolvedNonCacheableCombs, newCombs, newCombRefs)

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

-- | A map deserialized from the interchange form, in the form the
-- program's own maps have: the Haskell 'WrapMap' when base's @Map@ type is
-- replaced, the Unison tree otherwise (see `mapReplaced`).
mapVal :: M.Map Val Val -> Val
mapVal m
  | mapReplaced = encodeVal m
  | otherwise = boxedVal (inflateMap m)

-- | The key/value pairs of base's @Map@ tree in key order, when the type
-- isn't replaced and the tree has the shape `inflateMap` builds; the tree
-- is serialized as the interchange `Map` node, as the Haskell map is, so
-- that the bytes (and the typed hash) don't depend on the setting or on
-- the tree's shape.
nativeMapPairs :: Closure -> Maybe [(Val, Val)]
nativeMapPairs = go []
  where
    go acc (Enum _ t) | t == TT.mapTipTag = Just acc
    go acc (Data5 _ t _sz k v (BoxedVal l) (BoxedVal r))
      | t == TT.mapBinTag = go acc r >>= \acc' -> go ((k, v) : acc') l
    go _ _ = Nothing

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
          Enum _ t
            | not mapReplaced, t == TT.mapTipTag -> pure $ ANF.BLit (ANF.Map [])
          DataC _ t vs
            | not mapReplaced,
              t == TT.mapBinTag,
              Just kvs <- nativeMapPairs clos ->
                ANF.BLit . ANF.Map <$> traverse (\(k, v) -> (,) <$> goV k <*> goV v) kvs
            | otherwise -> do
                r <- resolveTy rty $ TT.typeTag t
                ANF.Data r (maskTags t) <$> traverse goV vs
          RecordC shape vals ->
            -- The shape carries the field names, in the same ascending order
            -- as the slots, which is also the order `reifyValue` expects.
            ANF.Record (ANF.RecordSchema (S.fromList (V.toList (shapeFields shape))))
              <$> traverse goV (V.toList vals)
          Captured k _ segs ->
            ANF.Cont <$> goVs segs <*> goK k
          TextC t -> pure (ANF.BLit (ANF.Text t))
          ListC s -> ANF.BLit . ANF.List . Seq.fromList <$> traverse goV (toList s)
          BytesC b -> pure (ANF.BLit (ANF.Bytes b))
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
    goK (Push f a cix _ _rsect _ k) =
      ANF.Push
        (fromIntegral f)
        (fromIntegral a)
        <$> goIx cix
        <*> goK k

    goF = \case
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
      recRefLookup <- readTVar (recordRefs cc)
      rfms <- readTVar (recordFieldMappings cc)
      case S.toList $ S.filter (`M.notMember` rtm) tmLinks of
        [] -> do
          newTy <- addRefs (freshTy cc) (refTy cc) (tagRefs cc) tyLinks
          pure . Right $ (combs, newTy, rtm, recRefLookup, rfms)
        l -> pure (Left l)
  traverse (\rfs -> reifyValue1 rfs val) erc

reifyValue1 ::
  (EnumMap Word64 MCombs, M.Map Reference Word64, M.Map Reference Word64, BM.BiMap ANF.RecordSchema ANF.RecordRef, RecordFieldMappings) ->
  Referenced ANF.Value ->
  IO Val
reifyValue1 tup (Plain v) = reifyValue0 tup v
reifyValue1 (combs, rty0, rtm0, rrLookup, rfms) (WithRefs tys tms v) = do
  let rty = HM.fromList . mapMaybe procTypeRefs $ zip [0 ..] tys
      rtm = HM.fromList . mapMaybe procTermRefs $ zip [0 ..] tms
  reifyValue0Canon combs tys tms rty rtm rrLookup rfms v
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
  BM.BiMap ANF.RecordSchema ANF.RecordRef ->
  RecordFieldMappings ->
  ANF.Value RefNum ->
  IO Val
reifyValue0Canon combs tys tms rty rtm rrLookup rfms = goV
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

    goVSeq :: Seq (ANF.Value RefNum) -> IO USeq
    goVSeq vs = Sq.fromList <$> traverse goV (toList vs)

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
    goV (ANF.Record rs vals) = do
      rref <- case BM.lookupL rs rrLookup of
        Just r -> pure r
        Nothing -> die [] . err $ "unknown record schema reference: " ++ show rs
      shape <- case recordShapeFrom rfms rref rs of
        Just shape -> pure shape
        Nothing -> die [] . err $ "record schema with un-interned fields: " ++ show rs
      -- `putValue` writes the values in ascending field-name order, which is
      -- the shape's slot order, so no reordering is needed.
      vals' <- goVs vals
      pure . boxedVal . RecordG shape $ recordVals vals'
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
        (cix, RComb (Comb (LamI _ fr sect cell))) ->
          Push
            (fromIntegral f)
            (fromIntegral a)
            cix
            fr
            sect
            cell
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
    goL (ANF.Map l) = mapVal . M.fromList <$> traverse goP l
      where
        goP (x, y) = (,) <$> goV x <*> goV y
    goL (ANF.BigInt i) = pure $ encodeVal i
    goL (ANF.BigNat n) = pure $ encodeVal n

reifyValue0 ::
  (EnumMap Word64 MCombs, M.Map Reference Word64, M.Map Reference Word64, BM.BiMap ANF.RecordSchema ANF.RecordRef, RecordFieldMappings) ->
  ANF.Value Reference ->
  IO Val
reifyValue0 (combs, rty, rtm, rrLookup, rfms) = goV
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
    goV (ANF.Record rs vals) = do
      rref <- case BM.lookupL rs rrLookup of
        Just r -> pure r
        Nothing -> die [] . err $ "unknown record schema reference: " ++ show rs
      shape <- case recordShapeFrom rfms rref rs of
        Just shape -> pure shape
        Nothing -> die [] . err $ "record schema with un-interned fields: " ++ show rs
      -- `putValue` writes the values in ascending field-name order, which is
      -- the shape's slot order, so no reordering is needed.
      vals' <- goVs vals
      pure . boxedVal . RecordG shape $ recordVals vals'
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
        (cix, RComb (Comb (LamI _ fr sect cell))) ->
          Push
            (fromIntegral f)
            (fromIntegral a)
            cix
            fr
            sect
            cell
            <$> goK k
        (CIx r _ _, _) ->
          die [] . err $
            "tried to reify a continuation with a cached value resumption"
              ++ show r

    goL :: ANF.BLit Reference -> IO Val
    goL (ANF.Text t) = pure $ encodeVal t
    goL (ANF.List l) = boxedVal . ListC . Sq.fromList <$> traverse goV (toList l)
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
    goL (ANF.Map l) = mapVal . M.fromList <$> traverse goP l
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
