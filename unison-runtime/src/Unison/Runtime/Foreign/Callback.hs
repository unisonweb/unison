{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Turning a Unison function into a C-callable function pointer, so that C
-- code can call back into Unison. This is the mirror image of the dynamic
-- call in "Unison.Runtime.Machine": there, Unison stack values are marshalled
-- into a libffi argument buffer and a C function is called; here a C caller
-- fills an argument buffer, a libffi /closure/ invokes a single generic
-- Haskell entry point, we decode the arguments directly onto a Unison stack,
-- run the Unison function, and marshal its result (left on the stack) back
-- into the C return slot.
--
-- The Haskell @libffi@ package does not expose the closure API, so the three
-- @ffi_closure_*@ entry points are imported directly (they are linked via
-- that package already), as is done for @ffi_prep_cif_var@ in
-- "Unison.Runtime.Foreign.Dynamic".
--
-- Running the Unison function is supplied by the caller (a @Stack -> (Stack ->
-- IO ()) -> IO ()@ action, in practice a partially-applied evaluator entry) so
-- that this module does not depend on the evaluator. Arguments are written
-- straight to the stack rather than through an intermediate @[Val]@, so the
-- marshalling allocates nothing beyond the stack itself.
module Unison.Runtime.Foreign.Callback
  ( RunCallback,
    mkCallback,
    freeCallback,
    CallbackException (..),
  )
where

import Control.Exception (Exception, finally, throwIO)
import Control.Monad (unless, when)
import Data.IORef
import Data.Int
import Data.Word
import Foreign.C.Types (CSize (..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.LibFFI.Internal (CIF, CValue, C_ffi_status, ffi_ok)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable (peek, peekElemOff, poke)
import GHC.Float (double2Float, float2Double)
import System.IO.Unsafe (unsafePerformIO)
import Unison.Runtime.Foreign.Dynamic
import Unison.Runtime.Stack
  ( Stack,
    alloc,
    bump,
    peekOffBi,
    peekOffD,
    peekOffI,
    peekOffN,
    pokeBi,
    pokeD,
    pokeI,
    pokeN,
  )

-- | Runs the Unison function from a callback: the given stack already holds the
-- decoded C arguments (pushed in parameter order); apply the function to them
-- and hand the result stack to the continuation (the result on top). Supplied
-- by the evaluator so this module stays off it. Expected to be synchronous.
type RunCallback = Stack -> (Stack -> IO ()) -> IO ()

-- | What a prepared closure needs at call time: the signature (for
-- marshalling), the action that runs the Unison function, and a reusable stack
-- guarded by an in-use flag (see 'callbackEntry').
data CallbackCtx = CallbackCtx !FFSpec !RunCallback !(IORef Bool) !Stack

data CallbackException
  = -- | A callback argument/result type that is not supported.
    UnsupportedCallbackType FFType
  | -- | @ffi_prep_closure_loc@ failed.
    BadClosureInit
  deriving (Eq, Show)

instance Exception CallbackException

-- The entry function has a fixed C signature regardless of the callback's
-- actual type, so it can be wrapped once and shared by every closure.
type CallbackEntry =
  Ptr CIF -> Ptr CValue -> Ptr (Ptr CValue) -> Ptr () -> IO ()

foreign import ccall "wrapper"
  wrapCallbackEntry :: CallbackEntry -> IO (FunPtr CallbackEntry)

foreign import ccall unsafe "ffi_closure_alloc"
  ffi_closure_alloc :: CSize -> Ptr (Ptr ()) -> IO (Ptr ())

foreign import ccall unsafe "ffi_closure_free"
  ffi_closure_free :: Ptr () -> IO ()

foreign import ccall unsafe "ffi_prep_closure_loc"
  ffi_prep_closure_loc ::
    Ptr () -> Ptr CIF -> FunPtr CallbackEntry -> Ptr () -> Ptr () -> IO C_ffi_status

-- @sizeof(ffi_closure)@ is tens of bytes on every supported platform;
-- @ffi_closure_alloc@ accepts any size at least that large, so we over-
-- allocate a generous fixed amount rather than pull in @ffi.h@ for the exact
-- size (its path is not portable). The extra bytes are unused.
closureSize :: CSize
closureSize = 1024

-- The single, shared entry point. It recovers the per-closure context from the
-- user-data pointer, obtains a Unison stack, decodes the C arguments onto it,
-- runs the function, and writes the result (left on the stack) back.
--
-- Copy-on-concurrent-write: the callback keeps one reusable stack behind an
-- atomic in-use flag. A caller claims it with a test-and-set; whoever wins
-- reuses it (and allocates nothing), and anyone who finds it already in use —
-- a concurrent call on another thread, or a reentrant call on this one (e.g.
-- the Unison function calls back out to C which invokes this callback again) —
-- falls back to a fresh stack, so two live calls never share one. The flag is
-- released in a @finally@ so an exception can't leave it stuck.
callbackEntry :: CallbackEntry
callbackEntry _cif ret argv userData = do
  CallbackCtx spec run inUse reusable <- deRefStablePtr (castPtrToStablePtr userData)
  won <- not <$> atomicModifyIORef' inUse (\used -> (True, used))
  stk <- if won then pure reusable else alloc
  let invoke = do
        stk <- pushArgs (ffArgs spec) argv stk
        run stk \result -> writeResult (ffResult spec) ret result
  invoke `finally` when won (writeIORef inUse False)

sharedEntry :: FunPtr CallbackEntry
sharedEntry = unsafePerformIO (wrapCallbackEntry callbackEntry)
{-# NOINLINE sharedEntry #-}

-- | Build a C-callable function pointer from a Unison function. The 'FFSpec'
-- describes the C signature; @run@ applies the Unison function to the arguments
-- the callback decodes onto the stack. The result is a 'CDynFunc' (so it flows
-- through the same paths as a symbol loaded from a DLL) whose underlying
-- pointer calls back into Unison.
--
-- The returned callback holds the Unison function (and anything it closes
-- over) alive until 'freeCallback' is called; using the pointer afterwards is
-- a use-after-free, exactly as with a freed 'Unison.Runtime.Foreign.Dynamic'
-- pointer.
mkCallback :: FFSpec -> RunCallback -> IO CDynFunc
mkCallback spec0 run = do
  cspec <- prepareSpec spec0
  let spec = ffSpec cspec
  mapM_ checkArgType (ffArgs spec)
  checkResultType (ffResult spec)
  inUse <- newIORef False
  reusable <- alloc
  alloca \codeRef -> do
    writable <- ffi_closure_alloc closureSize codeRef
    code <- peek codeRef
    sp <- newStablePtr (CallbackCtx spec run inUse reusable)
    status <-
      withForeignPtr (cInterface cspec) \cif ->
        ffi_prep_closure_loc writable cif sharedEntry (castStablePtrToPtr sp) code
    unless (status == ffi_ok) do
      ffi_closure_free writable
      freeStablePtr sp
      throwIO BadClosureInit
    -- The teardown is carried on the function value itself (see
    -- 'Unison.Runtime.Foreign.Dynamic.cFinalize'), so 'freeCallback' needs no
    -- global or runtime-threaded registry. The flag makes a double free safe.
    freedRef <- newIORef False
    let finalize = do
          firstTime <- atomicModifyIORef' freedRef \freed -> (True, not freed)
          when firstTime do
            ffi_closure_free writable
            freeStablePtr sp
    pure $ CDynFunc "<callback>" cspec (castPtrToFunPtr code) finalize

-- | Release a callback created by 'mkCallback': frees the libffi closure and
-- drops the reference keeping the Unison function alive. Safe to call more
-- than once, and a no-op on a 'CDynFunc' that is not a callback (e.g. a DLL
-- symbol, whose finalizer is @pure ()@).
freeCallback :: CDynFunc -> IO ()
freeCallback = cFinalize

checkArgType :: FFType -> IO ()
checkArgType t
  | t `elem` [MBArr, Fun, Void] = throwIO (UnsupportedCallbackType t)
  | otherwise = pure ()

checkResultType :: FFType -> IO ()
checkResultType t
  | t `elem` [MBArr, Fun] = throwIO (UnsupportedCallbackType t)
  | otherwise = pure ()

-- Decode the C argument buffer directly onto the stack, pushing one value per
-- argument in parameter order (so the first argument ends up deepest).
pushArgs :: [FFType] -> Ptr (Ptr CValue) -> Stack -> IO Stack
pushArgs tys argv = go 0 tys
  where
    go _ [] stk = pure stk
    go i (ty : rest) stk = do
      p <- castPtr <$> peekElemOff argv i
      stk <- bump stk
      pushArg ty p stk
      go (i + 1) rest stk

pushArg :: FFType -> Ptr () -> Stack -> IO ()
pushArg ty p stk = case ty of
  I8 -> (peek (castPtr p) :: IO Int8) >>= pokeI stk . fromIntegral
  I16 -> (peek (castPtr p) :: IO Int16) >>= pokeI stk . fromIntegral
  I32 -> (peek (castPtr p) :: IO Int32) >>= pokeI stk . fromIntegral
  I64 -> (peek (castPtr p) :: IO Int64) >>= pokeI stk . fromIntegral
  U8 -> (peek (castPtr p) :: IO Word8) >>= pokeN stk . fromIntegral
  U16 -> (peek (castPtr p) :: IO Word16) >>= pokeN stk . fromIntegral
  U32 -> (peek (castPtr p) :: IO Word32) >>= pokeN stk . fromIntegral
  U64 -> (peek (castPtr p) :: IO Word64) >>= pokeN stk
  F32 -> (peek (castPtr p) :: IO Float) >>= pokeD stk . float2Double
  D64 -> (peek (castPtr p) :: IO Double) >>= pokeD stk
  Ptr -> (peek (castPtr p) :: IO (Ptr ())) >>= pokeBi stk
  MBArr -> throwIO (UnsupportedCallbackType ty)
  Fun -> throwIO (UnsupportedCallbackType ty)
  Void -> throwIO (UnsupportedCallbackType ty)

-- Write the result, left on top of the stack, into the C return slot. Integer
-- results are written at @ffi_arg@ width (8 bytes), as libffi requires for
-- integral returns narrower than a register; floating-point and pointer results
-- are written at their natural width.
writeResult :: FFType -> Ptr CValue -> Stack -> IO ()
writeResult ty ret stk = case ty of
  Void -> pure ()
  I8 -> writeInt
  I16 -> writeInt
  I32 -> writeInt
  I64 -> writeInt
  U8 -> writeNat
  U16 -> writeNat
  U32 -> writeNat
  U64 -> writeNat
  F32 -> peekOffD stk 0 >>= poke (castPtr ret) . double2Float
  D64 -> peekOffD stk 0 >>= poke (castPtr ret)
  Ptr -> (peekOffBi stk 0 :: IO (Ptr ())) >>= poke (castPtr ret)
  MBArr -> throwIO (UnsupportedCallbackType ty)
  Fun -> throwIO (UnsupportedCallbackType ty)
  where
    writeInt = peekOffI stk 0 >>= poke (castPtr ret) . (fromIntegral :: Int -> Int64)
    writeNat = (peekOffN stk 0 :: IO Word64) >>= poke (castPtr ret)
