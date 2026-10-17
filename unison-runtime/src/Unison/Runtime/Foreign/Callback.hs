{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Turning a Unison function into a C-callable function pointer, so that C
-- code can call back into Unison. This is the mirror image of the dynamic
-- call in "Unison.Runtime.Machine": there, Unison stack values are marshalled
-- into a libffi argument buffer and a C function is called; here a C caller
-- fills an argument buffer, a libffi /closure/ invokes a single generic
-- Haskell entry point, we decode the arguments into Unison values, run the
-- Unison function, and marshal its result back into the C return slot.
--
-- The Haskell @libffi@ package does not expose the closure API, so the three
-- @ffi_closure_*@ entry points are imported directly (they are linked via
-- that package already), as is done for @ffi_prep_cif_var@ in
-- "Unison.Runtime.Foreign.Dynamic".
--
-- Running the Unison function is supplied by the caller (a @[Val] -> (Val ->
-- IO ()) -> IO ()@ action, in practice a partially-applied @nestEvalArgs@) so
-- that this module does not depend on the evaluator.
module Unison.Runtime.Foreign.Callback
  ( mkCallback,
    freeCallback,
    CallbackException (..),
  )
where

import Control.Exception (Exception, throwIO)
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
  ( Val,
    maybeUnwrapBuiltin,
    wrapBuiltin,
    pattern BoxedVal,
    pattern DoubleVal,
    pattern Foreign,
    pattern IntVal,
    pattern NatVal,
  )

-- | A function run by the callback: given the decoded arguments and a sink for
-- the result value, apply the Unison function. Expected to invoke the sink
-- exactly once (synchronously).
type RunCallback = [Val] -> (Val -> IO ()) -> IO ()

-- | What a prepared closure needs at call time: the signature (for
-- marshalling) and the action that runs the Unison function.
data CallbackCtx = CallbackCtx !FFSpec !RunCallback

data CallbackException
  = -- | A callback argument/result type that is not supported.
    UnsupportedCallbackType FFType
  | -- | The Unison function returned a value of the wrong shape.
    BadCallbackResult FFType
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
-- user-data pointer, decodes the C arguments, runs the Unison function, and
-- writes the result back.
callbackEntry :: CallbackEntry
callbackEntry _cif ret argv userData = do
  CallbackCtx spec run <- deRefStablePtr (castPtrToStablePtr userData)
  args <- decodeArgs (ffArgs spec) argv
  resultRef <- newIORef Nothing
  run args (writeIORef resultRef . Just)
  readIORef resultRef >>= \case
    Just v -> encodeResult (ffResult spec) ret v
    -- Void results carry nothing to write back.
    Nothing -> pure ()

sharedEntry :: FunPtr CallbackEntry
sharedEntry = unsafePerformIO (wrapCallbackEntry callbackEntry)
{-# NOINLINE sharedEntry #-}

-- | Build a C-callable function pointer from a Unison function. The 'FFSpec'
-- describes the C signature; @run@ applies the Unison function to the decoded
-- arguments. The result is a 'CDynFunc' (so it flows through the same paths as
-- a symbol loaded from a DLL) whose underlying pointer calls back into Unison.
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
  alloca \codeRef -> do
    writable <- ffi_closure_alloc closureSize codeRef
    code <- peek codeRef
    sp <- newStablePtr (CallbackCtx spec run)
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

-- Decode the C argument buffer into Unison values, in order.
decodeArgs :: [FFType] -> Ptr (Ptr CValue) -> IO [Val]
decodeArgs tys argv = traverse decode (zip [0 ..] tys)
  where
    decode (i, ty) = do
      p <- peekElemOff argv i
      decodeArg ty (castPtr p)

decodeArg :: FFType -> Ptr () -> IO Val
decodeArg ty p = case ty of
  I8 -> IntVal . fromIntegral <$> (peek (castPtr p) :: IO Int8)
  I16 -> IntVal . fromIntegral <$> (peek (castPtr p) :: IO Int16)
  I32 -> IntVal . fromIntegral <$> (peek (castPtr p) :: IO Int32)
  I64 -> IntVal . fromIntegral <$> (peek (castPtr p) :: IO Int64)
  U8 -> NatVal . fromIntegral <$> (peek (castPtr p) :: IO Word8)
  U16 -> NatVal . fromIntegral <$> (peek (castPtr p) :: IO Word16)
  U32 -> NatVal . fromIntegral <$> (peek (castPtr p) :: IO Word32)
  U64 -> NatVal <$> (peek (castPtr p) :: IO Word64)
  F32 -> DoubleVal . float2Double <$> (peek (castPtr p) :: IO Float)
  D64 -> DoubleVal <$> (peek (castPtr p) :: IO Double)
  Ptr -> ptrVal <$> (peek (castPtr p) :: IO (Ptr ()))
  MBArr -> throwIO (UnsupportedCallbackType ty)
  Fun -> throwIO (UnsupportedCallbackType ty)
  Void -> throwIO (UnsupportedCallbackType ty)

-- Write the Unison result value into the C return slot. Integer results are
-- written at @ffi_arg@ width (8 bytes), as libffi requires for integral
-- returns narrower than a register; floating-point and pointer results are
-- written at their natural width.
encodeResult :: FFType -> Ptr CValue -> Val -> IO ()
encodeResult ty ret v = case ty of
  Void -> pure ()
  I8 -> pokeI
  I16 -> pokeI
  I32 -> pokeI
  I64 -> pokeI
  U8 -> pokeN
  U16 -> pokeN
  U32 -> pokeN
  U64 -> pokeN
  F32 -> case v of
    DoubleVal d -> poke (castPtr ret) (double2Float d)
    _ -> bad
  D64 -> case v of
    DoubleVal d -> poke (castPtr ret) d
    _ -> bad
  Ptr -> case asPtr v of
    Just p -> poke (castPtr ret) p
    Nothing -> bad
  MBArr -> bad
  Fun -> bad
  where
    bad = throwIO (BadCallbackResult ty)
    pokeI = case v of
      IntVal n -> poke (castPtr ret) (fromIntegral n :: Int64)
      _ -> bad
    pokeN = case v of
      NatVal n -> poke (castPtr ret) (n :: Word64)
      _ -> bad

-- Build a boxed pointer value, and recover one.
ptrVal :: Ptr () -> Val
ptrVal p = BoxedVal (Foreign (wrapBuiltin p))

asPtr :: Val -> Maybe (Ptr ())
asPtr (BoxedVal (Foreign f)) = maybeUnwrapBuiltin f
asPtr _ = Nothing
