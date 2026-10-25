{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- | The interpreter's side of the boundary with native code: the C
-- functions in cbits/jit_rt.c that the Haskell runtime calls, and nothing
-- else. See unison-runtime/src/Unison/Runtime/JIT/design.md, "How the interpreter interacts
-- with native code". With the JIT off nothing here is reached, because no
-- cell ever holds code.
--
-- Three jobs, in three sections below:
--
-- 1. /Entering native code./ 'enterNative' is the one call the trampoline
--    (Machine.runNative) makes per entry; it hands over the stacks and
--    the pool and gets a status back. Everything else native code returns
--    is read afterwards out of the stacks' out-areas with 'outAp' and the
--    rest, as 'FrameRecord's for the frames of native callers.
--
-- 2. /Startup./ 'configureNative' passes the stress settings down,
--    'ctxLayout' and 'rtsFacts' fetch the numbers the code generator bakes
--    into every function (the offsets of the C @Ctx@ struct, the RTS's
--    info pointers and layout constants).
--
-- 3. /Addresses./ The names generated code and the IR helpers call, with
--    their addresses, which the JIT defines as absolute symbols before any
--    module is added ('helperAddresses', 'tableAddresses'), and the C
--    copies of the two helpers whose IR definition must shadow them.
--
-- The startup checks that run each C helper family against the
-- interpreter are in "Unison.Runtime.JIT.Native.Probes".
module Unison.Runtime.JIT.Native
  ( -- * Entering native code
    NativeFn,
    Status,
    statusOK,
    statusError,
    enterNative,
    FrameRecord (..),
    outAp,
    outFp,
    outSp,
    outFrameCount,
    outFrames,

    -- * Startup
    configureNative,
    ctxLayout,
    rtsFacts,
    hplimValue,

    -- * Addresses
    helperAddresses,
    tableAddresses,
    murmurCAddress,
    ucmpCAddress,
  )
where

import Control.Monad (forM)
import Control.Monad.Primitive (PrimState)
import Data.Int (Int64)
import Data.Primitive.Array (MutableArray (..))
import Data.Primitive.ByteArray (MutableByteArray (..), readByteArray)
import Data.Word (Word64)
import Foreign.C.String (CString, peekCString)
import Foreign.Marshal.Alloc (allocaBytes, free)
import Foreign.Ptr (FunPtr, Ptr, WordPtr (..), ptrToWordPtr, wordPtrToPtr)
import Foreign.Storable (peekElemOff)
import GHC.Exts (MutableArray#, MutableByteArray#)
import Unison.Runtime.Stack (Closure)

-- ---------------------------------------------------------------------------
-- Entering native code

-- | The uniform signature of compiled code: @(ctx, ap, fp, sp) -> status@.
data NativeFn

type Status = Int

-- | What a native caller writes down when its callee exits: which @Let@
-- body to continue with (an index into the frame table), and the two sizes
-- a @Push@ frame needs. A negative index, @-1 - i@, is a @Local@ frame
-- instead: the environment to switch back to is in the boxed out-area at
-- @i@ past the first pointer, and the pending-argument count is as for a
-- @Push@.
data FrameRecord = FrameRecord {frIndex :: !Int, frFrameSize :: !Int, frPendingArgs :: !Int}
  deriving (Show)

statusOK, statusError :: Status
statusOK = 0
statusError = -1

-- 'enterNative' leaves what native code hands back besides its status in
-- the spare words past the last slot of the unboxed stack (see
-- 'Unison.Runtime.Stack.nativeOutWords'): nothing is allocated for a round
-- trip. These read them, given the stack and its size in slots.
outAp, outFp, outSp, outFrameCount :: MutableByteArray (PrimState IO) -> Int -> IO Int
outAp ustk size = readByteArray ustk size
outFp ustk size = readByteArray ustk (size + 1)
outSp ustk size = readByteArray ustk (size + 2)
outFrameCount ustk size = readByteArray ustk (size + 3)
{-# INLINE outAp #-}
{-# INLINE outFp #-}
{-# INLINE outSp #-}
{-# INLINE outFrameCount #-}

-- | The frame records, in the order they were written (innermost caller
-- first). Call at most once per entry: a long list is freed as it is read.
outFrames :: MutableByteArray (PrimState IO) -> Int -> IO [FrameRecord]
outFrames = frameRecords

foreign import ccall unsafe "unison_jit_enter"
  c_enter ::
    FunPtr NativeFn ->
    MutableByteArray# (PrimState IO) ->
    MutableArray# (PrimState IO) Closure ->
    MutableArray# (PrimState IO) Closure ->
    Int64 ->
    Int64 ->
    Int64 ->
    Int64 ->
    IO Int64

-- | Runs native code with the given stacks (and their size in slots) and
-- pointers, and returns its status. Where it stopped and the frame records
-- it wrote are read with 'outAp' and the rest. The handler environment (an
-- @HEnv@, coerced) goes in and comes back in the boxed stack's out-area
-- (see 'Unison.Runtime.Stack.nativeOutPtrs'), so that the collector keeps
-- the pointer right while Haskell code runs around the call.
enterNative ::
  FunPtr NativeFn ->
  MutableByteArray (PrimState IO) ->
  MutableArray (PrimState IO) Closure ->
  MutableArray (PrimState IO) Closure ->
  Int ->
  Int ->
  Int ->
  Int ->
  IO Status
enterNative fn (MutableByteArray ustk) (MutableArray bstk) (MutableArray pool) size ap fp sp = do
  status <- c_enter fn ustk bstk pool (fromIntegral size) (fromIntegral ap) (fromIntegral fp) (fromIntegral sp)
  pure $! fromIntegral status
{-# INLINE enterNative #-}

frameRecords :: MutableByteArray (PrimState IO) -> Int -> IO [FrameRecord]
frameRecords out size = do
  n <- readByteArray out (size + 3) :: IO Int
  if n <= inlineFrames
    then mapM (\i -> FrameRecord <$> readByteArray out (size + 4 + 3 * i) <*> readByteArray out (size + 5 + 3 * i) <*> readByteArray out (size + 6 + 3 * i)) [0 .. n - 1]
    else do
      addr <- readByteArray out (size + 4) :: IO Word
      let buf = wordPtrToPtr (WordPtr addr) :: Ptr Int64
          record i =
            FrameRecord
              <$> (fromIntegral <$> peekElemOff buf (3 * i))
              <*> (fromIntegral <$> peekElemOff buf (3 * i + 1))
              <*> (fromIntegral <$> peekElemOff buf (3 * i + 2))
      fs <- mapM record [0 .. n - 1]
      free buf
      pure fs

-- | Must match UNISON_JIT_INLINE_FRAMES in jit_rt.h.
inlineFrames :: Int
inlineFrames = 8

-- ---------------------------------------------------------------------------
-- Startup

foreign import ccall unsafe "unison_jit_configure" c_configure :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO ()

-- | Passes stress settings to the C side: poll every N entries, treat every
-- Nth callee as uncompiled, C stack budget in bytes (0 for the default),
-- allocation budget in words, trace. Called once at startup.
configureNative :: Int -> Int -> Int -> Int -> Bool -> IO ()
configureNative pollEvery calleeEvery cstack alloc tr =
  c_configure (fromIntegral pollEvery) (fromIntegral calleeEvery) (fromIntegral cstack) (fromIntegral alloc) (if tr then 1 else 0)

foreign import ccall unsafe "unison_jit_ctx_layout" c_ctxLayout :: Ptr Int64 -> Int64 -> IO Int64

-- | The offsets of the fields of the C @Ctx@, in the order they are
-- declared, and its total size. The code generator uses these.
ctxLayout :: IO ([Int], Int)
ctxLayout = allocaBytes (8 * 32) $ \out -> do
  size <- c_ctxLayout out 32
  offs <- mapM (peekElemOff out) [0 .. 22]
  pure (map fromIntegral offs, fromIntegral size)

foreign import ccall unsafe "unison_jit_rts_facts" c_rtsFacts :: Ptr Int64 -> Int64 -> IO Int64

-- | Facts about the runtime that generated code needs: see
-- unison_jit_rts_facts in jit_rt.c for what each entry is.
rtsFacts :: IO [Int]
rtsFacts = allocaBytes (8 * 16) $ \out -> do
  n <- c_rtsFacts out 16
  map fromIntegral <$> mapM (peekElemOff out) [0 .. fromIntegral n - 1]

foreign import ccall unsafe "unison_jit_hplim_value" c_hplimValue :: IO Int64

-- | For tracing: what the poll would see right now.
hplimValue :: IO Int
hplimValue = fromIntegral <$> c_hplimValue

-- ---------------------------------------------------------------------------
-- Addresses

foreign import ccall unsafe "unison_jit_helper_addresses" c_helperAddresses :: Ptr CString -> Ptr (Ptr ()) -> Int64 -> IO Int64

-- | Every C function generated code and the IR modules call by name, with
-- its address, for the JIT to define before any module is added (so that
-- nothing is looked up in the process, which Linux and Windows don't allow
-- without more linker flags).
helperAddresses :: IO [(String, Word64)]
helperAddresses = namedAddresses 256 c_helperAddresses

foreign import ccall unsafe "unison_jit_table_addresses" c_tableAddresses :: Ptr CString -> Ptr (Ptr ()) -> Int64 -> IO Int64

-- | The names the IR helpers refer to the runtime's tables by, with the
-- tables' addresses, for the JIT to define before the helpers are added.
tableAddresses :: IO [(String, Word64)]
tableAddresses = namedAddresses 16 c_tableAddresses

-- | Reads a C table of (name, address) pairs of at most @most@ entries.
namedAddresses :: Int -> (Ptr CString -> Ptr (Ptr ()) -> Int64 -> IO Int64) -> IO [(String, Word64)]
namedAddresses most fill = allocaBytes (8 * most) $ \names -> allocaBytes (8 * most) $ \addrs -> do
  n <- fill names addrs (fromIntegral most)
  forM [0 .. fromIntegral n - 1] $ \i -> do
    name <- peekElemOff names i >>= peekCString
    addr <- peekElemOff addrs i
    pure (name, fromIntegral (ptrToWordPtr addr))

foreign import ccall "&unison_jit_murmur" c_murmurAddr :: FunPtr (Int64 -> Ptr () -> IO ())

foreign import ccall "&unison_jit_ucmp" c_ucmpAddr :: FunPtr (Int64 -> Ptr () -> Int64 -> Ptr () -> Int64 -> IO ())

-- | Where the C copy of @unison_jit_murmur@ is: what the JIT's own
-- definition of the name (the IR copy) must not be.
murmurCAddress :: FunPtr (Int64 -> Ptr () -> IO ())
murmurCAddress = c_murmurAddr

-- | Where the C copy of @unison_jit_ucmp@ is, for the same check.
ucmpCAddress :: FunPtr (Int64 -> Ptr () -> Int64 -> Ptr () -> Int64 -> IO ())
ucmpCAddress = c_ucmpAddr
