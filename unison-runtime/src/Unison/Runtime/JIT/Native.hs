{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- | Calling into native code. See unison-runtime/src/Unison/Runtime/JIT/design.md, "How the interpreter
-- interacts with native code". With the @jit@ flag off, nothing here can
-- be reached, because no cell ever holds code.
module Unison.Runtime.JIT.Native
  ( NativeFn,
    Status,
    FrameRecord (..),
    statusOK,
    statusError,
    enterNative,
    outAp,
    outFp,
    outSp,
    outFrameCount,
    outFrames,
    configureNative,
    ctxLayout,
    rtsFacts,
    probeClosure,
    listInit,
    listCheck,
    listTest,
    textInit,
    textCheck,
    textTest,
    bytesInit,
    arrayInit,
    murmurInit,
    murmurTest,
    cmpInit,
    cmpTest,
    envInit,
    murmurCAddress,
    ucmpCAddress,
    tableAddresses,
    bytesCheck,
    bytesTest,
    closureInit,
    nameTest,
    hplimValue,
  )
where

import Data.Primitive.Array (MutableArray (..), sizeofMutableArray)
import Data.Primitive.ByteArray (MutableByteArray (..), readByteArray)
import Foreign.Ptr (FunPtr)
import GHC.Exts (Any, RealWorld)
import Unison.Runtime.Stack (Closure)

import Data.Int (Int64)
import Control.Monad (forM)
import Data.Word (Word64)
import Foreign.C.String (CString, peekCString)
import Foreign.Marshal.Alloc (allocaBytes, free)
import Foreign.Ptr (Ptr, WordPtr (..), ptrToWordPtr, wordPtrToPtr)
import Foreign.Storable (peek, peekElemOff, pokeElemOff)
import GHC.Exts (MutableArray#, MutableByteArray#)

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
outAp, outFp, outSp, outFrameCount :: MutableByteArray RealWorld -> Int -> IO Int
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
outFrames :: MutableByteArray RealWorld -> Int -> IO [FrameRecord]
outFrames = frameRecords

foreign import ccall unsafe "unison_jit_enter"
  c_enter ::
    FunPtr NativeFn ->
    MutableByteArray# RealWorld ->
    MutableArray# RealWorld Closure ->
    MutableArray# RealWorld Closure ->
    Int64 ->
    Int64 ->
    Int64 ->
    Int64 ->
    IO Int64

foreign import ccall unsafe "unison_jit_configure" c_configure :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO ()

foreign import ccall unsafe "unison_jit_rts_facts" c_rtsFacts :: Ptr Int64 -> Int64 -> IO Int64

-- | Facts about the runtime that generated code needs: see
-- unison_jit_rts_facts in jit_rt.c for what each entry is.
rtsFacts :: IO [Int]
rtsFacts = allocaBytes (8 * 16) $ \out -> do
  n <- c_rtsFacts out 16
  map fromIntegral <$> mapM (peekElemOff out) [0 .. fromIntegral n - 1]

foreign import ccall unsafe "unison_jit_table_addresses" c_tableAddresses :: Ptr CString -> Ptr (Ptr ()) -> Int64 -> IO Int64

-- | The names the IR helpers refer to the runtime's tables by, with the
-- tables' addresses, for the JIT to define before the helpers are added.
tableAddresses :: IO [(String, Word64)]
tableAddresses = allocaBytes (8 * 16) $ \names -> allocaBytes (8 * 16) $ \addrs -> do
  n <- c_tableAddresses names addrs 16
  forM [0 .. fromIntegral n - 1] $ \i -> do
    name <- peekElemOff names i >>= peekCString
    addr <- peekElemOff addrs i
    pure (name, fromIntegral (ptrToWordPtr addr))

foreign import ccall unsafe "unison_jit_ctx_layout" c_ctxLayout :: Ptr Int64 -> Int64 -> IO Int64

foreign import ccall unsafe "unison_jit_hplim_value" c_hplimValue :: IO Int64


-- | For tracing: what the poll would see right now.
hplimValue :: IO Int
hplimValue = fromIntegral <$> c_hplimValue

foreign import ccall unsafe "unison_jit_probe" c_probe :: MutableArray# RealWorld Any -> Int64 -> Ptr Int64 -> IO Int64

-- | Runs native code with the given stacks (and their size in slots) and
-- pointers, and returns its status. Where it stopped and the frame records
-- it wrote are read with 'outAp' and the rest. The handler environment (an
-- @HEnv@, coerced) goes in and comes back in the boxed stack's out-area
-- (see 'Unison.Runtime.Stack.nativeOutPtrs'), so that the collector keeps
-- the pointer right while Haskell code runs around the call.
enterNative ::
  FunPtr NativeFn ->
  MutableByteArray RealWorld ->
  MutableArray RealWorld Closure ->
  MutableArray RealWorld Closure ->
  Int ->
  Int ->
  Int ->
  Int ->
  IO Status
enterNative fn (MutableByteArray ustk) (MutableArray bstk) (MutableArray pool) size ap fp sp = do
  status <- c_enter fn ustk bstk pool (fromIntegral size) (fromIntegral ap) (fromIntegral fp) (fromIntegral sp)
  pure $! fromIntegral status
{-# INLINE enterNative #-}

frameRecords :: MutableByteArray RealWorld -> Int -> IO [FrameRecord]
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

-- | Passes stress settings to the C side: poll every N entries, treat every
-- Nth callee as uncompiled, C stack budget in bytes (0 for the default),
-- trace. Called once at startup.
configureNative :: Int -> Int -> Int -> Int -> Bool -> IO ()
configureNative pollEvery calleeEvery cstack alloc tr =
  c_configure (fromIntegral pollEvery) (fromIntegral calleeEvery) (fromIntegral cstack) (fromIntegral alloc) (if tr then 1 else 0)

-- | The offsets of the fields of the C @Ctx@, in the order they are
-- declared, and its total size. The code generator uses these.
ctxLayout :: IO ([Int], Int)
ctxLayout = allocaBytes (8 * 32) $ \out -> do
  size <- c_ctxLayout out 32
  offs <- mapM (peekElemOff out) [0 .. 22]
  pure (map fromIntegral offs, fromIntegral size)

-- | Inspects a closure. Element 0 of the array is the sample, the rest are
-- the objects in its pointer fields. Returns the raw probe output: info
-- pointer, pointer tag, closure type, pointer count, non-pointer count,
-- constructor tag, the payload words, then a match index per pointer field.
probeClosure :: MutableArray RealWorld Any -> IO [Int]
probeClosure arr@(MutableArray arr#) = allocaBytes (8 * 64) $ \out -> do
  total <- c_probe arr# (fromIntegral (sizeofMutableArray arr)) out
  vals <- mapM (peekElemOff out) [0 .. 5 + 2 * fromIntegral total]
  pure (map fromIntegral vals)

foreign import ccall unsafe "unison_jit_list_init" c_listInit :: MutableArray# RealWorld Any -> Ptr Int64 -> IO Int64

foreign import ccall unsafe "unison_jit_list_check" c_listCheck :: MutableArray# RealWorld Any -> IO Int64

foreign import ccall unsafe "unison_jit_list_test" c_listTest :: MutableArray# RealWorld Any -> Int64 -> Int64 -> Int64 -> IO Int64

-- | Teaches the C list helpers the constructors of a list from samples
-- (see unison_jit_list_init), given the info pointers of Foreign, Val,
-- Data1 and Data2. 1 if all is well, else the number of the check that failed.
listInit :: MutableArray RealWorld Any -> [Int] -> IO Int
listInit (MutableArray arr#) infos = allocaBytes (8 * length infos) $ \p -> do
  mapM_ (\(i, v) -> pokeElemOff p i (fromIntegral v)) (zip [0 ..] infos)
  fromIntegral <$> c_listInit arr# p

-- | Checks the structure of the list in slot 0: -1 if it isn't what the C
-- helpers assume, else the set of constructors met.
listCheck :: MutableArray RealWorld Any -> IO Int
listCheck (MutableArray arr#) = fromIntegral <$> c_listCheck arr#

-- | Runs a C list helper on slot 0 (see unison_jit_list_test); the result
-- lands in slot 3. False if the helper left the case to the interpreter.
listTest :: MutableArray RealWorld Any -> Int -> Int -> Int -> IO Bool
listTest (MutableArray arr#) op a b = (== 1) <$> c_listTest arr# (fromIntegral op) (fromIntegral a) (fromIntegral b)

foreign import ccall unsafe "unison_jit_text_init" c_textInit :: MutableArray# RealWorld Any -> Ptr Int64 -> IO Int64

foreign import ccall unsafe "unison_jit_text_check" c_textCheck :: MutableArray# RealWorld Any -> IO Int64

foreign import ccall unsafe "unison_jit_text_test" c_textTest :: MutableArray# RealWorld Any -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64

-- | The same three for the text helpers (see unison_jit_text_init and
-- unison_jit_text_test). 'textTest' returns what the C function does.
textInit :: MutableArray RealWorld Any -> [Int] -> IO Int
textInit (MutableArray arr#) infos = allocaBytes (8 * length infos) $ \p -> do
  mapM_ (\(i, v) -> pokeElemOff p i (fromIntegral v)) (zip [0 ..] infos)
  fromIntegral <$> c_textInit arr# p

textCheck :: MutableArray RealWorld Any -> IO Bool
textCheck (MutableArray arr#) = (== 1) <$> c_textCheck arr#

textTest :: MutableArray RealWorld Any -> Int -> Int -> Int -> Int -> IO Int
textTest (MutableArray arr#) op a b c = fromIntegral <$> c_textTest arr# (fromIntegral op) (fromIntegral a) (fromIntegral b) (fromIntegral c)

foreign import ccall unsafe "unison_jit_bytes_init" c_bytesInit :: MutableArray# RealWorld Any -> Ptr Int64 -> IO Int64

foreign import ccall unsafe "unison_jit_array_init" c_arrayInit :: MutableArray# RealWorld Any -> Ptr Int64 -> IO Int64

foreign import ccall unsafe "unison_jit_murmur_init" c_murmurInit :: MutableArray# RealWorld Any -> Ptr Int64 -> IO Int64

-- the C side returns a pair (ok, hash) by value; the FFI can't, so this
-- wrapper writes it through a pointer
foreign import ccall unsafe "unison_jit_murmur_test_ffi" c_murmurTest :: MutableArray# RealWorld Any -> Int64 -> Int64 -> Ptr Int64 -> IO Int64

foreign import ccall unsafe "unison_jit_env_init" c_envInit :: MutableArray# RealWorld Any -> IO Int64

-- | Teaches the request helpers the handler environment's layout from
-- samples and checks the lookup on them (see unison_jit_env_init). 1 if
-- all is well, else the number of the check that failed.
envInit :: MutableArray RealWorld Any -> IO Int
envInit (MutableArray arr#) = fromIntegral <$> c_envInit arr#

foreign import ccall "&unison_jit_murmur" c_murmurAddr :: FunPtr (Int64 -> Ptr () -> IO ())

foreign import ccall "&unison_jit_ucmp" c_ucmpAddr :: FunPtr (Int64 -> Ptr () -> Int64 -> Ptr () -> Int64 -> IO ())

foreign import ccall unsafe "unison_jit_cmp_init" c_cmpInit :: Ptr Int64 -> IO Int64

foreign import ccall unsafe "unison_jit_cmp_test_ffi" c_cmpTest :: MutableArray# RealWorld Any -> Int64 -> Int64 -> Int64 -> Ptr Int64 -> IO Int64

-- | Where the C copy of @unison_jit_ucmp@ is, for the same check.
ucmpCAddress :: FunPtr (Int64 -> Ptr () -> Int64 -> Ptr () -> Int64 -> IO ())
ucmpCAddress = c_ucmpAddr

-- | Hands the comparison helpers the PAp info pointer, the packed tags of
-- boxed Int and Nat, and the type part of Any's packed tag.
cmpInit :: [Int] -> IO Int
cmpInit infos = allocaBytes (8 * length infos) $ \p -> do
  mapM_ (\(i, v) -> pokeElemOff p i (fromIntegral v)) (zip [0 ..] infos)
  fromIntegral <$> c_cmpInit p

-- | The native comparison (or equality, with the flag) of the values in
-- elements 0 and 1, whose unboxed halves are the two ints and whose boxed
-- halves are the elements; Nothing when the helper leaves the pair to the
-- interpreter.
cmpTest :: MutableArray RealWorld Any -> Int -> Int -> Bool -> IO (Maybe Int)
cmpTest (MutableArray arr#) u1 u2 eq = allocaBytes 8 $ \p -> do
  ok <- c_cmpTest arr# (fromIntegral u1) (fromIntegral u2) (if eq then 4 else 0) p
  if ok == 1 then Just . fromIntegral <$> peek p else pure Nothing

-- | Where the C copy of @unison_jit_murmur@ is: what the JIT's own
-- definition of the name (the IR copy) must not be.
murmurCAddress :: FunPtr (Int64 -> Ptr () -> IO ())
murmurCAddress = c_murmurAddr

-- | Hands the hash helper the type-tag closure in element 0 and the Enum
-- and DataG info pointers.
murmurInit :: MutableArray RealWorld Any -> [Int] -> IO Int
murmurInit (MutableArray arr#) infos = allocaBytes (8 * length infos) $ \p -> do
  mapM_ (\(i, v) -> pokeElemOff p i (fromIntegral v)) (zip [0 ..] infos)
  fromIntegral <$> c_murmurInit arr# p

-- | The native hash of element 0 (boxed), or of the unboxed value with the
-- type tag in element 0; Nothing when the helper leaves it to the interpreter.
murmurTest :: MutableArray RealWorld Any -> Int -> Bool -> IO (Maybe Word64)
murmurTest (MutableArray arr#) u unboxed = allocaBytes 8 $ \p -> do
  ok <- c_murmurTest arr# (fromIntegral u) (if unboxed then 1 else 0) p
  if ok == 1 then Just . fromIntegral <$> peek p else pure Nothing

-- | Hands the array and ref helpers the wrappers' info pointers and tags
-- (pairs, flattened) and the empty value in element 0.
arrayInit :: MutableArray RealWorld Any -> [Int] -> IO Int
arrayInit (MutableArray arr#) infos = allocaBytes (8 * length infos) $ \p -> do
  mapM_ (\(i, v) -> pokeElemOff p i (fromIntegral v)) (zip [0 ..] infos)
  fromIntegral <$> c_arrayInit arr# p

foreign import ccall unsafe "unison_jit_bytes_check" c_bytesCheck :: MutableArray# RealWorld Any -> IO Int64

foreign import ccall unsafe "unison_jit_bytes_test" c_bytesTest :: MutableArray# RealWorld Any -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64

-- | And for the bytes helpers (see unison_jit_bytes_init and
-- unison_jit_bytes_test).
bytesInit :: MutableArray RealWorld Any -> [Int] -> IO Int
bytesInit (MutableArray arr#) infos = allocaBytes (8 * length infos) $ \p -> do
  mapM_ (\(i, v) -> pokeElemOff p i (fromIntegral v)) (zip [0 ..] infos)
  fromIntegral <$> c_bytesInit arr# p

bytesCheck :: MutableArray RealWorld Any -> IO Bool
bytesCheck (MutableArray arr#) = (== 1) <$> c_bytesCheck arr#

bytesTest :: MutableArray RealWorld Any -> Int -> Int -> Int -> Int -> IO Int
bytesTest (MutableArray arr#) op a b c = fromIntegral <$> c_bytesTest arr# (fromIntegral op) (fromIntegral a) (fromIntegral b) (fromIntegral c)

foreign import ccall unsafe "unison_jit_closure_init" c_closureInit :: Ptr Int64 -> IO ()

foreign import ccall unsafe "unison_jit_name_test" c_nameTest :: MutableArray# RealWorld Any -> Int64 -> IO Int64

-- | Gives the partial-application helper the info pointers of PAp and of
-- the boxes around a segment's two arrays.
closureInit :: [Int] -> IO ()
closureInit infos = allocaBytes (8 * length infos) $ \p -> do
  mapM_ (\(i, v) -> pokeElemOff p i (fromIntegral v)) (zip [0 ..] infos)
  c_closureInit p

-- | Runs the helper on slot 0 with n arguments (see unison_jit_name_test).
nameTest :: MutableArray RealWorld Any -> Int -> IO Bool
nameTest (MutableArray arr#) n = (== 1) <$> c_nameTest arr# (fromIntegral n)

