-- | The foreign interface to LLVM, through the C shim in @cbits/jit_llvm.c@.
-- LLVM isn't linked in: the shim loads it with @dlopen@ when the JIT is
-- turned on ('initLLVM'), so that ucm builds and runs without LLVM, and the
-- JIT is unavailable, with a message saying where it looked, on a machine
-- that has no LLVM 20 or newer.
--
-- The shim is ours, over LLVM's C API, because the @llvm-hs@ bindings lag
-- LLVM releases and we need six functions. Modules are handed over as IR
-- /text/, which LLVM parses in memory: text can be built by string
-- building, read by a person and pasted into LLVM's command-line tools,
-- where bitcode is a bit-level stream only LLVM's own libraries write, and
-- building IR through the C API would mean many more foreign calls for IR
-- that can't be inspected without asking LLVM to print it. If parsing ever
-- shows in the compile times, the generator can be retargeted to the C API
-- without touching the code generator's logic.
module Unison.Runtime.JIT.LLVM
  ( initLLVM,
    LoadFailure (..),
    addModule,
    lookupSymbol,
    defineSymbol,
    targetTriple,
  )
where

import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (FunPtr, Ptr, WordPtr (..), castPtrToFunPtr, nullPtr, wordPtrToPtr)
import Foreign.Storable (peek, poke)

foreign import ccall safe "unison_jit_init" c_init :: IO CInt

foreign import ccall unsafe "unison_jit_load_attempts" c_loadAttempts :: IO CString

foreign import ccall unsafe "unison_jit_llvm_path" c_llvmPath :: IO CString

foreign import ccall unsafe "unison_jit_llvm_version" c_llvmVersion :: IO CString

foreign import ccall safe "unison_jit_add_module" c_addModule :: CString -> CSize -> CString -> Ptr CString -> IO CInt

foreign import ccall unsafe "unison_jit_free_string" c_freeString :: CString -> IO ()

foreign import ccall safe "unison_jit_lookup" c_lookup :: CString -> IO Word64

foreign import ccall safe "unison_jit_define_symbol" c_defineSymbol :: CString -> Word64 -> IO CInt

foreign import ccall unsafe "unison_jit_last_error" c_lastError :: IO CString

foreign import ccall unsafe "unison_jit_triple" c_triple :: IO CString

lastError :: String -> IO String
lastError what = do
  e <- peekCString =<< c_lastError
  pure ("JIT: " ++ what ++ ": " ++ e)

-- | Why LLVM couldn't be started.
data LoadFailure
  = -- | no usable library: each location tried, with what was found there
    NotFound [String]
  | -- | a library was loaded but the JIT couldn't be set up
    InitError String

-- | Loads LLVM and starts the JIT. Safe to call more than once. On success,
-- the version loaded and the path it came from.
initLLVM :: IO (Either LoadFailure (String, FilePath))
initLLVM = do
  r <- c_init
  path <- peekCString =<< c_llvmPath
  if r == 0
    then do
      v <- peekCString =<< c_llvmVersion
      pure (Right (v, path))
    else
      if null path
        then Left . NotFound . lines <$> (peekCString =<< c_loadAttempts)
        else Left . InitError <$> lastError "initializing LLVM"

-- | Parses a module from IR text, runs the given pass pipeline
-- (such as @default<O2>@, or @""@ for none) and hands it to the JIT.
-- When asked, also returns the module's text after the passes.
addModule :: Bool -> String -> T.Text -> IO (Either String (Maybe String))
addModule wantOptimized passes ir =
  unsafeUseAsCStringLen (encodeUtf8 ir) $ \(p, n) -> withCString passes $ \ps -> alloca $ \out -> do
    poke out nullPtr
    r <- c_addModule p (fromIntegral n) ps (if wantOptimized then out else nullPtr)
    if r /= 0
      then Left <$> lastError "adding a module"
      else do
        cs <- peek out
        if cs == nullPtr
          then pure (Right Nothing)
          else do
            txt <- peekCString cs
            c_freeString cs
            pure (Right (Just txt))

-- | The address of a compiled function. Code is generated on first lookup.
lookupSymbol :: String -> IO (Either String (FunPtr a))
lookupSymbol name = do
  a <- withCString name c_lookup
  if a == 0
    then Left <$> lastError ("looking up " ++ name)
    else pure (Right (castPtrToFunPtr (wordPtrToPtr (WordPtr (fromIntegral a)))))

-- | Makes @name@ in generated code refer to @addr@.
defineSymbol :: String -> Word64 -> IO (Either String ())
defineSymbol name addr = do
  r <- withCString name (\n -> c_defineSymbol n addr)
  if r == 0 then pure (Right ()) else Left <$> lastError ("defining " ++ name)

targetTriple :: IO String
targetTriple = peekCString =<< c_triple
