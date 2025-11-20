
-- Common interface wrapping Win32 DLL loading functions.
module Unison.Runtime.FFI.DLL where

import Foreign.Ptr
import System.Win32.DLL
import System.Win32.Types

data DLL = DLL !FilePath !HMODULE

getDLLPath :: DLL -> FilePath
getDLLPath (DLL path _) = path

openDLL :: FilePath -> IO DLL
openDLL path = DLL path <$> loadLibrary path

getDLLSym :: DLL -> String -> IO (FunPtr a)
getDLLSym (DLL _ mod) symbol = do
  ptr <- getProcAddress mod symbol
  pure $ castPtrToFunPtr ptr

