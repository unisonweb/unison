
-- Common interface wrapping Win32 DLL loading functions.
module Unison.Runtime.FFI.DLL where

import Foreign.Ptr
import System.Win32.DLL

data DLL = DLL !FilePath !HMODULE

getDLLPath :: DLL -> FilePath
getDLLPath (DLL path _) = path

openDLL :: FilePath -> IO DLL
openDLL path = DLL path <$> loadLibrary path

getDLLSym :: DLL -> String -> IO (FunPtr a)
getDLLSym (DLL module) symbol = do
  ptr <- getProcAddress module symbol
  pure $ castPtrToFunPtr ptr

closeDLL :: DLL -> IO ()
closeDLL (DLL module) = freeLibrary module


