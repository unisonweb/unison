
-- Common interface wrapping Posix DLL loading functions.
module Unison.Runtime.FFI.DLL where

import Foreign.Ptr
import System.Posix.DynamicLinker qualified as Posix

data DLL = DLL !FilePath !Posix.DL

getDLLPath :: DLL -> FilePath
getDLLPath (DLL path _) = path

openDLL :: FilePath -> IO DLL
openDLL path = DLL path <$> Posix.dlopen path [Posix.RTLD_LAZY]

getDLLSym :: DLL -> String -> IO (FunPtr a)
getDLLSym (DLL _ dll) symbol = Posix.dlsym dll symbol

closeDLL :: DLL -> IO ()
closeDLL (DLL _ dll) = Posix.dlclose dll


