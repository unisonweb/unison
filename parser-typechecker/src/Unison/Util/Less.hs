module Unison.Util.Less where

import System.Process
import System.IO (hPutStr, hClose)
import Control.Exception.Extra (ignore)
import Unison.Prelude (void)

less :: String -> IO ()
less str = do
  let args = ["--no-init"            -- don't clear the screen on exit
             ,"--raw-control-chars"  -- pass through colors and stuff
             ,"--quit-if-one-screen" -- self-explanatory
             ]
  (Just stdin, _stdout, _stderr, pid)
    <- createProcess (proc "less" args) { std_in = CreatePipe }

  -- print the thing to less!
  hPutStr stdin str

  -- If `less` has already exited, hClose throws an exception.
  ignore $ hClose stdin

  -- Wait for `less` to exit.
  void $ waitForProcess pid
