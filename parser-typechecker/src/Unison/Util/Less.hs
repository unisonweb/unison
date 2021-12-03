{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
module Unison.Util.Less where

import System.Environment (lookupEnv)
import System.Process
import System.IO (hPutStr, hClose)
import Control.Exception.Extra (ignore)
import Unison.Prelude (void)

less :: String -> IO ()
less str = do
  inEmacs <- lookupEnv "INSIDE_EMACS"
  case inEmacs of
    Just _ -> putStr str
    Nothing -> do
      let args = ["--no-init"            -- don't clear the screen on exit
                 ,"--raw-control-chars"  -- pass through colors and stuff
                 ,"--prompt=[less] Use space/arrow keys to navigate, or 'q' to return to ucm:"
                 ,"--quit-if-one-screen" -- self-explanatory
                 ]
      (Just stdin, _stdout, _stderr, pid)
        <- createProcess (proc "less" args) { std_in = CreatePipe }

      -- If `less` exits before consuming all of stdin, `hPutStr` will crash.
      ignore $ hPutStr stdin str

      -- If `less` has already exited, hClose throws an exception.
      ignore $ hClose stdin

      -- Wait for `less` to exit.
      void $ waitForProcess pid
